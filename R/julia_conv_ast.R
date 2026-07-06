# ==============================================================================
# AST-based R -> Julia equation translation
# ==============================================================================
# Translates an R equation to Julia by walking R's parse tree and emitting Julia
# directly, before convert_equations_julia() falls back to the regex/position
# string-surgery path. It is O(nodes), needs no per-equation regex sweeps
# or data-frame position tables, and reuses the existing function-mapping
# semantics (syntax_julia$syntax_df, sort_args(), conv_distribution/seq/sample).
#
# It handles expressions, operators, function calls, and control flow. It returns
# NULL for anything it does not handle, so convert_equations_julia() can use the
# legacy string converter for the remaining cases.
#
# NOTE: the lower-level regex helpers (get_range_*, parse_args,
# select_innermost_function, ...) are shared with the InsightMaker -> R
# converter and are intentionally left untouched.


#' Translate one R equation to Julia via its parse tree
#'
#' @param eqn Length-1 character R equation.
#' @param var_names Character vector of model variable names.
#' @returns Julia string, or `NULL` if the equation contains a construct the AST
#'   path does not handle (caller should fall back to convert_equations_julia()).
#' @noRd
convert_eqn_ast_julia <- function(eqn, var_names) {
  if (is.null(eqn) || length(eqn) != 1L || !nzchar(eqn)) {
    return(NULL)
  }
  parsed <- tryCatch(parse(text = eqn, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || length(parsed) != 1L) {
    return(NULL)
  }
  # Catch ANY error (not just the deliberate bail), so the AST path can never do
  # worse than the legacy translator: on any hiccup we return NULL and the caller
  # falls back, where a genuinely invalid equation surfaces its canonical error.
  tryCatch(emit_julia_node(parsed[[1]], var_names),
    error = function(e) NULL
  )
}


#' Signal that the AST path cannot handle a node (triggers fallback)
#' @noRd
.ast_bail <- function() {
  rlang::abort("unsupported AST node", class = "stockflow_ast_unsupported")
}


#' Format an R numeric/logical literal as a Julia literal
#'
#' @param as_int If TRUE, whole numbers are emitted as Julia integers (for array
#'   indices and ranges); otherwise as Floats (Julia infers Int otherwise, which
#'   can trigger InexactError in numeric code).
#' @noRd
num_to_julia <- function(x, as_int = FALSE) {
  if (is.logical(x)) {
    if (is.na(x)) {
      return("missing")
    }
    return(if (isTRUE(x)) "true" else "false")
  }
  if (is.nan(x)) {
    return("NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  if (is.na(x)) {
    return("missing")
  }
  s <- format(x, scientific = FALSE, trim = TRUE)
  if (as_int && x == round(x)) {
    return(format(as.integer(round(x)), scientific = FALSE, trim = TRUE))
  }
  # Ensure a Float literal so Julia never infers Int (avoids InexactError).
  if (!grepl("[.eE]", s)) {
    s <- paste0(s, ".0")
  }
  s
}


#' Emit a node as a Julia array index/range bound (integers stay integers)
#' @noRd
emit_julia_index <- function(node, var_names) {
  if (rlang::is_syntactic_literal(node) && is.logical(node)) {
    .ast_bail()
  }
  if (rlang::is_syntactic_literal(node) && is.numeric(node)) {
    if (node != round(node)) {
      .ast_bail()
    }
    return(num_to_julia(node, as_int = TRUE))
  }
  if (is.call(node) && identical(rlang::call_name(node), "c")) {
    items <- vapply(as.list(node)[-1L], emit_julia_index, character(1), var_names = var_names)
    return(paste0("[", paste(items, collapse = ", "), "]"))
  }
  emit_julia_node(node, var_names)
}


#' Emit the statements of a block body (no begin/end wrapper)
#' @noRd
emit_julia_body <- function(node, var_names) {
  if (is.call(node) && identical(as.character(node[[1L]]), "{")) {
    stmts <- as.list(node)[-1L]
    paste(
      vapply(stmts, emit_julia_node, character(1), var_names = var_names),
      collapse = "\n"
    )
  } else {
    emit_julia_node(node, var_names)
  }
}


#' Emit an if/else-if/else chain
#' @noRd
emit_julia_if <- function(node, var_names, keyword = "if", close = TRUE) {
  args <- as.list(node)[-1L]
  out <- paste0(
    keyword, " ", emit_julia_node(args[[1L]], var_names), "\n",
    emit_julia_body(args[[2L]], var_names)
  )

  if (length(args) == 3L) {
    else_node <- args[[3L]]
    if (is.call(else_node) && identical(rlang::call_name(else_node), "if")) {
      out <- paste0(out, "\n", emit_julia_if(else_node, var_names,
        keyword = "elseif", close = FALSE
      ))
    } else {
      out <- paste0(out, "\nelse\n", emit_julia_body(else_node, var_names))
    }
  }

  if (close) {
    out <- paste0(out, "\nend")
  }
  out
}


#' Emit a function definition (named if `name` supplied, else anonymous)
#' @noRd
emit_julia_function <- function(fn_node, name, var_names) {
  fmls <- fn_node[[2L]]
  body <- fn_node[[3L]]
  nms <- names(fmls)

  if (any(nms == "...")) {
    .ast_bail()
  }

  # A formal with no default is R's empty/"missing argument" symbol. Assigning it
  # to a local makes missing() report TRUE, which we can test before using it
  # (using it directly would raise "argument is missing").
  parts <- character(length(fmls))
  has_default <- logical(length(fmls))
  for (i in seq_along(fmls)) {
    default <- fmls[[i]]
    if (missing(default)) {
      parts[i] <- nms[i]
    } else {
      has_default[i] <- TRUE
      parts[i] <- paste0(nms[i], " = ", emit_julia_node(default, var_names))
    }
  }
  if (any(has_default) && any(!has_default[seq.int(min(which(has_default)), length(has_default))])) {
    .ast_bail()
  }

  args <- paste(parts, collapse = ", ")
  header <- if (!is.null(name) && nzchar(name)) {
    paste0("function ", name, "(", args, ")")
  } else {
    paste0("function (", args, ")")
  }
  paste0(header, "\n", emit_julia_body(body, var_names), "\nend")
}


#' Recursively emit Julia for a parsed R node
#' @noRd
emit_julia_node <- function(node, var_names) {
  if (is.null(node)) {
    return("nothing")
  }

  # Literals: numbers, logicals, strings
  if (rlang::is_syntactic_literal(node)) {
    if (is.character(node)) {
      return(encodeString(node, quote = "\""))
    }
    return(num_to_julia(node))
  }

  # Bare names (variables, constants like pi); R loop keywords -> Julia
  if (is.symbol(node)) {
    nm <- as.character(node)
    if (nm == "T") {
      return("true")
    }
    if (nm == "F") {
      return("false")
    }
    if (nm == "next") {
      return("continue")
    }
    return(nm)
  }

  if (!is.call(node)) {
    .ast_bail()
  }

  fname <- tryCatch(rlang::call_name(node), error = function(e) NULL)
  args <- as.list(node)[-1L]

  # --- Assignment -----------------------------------------------------------
  if (!is.null(fname) && fname %in% c("<-", "<<-", "=")) {
    lhs <- args[[1L]]
    rhs <- args[[2L]]
    # `name = function(...) {...}` becomes a named Julia function definition.
    if (is.call(rhs) && identical(as.character(rhs[[1L]]), "function")) {
      return(emit_julia_function(rhs, emit_julia_node(lhs, var_names), var_names))
    }
    return(paste0(
      emit_julia_node(lhs, var_names), " = ", emit_julia_node(rhs, var_names)
    ))
  }

  # --- Control flow ---------------------------------------------------------
  if (!is.null(fname)) {
    if (fname == "function") {
      return(emit_julia_function(node, NULL, var_names))
    }
    if (fname == "{") {
      return(paste0("begin\n", emit_julia_body(node, var_names), "\nend"))
    }
    if (fname == "if") {
      return(emit_julia_if(node, var_names))
    }
    if (fname == "for") {
      return(paste0(
        "for ", emit_julia_node(args[[1L]], var_names),
        " in ", emit_julia_node(args[[2L]], var_names), "\n",
        emit_julia_body(args[[3L]], var_names), "\nend"
      ))
    }
    if (fname == "while") {
      return(paste0(
        "while ", emit_julia_node(args[[1L]], var_names), "\n",
        emit_julia_body(args[[2L]], var_names), "\nend"
      ))
    }
    if (fname == "repeat") {
      return(paste0("while true\n", emit_julia_body(args[[1L]], var_names), "\nend"))
    }
    if (fname == "break") {
      return("break")
    }
    if (fname == "next") {
      return("continue")
    }
  }

  # --- Parentheses ----------------------------------------------------------
  if (identical(fname, "(")) {
    return(paste0("(", emit_julia_node(args[[1L]], var_names), ")"))
  }

  # --- Range a:b (operands keep integer form) -------------------------------
  if (identical(fname, ":")) {
    return(paste0(
      emit_julia_index(args[[1L]], var_names), ":",
      emit_julia_index(args[[2L]], var_names)
    ))
  }

  # --- Indexing x[i], x[[i]] -> x[i] (integer indices) ----------------------
  if (!is.null(fname) && fname %in% c("[", "[[")) {
    base <- emit_julia_node(args[[1L]], var_names)
    idx <- vapply(args[-1L], emit_julia_index, character(1), var_names = var_names)
    return(paste0(base, "[", paste(idx, collapse = ", "), "]"))
  }

  # --- Unary +/- and logical not -------------------------------------------
  if (length(args) == 1L && !is.null(fname) &&
    fname %in% c("-", "+", "!")) {
    return(paste0(fname, emit_julia_node(args[[1L]], var_names)))
  }

  # --- Binary operators -----------------------------------------------------
  # Binary operators -> Julia. Arithmetic/comparison broadcast (.op); logical map
  # to scalar &&/||; %%/%/% map to the package's Unicode operators (emitted as the
  # \uXXXX escape text, converted downstream exactly like the legacy translator);
  # %*% is plain * and %in% is `in`.
  .ast_binary_ops <- c(
    "+" = ".+", "-" = ".-", "*" = ".*", "/" = "./", "^" = ".^",
    "==" = ".==", "!=" = ".!=", "<" = ".<", ">" = ".>",
    "<=" = ".<=", ">=" = ".>=",
    "&" = "&&", "|" = "||", "&&" = "&&", "||" = "||",
    "%%" = "\\u2295", "%/%" = "\\u2298", "%*%" = "*", "%in%" = "in"
  )

  if (length(args) == 2L && !is.null(fname) &&
    fname %in% names(.ast_binary_ops)) {
    return(paste0(
      emit_julia_node(args[[1L]], var_names), " ",
      .ast_binary_ops[[fname]], " ",
      emit_julia_node(args[[2L]], var_names)
    ))
  }

  # --- c(...) -> [...] ------------------------------------------------------
  if (identical(fname, "c")) {
    items <- vapply(args, emit_julia_node, character(1), var_names = var_names)
    return(paste0("[", paste(items, collapse = ", "), "]"))
  }

  # --- Function calls -------------------------------------------------------
  # Bail on non-identifier heads we have not handled (odd infix operators, odd
  # heads like (f)(x)); the fallback translator handles or rejects them.
  if (is.null(fname) || !grepl("^[A-Za-z.][A-Za-z0-9._]*$", fname)) {
    .ast_bail()
  }
  emit_julia_call(node, fname, args, var_names)
}
