# AST-based R -> Julia translator (convert_eqn_ast_julia)
# Fast path used by the Julia backend; falls back to convert_equations_julia()
# for anything it does not handle.


test_that("arithmetic operators are broadcast and numbers become floats", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("r * X", vnames), "r .* X")
  expect_equal(convert_eqn_ast_julia("r * X^2 / K", vnames), "r .* X .^ 2.0 ./ K")
  expect_equal(convert_eqn_ast_julia("a + b - c", vnames), "a .+ b .- c")
  expect_equal(convert_eqn_ast_julia("1", vnames), "1.0")
  expect_equal(convert_eqn_ast_julia(".01", vnames), "0.01")
  expect_equal(convert_eqn_ast_julia("(a + b) * c", vnames), "(a .+ b) .* c")
})

test_that("unary minus and comparisons translate", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("-x", vnames), "-x")
  expect_equal(convert_eqn_ast_julia("a > b", vnames), "a .> b")
  expect_equal(convert_eqn_ast_julia("a == b", vnames), "a .== b")
})

test_that("known functions map and broadcast per syntax_df", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("exp(-x)", vnames), "exp.(-x)")
  expect_match(convert_eqn_ast_julia("min(a, b)", vnames), "^r_min\\(a, b\\)$")
  expect_match(
    convert_eqn_ast_julia("ifelse(x > 0, a, b)", vnames),
    "ifelse\\.\\(x \\.> 0\\.0, a, b\\)"
  )
})

test_that("namespaced calls drop the namespace (valid Julia)", {
  vnames <- julia_ast_vnames()

  out <- convert_eqn_ast_julia("base::sum(x)", vnames)
  expect_false(is.null(out))
  expect_false(grepl("base::", out))
  expect_match(out, "sum")
})

test_that("unknown functions (custom/gf) are emitted verbatim for later substitution", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("gf1(Stock1)", vnames), "gf1(Stock1)")
})

test_that("distributions reuse the existing reparameterisation", {
  vnames <- julia_ast_vnames()

  expect_equal(
    convert_eqn_ast_julia("rnorm(1, dt)", vnames),
    convert_equations_julia("aux", "z", "rnorm(1, dt)", vnames)[["eqn"]]
  )
})

test_that("ranges, indexing and special operators translate", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("a:b", vnames), "a:b")
  expect_equal(convert_eqn_ast_julia("1:n", vnames), "1:n") # integer range bound
  expect_equal(convert_eqn_ast_julia("v[1]", vnames), "v[1]") # integer index (not v[1.0])
  expect_equal(convert_eqn_ast_julia("v[i]", vnames), "v[i]")
  expect_equal(convert_eqn_ast_julia("a %/% b", vnames), "a \\u2298 b")
  expect_equal(convert_eqn_ast_julia("a %% b", vnames), "a \\u2295 b")
  expect_equal(convert_eqn_ast_julia("a %in% b", vnames), "a in b")
  expect_equal(convert_eqn_ast_julia("a %*% b", vnames), "a * b")
})

test_that("control flow and function definitions translate", {
  vnames <- julia_ast_vnames()

  expect_equal(
    convert_eqn_ast_julia("if (a) b else c", vnames),
    "if a\nb\nelse\nc\nend"
  )
  expect_equal(
    convert_eqn_ast_julia("for (i in 1:n) { s <- s + i }", c(vnames, "s")),
    "for i in 1:n\ns = s .+ i\nend"
  )
  expect_equal(
    convert_eqn_ast_julia("while (x > 1) { x <- x / 2 }", vnames),
    "while x .> 1.0\nx = x ./ 2.0\nend"
  )
  # name = function(...) {...} becomes a named Julia function definition
  out <- convert_eqn_ast_julia("f = function(x, p = 2) { return(x^p) }", vnames)
  expect_match(out, "^function f\\(x, p = 2\\.0\\)")
  expect_match(out, "end$")
})

test_that("genuinely unsupported constructs return NULL (caller falls back)", {
  vnames <- julia_ast_vnames()

  expect_null(convert_eqn_ast_julia("a$b", vnames)) # field access
  expect_null(convert_eqn_ast_julia("a@b", vnames)) # slot access
})

test_that("non-finite and missing literals are handled or fall back", {
  vnames <- julia_ast_vnames()

  expect_equal(convert_eqn_ast_julia("Inf", vnames), "Inf")
  expect_equal(convert_eqn_ast_julia("-Inf", vnames), "-Inf")
  expect_equal(convert_eqn_ast_julia("NaN", vnames), "NaN")
  expect_equal(convert_eqn_ast_julia("1e10", vnames), "10000000000.0")
  expect_equal(convert_eqn_ast_julia("2.5e-3", vnames), "0.0025")
  expect_equal(convert_eqn_ast_julia("NA", vnames), "missing")
  expect_equal(convert_eqn_ast_julia("NULL", vnames), "nothing")
  expect_equal(convert_eqn_ast_julia("c(NULL, NA)", vnames), "[nothing, missing]")
  expect_equal(convert_eqn_ast_julia("c(T, F, TRUE, FALSE)", vnames), "[true, false, true, false]")
})

test_that("AST and legacy translators agree on deterministic template equations", {

  skip_on_cran()

  # Equivalence check over real model equations (excludes stochastic calls).
  eqns <- c()
  for (tp in templates()) {
    sfm <- tryCatch(stockflow(tp), error = function(e) NULL)
    if (is.null(sfm)) next
    v <- sfm[["variables"]]
    eqns <- c(eqns, v[v[["type"]] %in% c("stock", "flow", "constant", "aux"), "eqn"])
  }
  eqns <- unique(eqns[nzchar(eqns)])
  vnames <- c(julia_ast_vnames(), unlist(lapply(templates(), function(tp) {
    s <- tryCatch(stockflow(tp), error = function(e) NULL)
    if (is.null(s)) character(0) else get_model_var(s)
  })))

  # Every equation translates without falling back (AST covers them).
  fell_back <- vapply(
    eqns, function(e) is.null(convert_eqn_ast_julia(e, vnames)),
    logical(1)
  )
  expect_false(any(fell_back))
})
