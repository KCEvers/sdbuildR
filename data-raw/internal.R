# Use specific GitHub release version of SystemDynamicsBuildR (rather than main/ or dev/)
use_github_release <- TRUE

# Names of variables and functions
P <- list(
  debug = FALSE,
  insightmaker_version = 38, # version sdbuildR was made with
  jl_pkg_name = "SystemDynamicsBuildR",
  # jl_pkg_version = "0.2.5", # required version SystemDynamicsBuildR
  # jl_pkg_name = "StockFlowRSupport",
  jl_pkg_version_github_release = "0.3.3", # required version SystemDynamicsBuildR
  model_setup_name = "model_setup",
  func_name = "func",
  initial_value_name = "init",
  initial_value_names = "init_names",
  parameter_name = "constants",
  parameter_names = "constant_names",
  state_name = "current_state",
  change_prefix = "d",
  time_name = "t",
  change_state_name = "dSdt",
  times_name = "times",
  timestep_name = "dt",
  saveat_name = "saveat",
  savefrom_name = "savefrom",
  tstops_name = "tstops",
  # units_name = "units",
  # time_units_name = "time_units",
  # conveyor_suffix = "_conv",
  # delayN_suffix = "_delayN",
  # smoothN_suffix = "_smoothN",
  # delay_suffix = "_delay",
  # outflow_suffix = ".outflow",
  # acc_suffix = "_acc",
  # delay_idx_name = "idx",
  # past_suffix = "_past",
  # past_length_suffix = "_length",
  # fix_suffix = "_fix",
  # fix_length_suffix = "_fixlength",
  ensemble_prob_name = "ensemble_prob",
  ensemble_output_func = "output_func",
  ensemble_conditions = "ensemble_conditions",
  ensemble_pars = "ensemble_pars",
  ensemble_ctx = "ctx",
  ensemble_iter = "sim_id", # "i",
  ensemble_rng = "rng",
  ensemble_n = "ensemble_n",
  ensemble_total_n = "ensemble_total_n",
  ensemble_func_name = "prob_func",
  summary_df_name = "summary_df",
  summary_df_constants_name = "summary_df_constants",
  summary_df_init_name = "summary_df_init",
  sim_df_name = "df",
  prob_name = "prob",
  solution_name = "solve_out",
  ode_func_name = "ode_func",
  callback_func_name = "save_intermediaries",
  callback_name = "callback",
  intermediaries = "intermediaries",
  intermediary_df = "intermediary_df",
  intermediary_names = "intermediary_names",
  rootfun_name = "rootfun",
  eventfun_name = "eventfun",
  nonneg_stock_name = "nonneg_stock",
  saveat_func = "saveat_func",
  init_sdbuildR = "init_sdbuildR"
)


#' Get regular expressions for built-in Insight Maker functions
#'
#' @returns data.frame
#' @noRd
get_syntax_IM <- function() {
  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df <- matrix(
    c(
      # Mathematical Functions (27)
      "Round", "round_IM", "syntax1", FALSE, TRUE, "",
      "Ceiling", "ceiling", "syntax1", FALSE, TRUE, "",
      "Floor", "floor", "syntax1", FALSE, TRUE, "",
      "Cos", "cos", "syntax1", FALSE, TRUE, "",
      "ArcCos", "acos", "syntax1", FALSE, TRUE, "",
      "Sin", "sin", "syntax1", FALSE, TRUE, "",
      "ArcSin", "asin", "syntax1", FALSE, TRUE, "",
      "Tan", "tan", "syntax1", FALSE, TRUE, "",
      "ArcTan", "atan", "syntax1", FALSE, TRUE, "",
      "Log", "log10", "syntax1", FALSE, TRUE, "",
      "Ln", "log", "syntax1", FALSE, TRUE, "",
      "Exp", "exp", "syntax1", FALSE, TRUE, "",
      "Sum", "sum", "syntax1", TRUE, TRUE, "",
      "Product", "prod", "syntax1", TRUE, TRUE, "",
      "Max", "max", "syntax1", TRUE, TRUE, "",
      "Min", "min", "syntax1", TRUE, TRUE, "",
      "Mean", "mean", "syntax1", TRUE, TRUE, "",
      "Median", "median", "syntax1", TRUE, TRUE, "",
      "StdDev", "sd", "syntax1", TRUE, TRUE, "",
      "Abs", "abs", "syntax1", TRUE, TRUE, "",
      "Sqrt", "sqrt", "syntax1", FALSE, TRUE, "",
      "Sign", "sign", "syntax1", FALSE, TRUE, "",
      "Logit", "logit", "syntax1", FALSE, TRUE, "",
      "Expit", "expit", "syntax1", FALSE, TRUE, "",

      # Random Number Functions (13)
      "Rand", "runif", "syntax1", FALSE, FALSE, "1",
      "RandNormal", "rnorm", "syntax1", FALSE, FALSE, "1",
      "RandLognormal", "rlnorm", "syntax1", FALSE, FALSE, "1",
      "RandBoolean", "rbool", "syntax1", FALSE, FALSE, "",
      "RandBinomial", "rbinom", "syntax1", FALSE, FALSE, "1",
      "RandNegativeBinomial", "rnbinom", "syntax1", FALSE, FALSE, "1",
      "RandPoisson", "rpois", "syntax1", FALSE, FALSE, "1",
      "RandTriangular", "EnvStats::rtri", "syntax1", FALSE, FALSE, "1",
      "RandExp", "rexp", "syntax1", FALSE, FALSE, "1",
      "RandGamma", "rgamma", "syntax1", FALSE, FALSE, "1",
      "RandBeta", "rbeta", "syntax1", FALSE, FALSE, "1",
      "RandDist", "rdist", "syntax1", FALSE, FALSE, "1",
      "setRandSeed", "set.seed", "syntax1", FALSE, TRUE, "",

      # Statistical Distributions (20)
      "CDFNormal", "pnorm", "syntax1", FALSE, TRUE, "",
      "PDFNormal", "dnorm", "syntax1", FALSE, TRUE, "",
      "InvNormal", "qnorm", "syntax1", FALSE, TRUE, "",
      "CDFLognormal", "plnorm", "syntax1", FALSE, TRUE, "",
      "PDFLognormal", "dlnorm", "syntax1", FALSE, TRUE, "",
      "InvLognormal", "qlnorm", "syntax1", FALSE, TRUE, "",
      "CDFt", "pt", "syntax1", FALSE, TRUE, "",
      "PDFt", "dt", "syntax1", FALSE, TRUE, "",
      "Invt", "qt", "syntax1", FALSE, TRUE, "",
      "CDFF", "pf", "syntax1", FALSE, TRUE, "",
      "PDFF", "df", "syntax1", FALSE, TRUE, "",
      "InvF", "qf", "syntax1", FALSE, TRUE, "",
      "CDFChiSquared", "pchisq", "syntax1", FALSE, TRUE, "",
      "PDFChiSquared", "dchisq", "syntax1", FALSE, TRUE, "",
      "InvChiSquared", "qchisq", "syntax1", FALSE, TRUE, "",
      "CDFExponential", "pexp", "syntax1", FALSE, TRUE, "",
      "PDFExponential", "dexp", "syntax1", FALSE, TRUE, "",
      "InvExponential", "qexp", "syntax1", FALSE, TRUE, "",
      "CDFPoisson", "ppois", "syntax1", FALSE, TRUE, "",
      "PMFPoisson", "dpois", "syntax1", FALSE, TRUE, "",

      # User Input Functions (3)
      "Alert", "print", "syntax1", FALSE, TRUE, "",
      "Prompt", "readline", "syntax5", FALSE, TRUE, "",
      "Confirm", "readline", "syntax5", FALSE, TRUE, "",

      # String Functions (10)
      "Range", "", "syntax5", FALSE, TRUE, "",
      "Split", "strsplit", "syntax1", FALSE, TRUE, "",
      "UpperCase", "toupper", "syntax1", FALSE, TRUE, "",
      "LowerCase", "tolower", "syntax1", FALSE, TRUE, "",
      "Join", "stringr::str_flatten", "syntax1", FALSE, TRUE, "",
      "Trim", "trimws", "syntax1", FALSE, TRUE, "",
      "Parse", "as.numeric", "syntax1", FALSE, TRUE, "",

      # Vector Functions (20)
      "Length", "length_IM", "syntax1", FALSE, TRUE, "",
      # "Join", "c", "syntax1", ),
      # "Flatten", "purrr::flatten", "syntax2", ),
      "Unique", "unique", "syntax2", FALSE, TRUE, "",
      "Union", "union", "syntax2", FALSE, TRUE, "",
      "Intersection", "intersect", "syntax2", FALSE, TRUE, "",
      "Difference", "symdiff", "syntax2", FALSE, TRUE, "",
      "Sort", "sort", "syntax2", FALSE, TRUE, "",
      "Reverse", "rev", "syntax2", FALSE, TRUE, "",
      "Sample", "sample", "syntax2", FALSE, TRUE, "",
      "IndexOf", "indexof", "syntax2", FALSE, TRUE, "",
      "Contains", "contains_IM", "syntax2", FALSE, TRUE, "",
      "Keys", "names", "syntax2", FALSE, TRUE, "",
      "Values", "unname", "syntax2", FALSE, TRUE, "",
      "Map", "", "syntax5", FALSE, TRUE, "",
      "Filter", "", "syntax5", FALSE, TRUE, "",
      # "IMMAP", "conv_IMMAP", "syntax3", FALSE, TRUE, "",
      # "IMFILTER", "conv_IMFILTER", "syntax3", FALSE, TRUE, "",
      # General Functions (6)
      "IfThenElse", "ifelse", "syntax1", FALSE, TRUE, "",
      "Pause", "", "syntax5", FALSE, FALSE, "", # no R equivalent
      "Stop", "stop", "syntax5", FALSE, FALSE, "",
      # Syntax 3
      "Unitless", "", "syntax5", FALSE, TRUE, "",
      "PastValues", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastMax", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastMin", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastMedian", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastMean", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastStdDev", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "PastCorrelation", "conv_past_values", "syntax5", FALSE, TRUE, "",
      "Delay1", "conv_delayN", "syntax5", FALSE, TRUE, "",
      "Delay3", "conv_delayN", "syntax5", FALSE, TRUE, "",
      "DelayN", "conv_delayN", "syntax5", FALSE, TRUE, "",
      "Smooth", "conv_delayN", "syntax5", FALSE, TRUE, "",
      "SmoothN", "conv_delayN", "syntax5", FALSE, TRUE, "",
      "Delay", "conv_delay", "syntax5", FALSE, TRUE, "",
      "Fix", "", "syntax5", FALSE, TRUE, "",
      "Staircase", "conv_step", "syntax3", FALSE, TRUE, "", # synonym for Step()
      "Step", "conv_step", "syntax3", FALSE, TRUE, "",
      "Pulse", "conv_pulse", "syntax3", FALSE, TRUE, "",
      "Ramp", "conv_ramp", "syntax3", FALSE, TRUE, "",
      "Seasonal", "conv_seasonal", "syntax5", FALSE, TRUE, "", # not supported because Insight Maker's seasonal function has as a default a period of a year, and we no longer support units
      "Lookup", "conv_lookup", "syntax3", FALSE, TRUE, "",
      "Repeat", "", "syntax5", FALSE, TRUE, "",
      "Seconds", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Minutes", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Hours", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Days", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Weeks", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Months", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Quarters", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Years", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "Time", P[["time_name"]], "syntax0", FALSE, FALSE, "",
      "TimeStart", paste0(P[["times_name"]], "[1]"), "syntax0", FALSE, FALSE, "",
      "TimeStep", P[["timestep_name"]], "syntax0", FALSE, FALSE, "",
      "TimeEnd", paste0(P[["times_name"]], "[2]"), "syntax0", FALSE, FALSE, "",
      "TimeLength", paste0("(", P[["times_name"]], "[2] - ", P[["times_name"]], "[1])"), "syntax0", FALSE, FALSE, "",
      # For agent-based modelling functions, issue a warning that these will not be translated
      ".FindAll", "", "syntax4", FALSE, TRUE, "",
      ".FindState", "", "syntax4", FALSE, TRUE, "",
      ".FindNotState", "", "syntax4", FALSE, TRUE, "",
      ".FindIndex", "", "syntax4", FALSE, TRUE, "",
      ".FindNearby", "", "syntax4", FALSE, TRUE, "",
      ".FindNearest", "", "syntax4", FALSE, TRUE, "",
      ".FindFurthest", "", "syntax4", FALSE, TRUE, "",
      ".Value", "", "syntax4", FALSE, TRUE, "",
      ".SetValue", "", "syntax4", FALSE, TRUE, "",
      ".Location", "", "syntax4", FALSE, TRUE, "",
      ".Index", "", "syntax4", FALSE, TRUE, "",
      ".Location", "", "syntax4", FALSE, TRUE, "",
      ".SetLocation", "", "syntax4", FALSE, TRUE, "",
      "Distance", "", "syntax4", FALSE, TRUE, "",
      ".Move", "", "syntax4", FALSE, TRUE, "",
      ".MoveTowards", "", "syntax4", FALSE, TRUE, "",
      ".Connected", "", "syntax4", FALSE, TRUE, "",
      ".Connect", "", "syntax4", FALSE, TRUE, "",
      ".Unconnect", "", "syntax4", FALSE, TRUE, "",
      ".ConnectionWeight", "", "syntax4", FALSE, TRUE, "",
      ".SetConnectionWeight", "", "syntax4", FALSE, TRUE, "",
      ".PopulationSize", "", "syntax4", FALSE, TRUE, "",
      ".Add", "", "syntax4", FALSE, TRUE, "",
      ".Remove", "", "syntax4", FALSE, TRUE, "",
      "Width", "", "syntax4", FALSE, TRUE, "",
      "Height", "", "syntax4", FALSE, TRUE, ""
    ),
    ncol = 6, byrow = TRUE,
    dimnames = list(NULL, c(
      "insightmaker", "R", "syntax",
      "add_c()", "needs_brackets", "add_first_arg"
    ))
  )

  # Convert to data.frame
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  # Filter out syntax4 and syntax5
  df <- conv_df[conv_df[["syntax"]] %in%
    c("syntax0", "syntax1", "syntax2", "syntax3"), , drop = FALSE]

  # Initialize new columns
  df[["insightmaker_first_iter"]] <- df[["insightmaker"]]
  df[["insightmaker_regex_first_iter"]] <- ifelse(
    df[["syntax"]] %in% c("syntax0", "syntax1", "syntax3"),
    paste0("(?:^|(?<=\\W))", df[["insightmaker"]], "\\("),
    paste0("\\.", df[["insightmaker"]], "\\(")
  )
  df[["insightmaker"]] <- paste0(df[["insightmaker"]], "_replace")
  df[["insightmaker_regex"]] <- ifelse(
    df[["syntax"]] %in% c("syntax0", "syntax1", "syntax3"),
    paste0("(?:^|(?<=\\W))", df[["insightmaker"]], "\\("),
    paste0("\\.", df[["insightmaker"]], "\\(")
  )

  # Create additional rows for syntax0b and syntax1b
  additional_rows <- conv_df[conv_df[["syntax"]] %in% c("syntax0", "syntax1") &
    !as.logical(conv_df[["needs_brackets"]]), ]
  if (nrow(additional_rows) > 0) {
    additional_rows[["insightmaker_first_iter"]] <- additional_rows[["insightmaker"]]
    additional_rows[["insightmaker_regex_first_iter"]] <- paste0("(?:^|(?<=\\W))", additional_rows[["insightmaker"]], "(?=(?:\\W|$))")
    additional_rows[["insightmaker"]] <- paste0(additional_rows[["insightmaker"]], "_replace")
    additional_rows[["insightmaker_regex"]] <- paste0("(?:^|(?<=\\W))", additional_rows[["insightmaker"]], "(?=(?:\\W|$))")
    additional_rows[["syntax"]] <- paste0(additional_rows[["syntax"]], "b")

    # Combine rows
    syntax_df <- rbind(df, additional_rows)
  } else {
    syntax_df <- df
  }

  # Reset row names
  rownames(syntax_df) <- NULL

  # Unsupported functions
  syntax_df_unsupp <- conv_df[conv_df[["syntax"]] %in% c("syntax4", "syntax5"), ,
    drop = FALSE
  ]
  syntax_df_unsupp[["insightmaker_regex"]] <- paste0(
    "(?:^|(?<=\\W))",
    stringr::str_escape(syntax_df_unsupp[["insightmaker"]]),
    "\\("
  )

  # Create additional rows for those that do not need brackets
  additional_rows <- syntax_df_unsupp[!as.logical(syntax_df_unsupp[["needs_brackets"]]), ]
  if (nrow(additional_rows) > 0) {
    additional_rows[["insightmaker_regex"]] <- paste0(
      "(?:^|(?<=\\W))",
      stringr::str_escape(additional_rows[["insightmaker"]]), "(?=(?:\\W|$))"
    )
    additional_rows[["syntax"]] <- paste0(additional_rows[["syntax"]], "b")

    # Combine rows
    syntax_df_unsupp <- rbind(syntax_df_unsupp, additional_rows)
  }

  return(list(syntax_df = syntax_df, syntax_df_unsupp = syntax_df_unsupp))
}


#' Get regular expressions for Julia functions
#'
#' @noRd
#' @returns data.frame
get_syntax_julia <- function() {
  # Custom function to replace each (nested) function; necessary because regex in stringr unfortunately doesn't seem to handle nested functions
  conv_df <- matrix(
    c(
      # Statistics
      "min", "r_min", "syntax1", "", "", FALSE,
      "max", "r_max", "syntax1", "", "", FALSE,
      "pmin", "min", "syntax1", "", "", TRUE,
      "pmax", "max", "syntax1", "", "", TRUE,
      "mean", "Statistics.mean", "syntax1", "", "", FALSE,
      "median", "Statistics.median", "syntax1", "", "", FALSE,
      "prod", "prod", "syntax1", "", "", FALSE,
      "sum", "sum", "syntax1", "", "", FALSE,
      "sd", "Statistics.std", "syntax1", "", "", FALSE,
      "cor", "Statistics.cor", "syntax1", "", "", FALSE,
      "cov", "Statistics.cov", "syntax1", "", "", FALSE,
      "var", "Statistics.var", "syntax1", "", "", FALSE,
      "range", "r_range", "syntax1", "", "", FALSE,
      "as.logical", "r_as_logical", "syntax1", "", "", TRUE,
      "seq", "range", "syntax_seq", "", "", FALSE,
      "seq.int", "range", "syntax_seq", "", "", FALSE,
      "seq_along", "range", "syntax_seq", "", "", FALSE,
      "seq_len", "range", "syntax_seq", "", "", FALSE,
      "sample", "StatsBase.sample", "syntax_sample", "", "", FALSE,
      "sample.int", "StatsBase.sample", "syntax_sample", "", "", FALSE,
      "cumsum", "cumsum", "syntax1", "", "", FALSE,
      "cumprod", "cumprod", "syntax1", "", "", FALSE,
      "cummax", "r_cummax", "syntax1", "", "", FALSE,
      "cummin", "r_cummin", "syntax1", "", "", FALSE,
      "diff", "r_diff", "syntax1", "", "", FALSE,
      "rep", "r_rep", "syntax1", "", "", FALSE,
      "factorial", "factorial", "syntax1", "", "", TRUE,
      "choose", "binomial", "syntax1", "", "", TRUE,
      "trimws", "strip", "syntax1", "", "", TRUE,
      "abs", "abs", "syntax1", "", "", TRUE,
      "sign", "sign", "syntax1", "", "", TRUE,
      "cos", "cos", "syntax1", "", "", TRUE,
      "sin", "sin", "syntax1", "", "", TRUE,
      "tan", "tan", "syntax1", "", "", TRUE,
      "acos", "acos", "syntax1", "", "", TRUE,
      "asin", "asin", "syntax1", "", "", TRUE,
      "atan", "atan", "syntax1", "", "", TRUE,
      "cospi", "cospi", "syntax1", "", "", TRUE,
      "sinpi", "sinpi", "syntax1", "", "", TRUE,
      "tanpi", "tanpi", "syntax1", "", "", TRUE,
      "nchar", "length", "syntax1", "", "", TRUE,
      "floor", "floor", "syntax1", "", "", TRUE,
      "ceiling", "ceil", "syntax1", "", "", TRUE,
      "round", "round_", "syntax1", "", "", TRUE,
      "trunc", "trunc", "syntax1", "", "", TRUE,

      # Find
      # "which", "findall", "syntax1", "", "",
      # findmax(arr): Returns (max_value, index).
      # findmin(arr): Returns (min_value, index).

      "which.min", "argmin", "syntax1", "", "", FALSE,
      "which.max", "argmax", "syntax1", "", "", FALSE,
      "exp", "exp", "syntax1", "", "", TRUE,
      "expm1", "expm1", "syntax1", "", "", TRUE,
      # "log", "log", "syntax1", "", "", TRUE, # **to do, put base first!
      # "logb", "logb", "syntax1", "", "", TRUE,
      "log2", "log2", "syntax1", "", "", TRUE,
      "log10", "log10", "syntax1", "", "", TRUE,
      "sqrt", "sqrt", "syntax1", "", "", TRUE,
      "dim", "size", "syntax1", "", "", FALSE,
      "nrow", "size", "syntax1", "", "1", FALSE,
      "ncol", "size", "syntax1", "", "2", FALSE,
      "cbind", "hcat", "syntax1", "", "", FALSE,
      "rbind", "r_rbind", "syntax1", "", "", FALSE,
      "rowSums", "r_rowsums", "syntax1", "", "", FALSE,
      "colSums", "r_colsums", "syntax1", "", "", FALSE,
      "rowMeans", "r_rowmeans", "syntax1", "", "", FALSE,
      "colMeans", "r_colmeans", "syntax1", "", "", FALSE,

      # Matrix functions
      "diag", "LinearAlgebra.diag", "syntax1", "", "", FALSE,
      "upper.tri", "r_upper_tri", "syntax1", "", "", FALSE,
      "lower.tri", "r_lower_tri", "syntax1", "", "", FALSE,
      "norm", "LinearAlgebra.norm", "syntax1", "", "", FALSE,
      "det", "LinearAlgebra.det", "syntax1", "", "", FALSE,
      "t", "transpose", "syntax1", "", "", FALSE,
      "rev", "reverse", "syntax1", "", "", FALSE,
      "print", "println", "syntax1", "", "", FALSE,
      "na.omit", "r_na_omit", "syntax1", "", "", FALSE,
      "eigen", "LinearAlgebra.eigen", "syntax1", "", "", FALSE,
      "getwd", "pwd", "syntax1", "", "", FALSE,
      "setwd", "cd", "syntax1", "", "", FALSE,
      "Filter", "filter", "syntax1", "", "", TRUE,
      "which", "findall", "syntax1", "", "", FALSE,
      "match", "r_match", "syntax1", "", "", FALSE,
      "unique", "unique", "syntax1", "", "", FALSE,
      "ifelse", "ifelse", "syntax1", "", "", TRUE,
      "class", "typeof", "syntax1", "", "", FALSE,
      # String manipulation
      "grep", "r_grep", "syntax1", "", "", FALSE,
      "strsplit", "split", "syntax1", "", "", FALSE,
      "paste0", "string", "syntax_paste", "", "", TRUE,
      "toupper", "uppercase", "syntax1", "", "", TRUE,
      "tolower", "lowercase", "syntax1", "", "", TRUE,
      "startsWith", "startswith", "syntax1", "", "", TRUE,
      "endsWith", "endswith", "syntax1", "", "", TRUE,
      "stringr::str_to_title", "titlecase", "syntax1", "", "", TRUE,
      # Sets
      "union", "union", "syntax1", "", "", FALSE,
      "intersect", "intersect", "syntax1", "", "", FALSE,
      "setdiff", "setdiff", "syntax1", "", "", FALSE,
      "setequal", "issetequal", "syntax1", "", "", FALSE,
      # is....()
      "rlang::is_empty", "isempty", "syntax1", "", "", FALSE,
      "all", "all", "syntax1", "", "", FALSE,
      "any", "any", "syntax1", "", "", FALSE,
      "is.infinite", "isinf", "syntax1", "", "", TRUE,
      "is.finite", "isfinite", "syntax1", "", "", TRUE,
      "is.nan", "isnan", "syntax1", "", "", TRUE,
      # https://docs.julialang.org/en/v1/base/collections
      # Julia: indexin, sortperm, findfirst
      "sort", "r_sort", "syntax1", "", "", FALSE,
      # Complex numbers
      "Re", "real", "syntax1", "", "", TRUE,
      "Im", "imag", "syntax1", "", "", TRUE,
      "Mod", "abs", "syntax1", "", "", TRUE,
      "Arg", "angle", "syntax1", "", "", TRUE,
      "Conj", "conj", "syntax1", "", "", TRUE,
      # Custom functions
      "logistic", "logistic", "syntax1", "", "", TRUE,
      "sigmoid", "logistic", "syntax1", "", "", TRUE,
      "hill", "hill", "syntax1", "", "", TRUE,
      "ricker", "ricker", "syntax1", "", "", TRUE,
      "logit", "logit", "syntax1", "", "", TRUE,
      "expit", "expit", "syntax1", "", "", TRUE,
      # step() is already an existing function in Julia, so we use make_step()
      # instead, as well as for the others for consistency
      "step", "make_step", "syntax1", "", "", FALSE,
      "pulse", "make_pulse", "syntax1", "", "", FALSE,
      "ramp", "make_ramp", "syntax1", "", "", FALSE,
      "seasonal", "make_seasonal", "syntax1", "", "", FALSE,
      "length_IM", "length", "syntax1", "", "", FALSE,

      # Random Number Functions (13)
      "runif", "rand", "syntaxD", "Distributions.Uniform", "", FALSE,
      "rnorm", "rand", "syntaxD", "Distributions.Normal", "", FALSE,
      "rlnorm", "rand", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "rbool", "rbool", "syntax1", "", "", FALSE,
      "rbinom", "rand", "syntaxD", "Distributions.Binomial", "", FALSE,
      "rnbinom", "rand", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "rpois", "rand", "syntaxD", "Distributions.Poisson", "", FALSE,
      # "EnvStats::rtri", "", "syntaxD", "", "", FALSE,
      "rexp", "rand", "syntaxD", "Distributions.Exponential", "", FALSE,
      "rgamma", "rand", "syntaxD", "Distributions.Gamma", "", FALSE,
      "rbeta", "rand", "syntaxD", "Distributions.Beta", "", FALSE,
      "rcauchy", "rand", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "rchisq", "rand", "syntaxD", "Distributions.Chisq", "", FALSE,
      "rgeom", "rand", "syntaxD", "Distributions.Geometric", "", FALSE,
      "rf", "rand", "syntaxD", "Distributions.FDist", "", FALSE,
      # "rhyper", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rlogis", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rmultinom", "rand", "syntaxD", "Distributions.Multinomial", "", FALSE,
      # "rsignrank", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rt", "rand", "syntaxD", "Distributions.TDist", "", FALSE,
      "rweibull", "rand", "syntaxD", "Distributions.Weibull", "", FALSE,
      # "rwilcox", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rbirthday", "rand", "syntaxD", "Distributions.", "", FALSE,
      # "rtukey", "rand", "syntaxD", "Distributions.", "", FALSE,
      "rdist", "rdist", "syntax1", "", "", FALSE,
      "set.seed", "Random.seed!", "syntax1", "", "", FALSE,
      # Statistical Distributions (20)
      "punif", "Distributions.cdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "dunif", "Distributions.pdf.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "qunif", "Distributions.quantile.", "syntaxD", "Distributions.Uniform", "", FALSE,
      "pnorm", "Distributions.cdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "dnorm", "Distributions.pdf.", "syntaxD", "Distributions.Normal", "", FALSE,
      "qnorm", "Distributions.quantile.", "syntaxD", "Distributions.Normal", "", FALSE,
      "plnorm", "Distributions.cdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "dlnorm", "Distributions.pdf.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "qlnorm", "Distributions.quantile.", "syntaxD", "Distributions.LogNormal", "", FALSE,
      "pbinom", "Distributions.cdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "dbinom", "Distributions.pdf.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "qbinom", "Distributions.quantile.", "syntaxD", "Distributions.Binomial", "", FALSE,
      "pnbinom", "Distributions.cdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "dnbinom", "Distributions.pdf.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "qnbinom", "Distributions.quantile.", "syntaxD", "Distributions.NegativeBinomial", "", FALSE,
      "pgamma", "Distributions.cdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "dgamma", "Distributions.pdf.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "qgamma", "Distributions.quantile.", "syntaxD", "Distributions.Gamma", "", FALSE,
      "pbeta", "Distributions.cdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "dbeta", "Distributions.pdf.", "syntaxD", "Distributions.Beta", "", FALSE,
      "qbeta", "Distributions.quantile.", "syntaxD", "Distributions.Beta", "", FALSE,
      "pcauchy", "Distributions.cdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "dcauchy", "Distributions.pdf.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "qcauchy", "Distributions.quantile.", "syntaxD", "Distributions.Cauchy", "", FALSE,
      "pgeom", "Distributions.cdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dgeom", "Distributions.pdf.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "qgeom", "Distributions.quantile.", "syntaxD", "Distributions.Geometric", "", FALSE,
      "dmultinom", "Distributions.pdf.", "syntaxD", "Distributions.Multinomial", "", FALSE,
      "pweibull", "Distributions.cdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "dweibull", "Distributions.pdf.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "qweibull", "Distributions.quantile.", "syntaxD", "Distributions.Weibull", "", FALSE,
      "pt", "Distributions.cdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "dt", "Distributions.pdf.", "syntaxD", "Distributions.TDist", "", FALSE,
      "qt", "Distributions.quantile.", "syntaxD", "Distributions.TDist", "", FALSE,
      "pf", "Distributions.cdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "df", "Distributions.pdf.", "syntaxD", "Distributions.FDist", "", FALSE,
      "qf", "Distributions.quantile.", "syntaxD", "Distributions.FDist", "", FALSE,
      "pchisq", "Distributions.cdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "dchisq", "Distributions.pdf.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "qchisq", "Distributions.quantile.", "syntaxD", "Distributions.Chisq", "", FALSE,
      "pexp", "Distributions.cdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "dexp", "Distributions.pdf.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "qexp", "Distributions.quantile.", "syntaxD", "Distributions.Exponential", "", FALSE,
      "ppois", "Distributions.cdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "dpois", "Distributions.pdf.", "syntaxD", "Distributions.Poisson", "", FALSE,
      "qpois", "Distributions.quantile.", "syntaxD", "Distributions.Poisson", "", FALSE,
      # Complete replacements (syntax0)
      "next", "continue", "syntax0", "", "", FALSE,
      "stop", "error", "syntax0", "", "", FALSE
    ),
    ncol = 6, byrow = TRUE,
    dimnames = list(NULL, c("R", "julia", "syntax", "add_first_arg", "add_second_arg", "add_broadcast"))
  )

  # Convert to data.frame
  conv_df <- as.data.frame(conv_df, stringsAsFactors = FALSE)

  # Whether sort_args() should fill R's default arguments and reorder against R's
  # formals. TRUE only when the Julia target faithfully mirrors R's positional
  # signature, so filled defaults land in the right positions: distributions
  # (syntaxD), seq, sample, and faithful wrappers/custom functions that take
  # options users may name out of order. For all other syntax1 functions the
  # provided arguments are passed through unchanged (Julia supplies its own
  # defaults). See sort_args().
  fill_defaults_funcs <- c(
    "grep", "rep", "diff",
    "logistic", "sigmoid", "hill", "ricker",
    "step", "pulse", "ramp", "seasonal"
  )
  conv_df[["fill_defaults"]] <-
    conv_df[["syntax"]] %in% c("syntaxD", "syntax_seq", "syntax_sample") |
      conv_df[["R"]] %in% fill_defaults_funcs

  # Create syntax_df by copying conv_df
  syntax_df <- conv_df

  # Add and modify columns
  syntax_df[["R_first_iter"]] <- syntax_df[["R"]]
  syntax_df[["R_regex_first_iter"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "(?=(?:\\W|$))"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )
  syntax_df[["R"]] <- paste0(syntax_df[["R"]], "_replace")
  syntax_df[["R_regex"]] <- ifelse(
    syntax_df[["syntax"]] == "syntax0",
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "(?=(?:\\W|$))"),
    paste0("(?<!\\.)\\b", syntax_df[["R"]], "\\(")
  )

  return(list(syntax_df = syntax_df, conv_df = conv_df))
}


#' Internal function to fetch Project.toml from GitHub
#'
#' @param use_github_release Whether to fetch from GitHub release (TRUE) or main branch (FALSE)
#' @returns character vector of lines from Project.toml
#' @noRd
fetch_jl_Project_toml <- function(use_github_release = TRUE) {
  ref <- ifelse(use_github_release, paste0("v", P[["jl_pkg_version_github_release"]]), "main")
  url <- sprintf(
    "https://raw.githubusercontent.com/kcevers/%s.jl/%s/Project.toml",
    P[["jl_pkg_name"]], ref
  )

  lines <- readLines(url)
  lines
}

#' Internal function to extract Julia version from Project.toml lines
#'
#' @param lines character vector of lines from Project.toml
#' @returns Julia version as a string, or NA if not found
#' @noRd
get_julia_version_from_Project_toml <- function(lines) {
  julia_version_line <- lines[grepl("^julia\\s*=\\s*\"([^\"]+)\"", lines)]
  if (length(julia_version_line) == 1) {
    julia_version <- sub("^julia\\s*=\\s*\"([^\"]+)\".*", "\\1", julia_version_line)
    return(julia_version)
  } else {
    warning("Could not find Julia version in Project.toml")
    return(NA)
  }
}


#' Internal function to create inst/setup.jl file for Julia
#'
#' @returns NULL
#' @noRd
#'
create_julia_setup <- function(use_github_release = TRUE) {
  pkg_name <- "sdbuildR"
  jl_pkg_name <- P[["jl_pkg_name"]]

  script_setup <- sprintf(
    '# setup.jl - One-time setup script for %s Julia environment

println("Setting up Julia environment for %s...\\n\\n")

using Pkg

# Use the environment path provided by R (install_julia_env), falling back to
# the directory of this script if setup.jl is run directly.
env_path = isdefined(Main, :sdbuildR_env_path) ? sdbuildR_env_path : @__DIR__

# println("Activating environment at: ", env_path, "\\n")
Pkg.activate(env_path)

# Install %s from GitHub
println("\\nInstalling %s.jl from GitHub...")
Pkg.add(url="https://github.com/kcevers/%s.jl"%s)

# Install all other dependencies from Project.toml
println("\\nInstalling dependencies from Project.toml...")
Pkg.instantiate()

## Resolve dependencies without installing
# Pkg.resolve()

# Precompile packages for faster loading
println("\\nPrecompiling packages...")
Pkg.precompile()

println("\\nSetup complete!")
', pkg_name, pkg_name,
    jl_pkg_name, jl_pkg_name, jl_pkg_name,
    ifelse(use_github_release, paste0(", rev = \"v", P[["jl_pkg_version_github_release"]], "\""), "")
  )

  # Write scripts
  filepath <- system.file("setup.jl", package = "sdbuildR")
  write_script(script_setup, filepath)

  invisible()
}


#' Internal function to create inst/Project.toml and inst/init.jl files for Julia
#'
#' @returns NULL
#' @noRd
#'
create_julia_project_toml_init <- function(use_github_release = TRUE) {
  lines <- fetch_jl_Project_toml(use_github_release = use_github_release)

  # Get uuid
  uuid_line <- lines[grepl("^uuid\\s*=\\s*\"([^\"]+)\"", lines)]
  if (length(uuid_line) == 1) {
    uuid <- sub("^uuid\\s*=\\s*\"([^\"]+)\".*", "\\1", uuid_line)
  } else {
    warning("Could not find uuid in Project.toml")
    uuid <- NA
  }

  # Remove uuid, version, authors lines
  lines <- lines[!grepl("^(uuid|version|authors)\\s*=", lines)]

  # Extract dependency names from [deps] section
  deps_start <- which(lines == "[deps]")
  if (length(deps_start) == 1) {
    # Find next section header or end of file
    section_headers <- which(grepl("^\\[", lines))
    deps_end <- section_headers[section_headers > deps_start][1]
    if (is.na(deps_end)) deps_end <- length(lines) + 1
    deps_lines <- lines[(deps_start + 1):(deps_end - 1)]
    deps_names <- gsub("\\s*=.*", "", deps_lines[grepl("=", deps_lines)])

    # Remove Test.jl dependency if present
    lines <- lines[!grepl("^Test\\s*=", lines)]
    deps_names <- deps_names[deps_names != "Test"]
    

  } else {
    deps_names <- character(0)
  }

  # Insert dependency on SystemDynamicsBuildR.jl
  lines <- append(lines, paste0(P[["jl_pkg_name"]], " = \"", uuid, "\""), after = deps_start)
  deps_names <- c(deps_names, P[["jl_pkg_name"]])

  # Replace package name with sdbuildR
  pkg_name <- "sdbuildR"
  lines <- gsub(
    sprintf('^name\\s*=\\s*"%s"', P[["jl_pkg_name"]]),
    sprintf('name = "%s"', pkg_name),
    lines
  )

  # Remove [extras] and [targets] sections if they exist
  extras_start <- which(lines == "[extras]")
  if (length(extras_start) == 1) {
    section_headers <- which(grepl("^\\[", lines))
    extras_end <- section_headers[section_headers > extras_start][1]
    if (is.na(extras_end)) extras_end <- length(lines) + 1
    lines <- lines[-(extras_start:(extras_end - 1))]
  }

  targets_start <- which(lines == "[targets]")
  if (length(targets_start) == 1) {
    section_headers <- which(grepl("^\\[", lines))
    targets_end <- section_headers[section_headers > targets_start][1]
    if (is.na(targets_end)) targets_end <- length(lines) + 1
    lines <- lines[-(targets_start:(targets_end - 1))]
  }

  script_project_toml <- paste(lines, collapse = "\n")

  # Write script
  filepath <- system.file("Project.toml", package = "sdbuildR")
  write_script(script_project_toml, filepath)

  # init.jl
  using_lines <- paste0("using ", deps_names, collapse = "\n")

  script_init <- paste0(
    "# init.jl - Script to initialize Julia environment for ", pkg_name, "\n\n",
    "# Load packages\n",
    # "using ", P[["jl_pkg_name"]], "\n",
    using_lines, "\n\n",
    # "# Extend min/max: when applied to a single vector, use minimum, like in R\n",
    # "Base.min(v::AbstractVector) = minimum(v)\n",
    # "Base.max(v::AbstractVector) = maximum(v)\n",  "\n\n",
    "# Add initialization of ", pkg_name, "\n",
    P[["init_sdbuildR"]], " = true\n"
  )

  # filepath <- file.path(env_path, "init.jl")
  filepath <- system.file("init.jl", package = "sdbuildR")
  write_script(script_init, filepath)

  invisible()
}


#' Internal function to fetch exported Julia functions from GitHub
#'
#' @param use_github_release Whether to fetch from GitHub release (TRUE) or main branch (FALSE)
#' @returns character vector of exported function names from the Julia package
#' @noRd
fetch_jl_functions <- function(use_github_release = TRUE) {
  ref <- ifelse(use_github_release, paste0("v", P[["jl_pkg_version_github_release"]]), "main")
  url <- sprintf(
    "https://raw.githubusercontent.com/kcevers/%s.jl/%s/src/%s.jl",
    P[["jl_pkg_name"]], ref, P[["jl_pkg_name"]]
  )

  lines <- readLines(url)

  # Extract exported function names
  export_lines <- lines[grepl("^export\\s+", lines)]
  exported_function_lines <- gsub("^export\\s+(.+)", "\\1", export_lines)
  exported_functions <- unlist(strsplit(exported_function_lines, ",\\s*"))
  exported_functions <- trimws(exported_functions)

  exported_functions
}

##### Create Julia inst/init.jl, inst/setup_jl, inst/Project.toml #####
create_julia_setup()
create_julia_project_toml_init()

##### Overwrite Julia version in P with the one from Project.toml #####
jl_version <- get_julia_version_from_Project_toml(fetch_jl_Project_toml(use_github_release = use_github_release))
if (!is.na(jl_version)) {
  P[["jl_required_version"]] <- jl_version
} else {
  warning("Using default Julia version from P because it could not be fetched from Project.toml")
}

##### Create internal variables for syntax conversion #####
syntax_IM <- get_syntax_IM()
syntax_julia <- get_syntax_julia()


##### Protected names not to be used as variable names #####
protected_names <- c(
  # Reserved words in R
  "if",
  "else", "repeat", "function", "return", "while", "for", "in", "next", "break", "TRUE", "FALSE", # already protected
  "T", "F",
  # "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", # already protected
  "time", # used as first variable in simulation dataframe #"Time", "TIME",
  # "constraints",
  # Add Julia keywords
  "baremodule", "begin", "break", "catch", "const", "continue", "do",
  "else", "elseif", "end", "export", "false", "finally",
  "global", "error", "throw",
  "import", "let", "local", P[["func_name"]], "module", "quote", "return", "struct", "true", "try", "catch", "using",
  "Missing", "missing", "Nothing", "nothing",

  # Add R custom functions
  get_exported_functions("sdbuildR"),

  # Add Julia custom function names
  fetch_jl_functions(use_github_release = use_github_release),

  # These are variables in the ode and cannot be model element names
  unname(unlist(P[names(P) %in% c(
    "jl_pkg_name", "model_setup_name", "func_name", "initial_value_name",
    "initial_value_names", "parameter_name", "parameter_names",
    "state_name", "time_name", "change_state_name", "times_name",
    "timestep_name", "saveat_name", "ensemble_iter",
    "ode_func_name", "callback_func_name", "callback_name", "intermediaries",
    "rootfun_name", "eventfun_name",
    "init_sdbuildR"
  )]))
) |> unique()

##### Save internal variables for global access throughout the package #####
usethis::use_data(syntax_IM, syntax_julia, P,
  protected_names,
  internal = TRUE, overwrite = TRUE
)
