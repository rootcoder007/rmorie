# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/generate_runnable_examples.R
#
# rOpenSci #770 :eyes: 8 — replace 250 boilerplate `\dontrun{<vignette pointer>}`
# example blocks with auto-generated runnable examples driven by formals().
#
# Strategy:
#   1. For every exported function whose @examples is the literal boilerplate,
#      examine formals(fn) and craft a candidate call using arg-name heuristics.
#   2. Execute the candidate in a subprocess with a 5-s timeout.
#   3. Functions whose candidate runs without error become "runnable";
#      the boilerplate \dontrun is replaced with the candidate.
#   4. Functions whose candidate errors are left untouched.
#
# Run with:
#   Rscript tools/generate_runnable_examples.R [--apply | --dry]

suppressMessages({
  library(morie)
  library(callr)
})

ARGS <- commandArgs(trailingOnly = TRUE)
APPLY <- "--apply" %in% ARGS

# ---- value-guess heuristics (by formal argument name) --------------------
guess <- function(name) {
  switch(name,
    # common univariate / matrix data
    x = quote(rnorm(50)), y = quote(rnorm(50)), z = quote(rnorm(50)),
    X = quote(matrix(rnorm(150), 50, 3)),
    Y = quote(matrix(rnorm(100), 50, 2)),
    Z = quote(matrix(rnorm(100), 50, 2)),
    Q = quote(matrix(rnorm(150), 50, 3)),
    K_mat = quote(matrix(rnorm(150), 50, 3)),
    V = quote(matrix(rnorm(150), 50, 3)),
    # scalars
    n = 50L, N = 50L, k = 2L, K = 2L, p = 3L, P = 3L,
    m = 3L, M = 3L, J = 3L, d = 2L, D = 2L,
    alpha = 0.5, beta = 0.5, gamma = 0.5, lambda = 0.5,
    h = 1.0, bandwidth = 1.0, tol = 1e-6,
    fs = 100, order = 2L, low = 0.1, high = 0.4,
    seed = 1L, verbose = FALSE,
    # ts/event/data
    times = quote(sort(cumsum(rexp(50)))),
    event_times = quote(sort(cumsum(rexp(50)))),
    data = quote(data.frame(x = rnorm(50), y = rnorm(50))),
    df = quote(data.frame(x = rnorm(50), y = rnorm(50))),
    # spatial
    coords = quote(matrix(runif(100), 50, 2)),
    location = quote(matrix(runif(100), 50, 2)),
    # functional / categorical
    f = quote(function(z) sum(z^2)),
    formula = quote(stats::as.formula("y ~ x")),
    grid = quote(seq(-2, 2, length.out = 20)),
    party = quote(sample(c("A", "B"), 50, TRUE)),
    group = quote(sample(c("A", "B"), 50, TRUE)),
    treatment = quote(rbinom(50, 1, 0.5)),
    outcome = quote(rnorm(50)),
    # named-config defaults
    options = quote(NULL),
    setter_ideal = 0.5, reversion = 0.5,
    # extended heuristics (round 2) -------------------------------------
    name = "example", method = quote(NULL),
    markers = quote(matrix(sample(0:2, 200, TRUE), 50, 4)),
    y_true = quote(rbinom(50, 1, 0.5)),
    y_pred = quote(rbinom(50, 1, 0.5)),
    mu0 = 0, sigma0 = 1, theta0 = 0.5,
    mask = quote(NULL),
    pred = quote(rbinom(50, 1, 0.5)),
    truth = quote(rbinom(50, 1, 0.5)),
    actual = quote(rbinom(50, 1, 0.5)),
    cb = quote(function(z) sum(z)),
    fn = quote(function(z) sum(z^2)),
    obj = quote(function(p) sum(p^2)),
    # data-access / fetch (default to safe NULL or short string)
    db_path = quote(NULL), use_ckan = FALSE,
    dataset_key = "ocp21", resource_id = quote(NULL),
    limit = 10L, survey = "ccsd",
    # ml / cv
    cv = 3L, task = "regression", tune_grid = quote(NULL),
    deterministic_seed = TRUE, n_iter = 100L, burn = 20L,
    lam = 1.0, n_dim = 3L, batch_size = 10L,
    # scalar bounds
    t = quote(seq(0, 1, length.out = 50)),
    eps = 1e-6, threshold = 0.5, prob = 0.5, rate = 1.0,
    # categorical / labelled
    label = quote(sample(c("A", "B"), 50, TRUE)),
    # round 3 ------------------------------------------------------------
    rr = 2.5, rr_lower = 1.2,
    treated = quote(rnorm(30, 0.5)),
    control = quote(rnorm(30)),
    gamma_range = quote(c(1, 2)),
    preference_matrix = quote(matrix(rnorm(25), 5, 5)),
    w = quote(rnorm(3)), b = quote(rnorm(3)),
    stride = 1L, padding = 0L,
    y1 = quote(rnorm(100)), y2 = quote(rnorm(100)),
    max_lag = 4L,
    target = quote(rnorm(50)), # for cokrig
    sill_p = 1.0, range_p = 1.0, sill_s = 1.0, range_s = 1.0,
    cross_sill = 0.5, cross_range = 1.0, nugget = 0.1,
    groups = quote(list(rnorm(20), rnorm(20), rnorm(20))),
    control_index = 1L, adjust = "none",
    table_name = "demo", path = quote(tempfile(fileext = ".rds")),
    refresh = FALSE,
    key = "ocp21",
    T0 = quote(rep(0, 10)), dx = 0.1, dt = 0.01, n_steps = 5L,
    x0 = quote(rnorm(50)), betas = quote(rep(0.1, 5)),
    num_steps = 5L, noise = 0.1,
    activation = "relu", kind = "vanilla",
    D_real = quote(rnorm(20)), D_fake = quote(rnorm(20)),
    time = quote(cumsum(rexp(50))),
    event = quote(rbinom(50, 1, 0.8)),
    c = 1, lam0 = 0.5,
    instrument = quote(rbinom(50, 1, 0.5)),
    group_col = quote(NULL), propensity_col = quote(NULL),
    outcome_model = quote(NULL), instrument_col = "z",
    treatment_col = "t", outcome_col = "y",
    covariate_cols = quote(c("x1", "x2")),
    # power.t.test-style:
    delta = 0.5, sd = 1.0, sig_level = 0.05, power = quote(NULL),
    alternative = "two.sided", type = "two.sample",
    # other common
    trim = quote(c(0.01, 0.99)),
    NULL # fallback signals "no heuristic"
  )
}

# ---- function-name overrides (special multi-arg shapes) -------------------
# These take precedence over per-arg guess(). Returns a list of args or NULL.
# IMPORTANT: each arg expression MUST deparse to a single line. The apply
# step writes the deparsed string into a roxygen #' line; multi-statement
# {...} blocks would break R's parser. Use a single-expression
# data.frame() constructor with literal lengths instead of n <- 100.
SPECIAL <- list(
  .cpads_causal = function() list(
    data = quote(data.frame(
      t = stats::rbinom(100, 1, 0.4),
      y = stats::rbinom(100, 1, 0.3),
      x1 = stats::rnorm(100),
      x2 = stats::rnorm(100)
    )),
    treatment = "t", outcome = "y", covariates = c("x1", "x2")
  )
)
SPECIAL[["estimate_ate"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_att"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_atc"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_aipw"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_g_computation"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_gate"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_cate"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_irm"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_late"]] <- SPECIAL$.cpads_causal
SPECIAL[["estimate_propensity_scores"]] <- SPECIAL$.cpads_causal
SPECIAL[[".cpads_causal"]] <- NULL # remove the prototype

# error-case overrides — functions where the naive guess errored at runtime
SPECIAL[["contingency_coefficient"]] <- function() list(
  x = quote(matrix(sample(1:5, 50, TRUE), 10, 5))
)
SPECIAL[["dcc_multivariate_garch"]] <- function() list(
  x = quote(matrix(rnorm(150), 50, 3))
)
SPECIAL[["fzmrb"]] <- function() list(
  x = quote(rnorm(50)), t = quote(seq(0, 1, length.out = 50)), h = 0.1
)
SPECIAL[["bayesian_lasso_full"]] <- function() list(
  x = quote(matrix(rnorm(150), 50, 3)),
  y = quote(rnorm(50)),
  n_iter = 50L, burn = 10L, lam = 1.0, seed = 1L, deterministic_seed = TRUE
)
SPECIAL[["morie_grid_search_cv"]] <- function() list(
  x = quote(matrix(rnorm(150), 50, 3)),
  y = quote(rnorm(50)),
  method = "lm",
  tune_grid = quote(data.frame(intercept = c(TRUE, FALSE))),
  cv = 3L, task = "regression", seed = 1L
)
SPECIAL[["morie_power_t_test"]] <- function() list(
  n = 30L, delta = 0.5, sd = 1.0, sig_level = 0.05,
  power = quote(NULL), alternative = "two.sided", type = "two.sample"
)
SPECIAL[["morie_power_prop_test"]] <- function() list(
  n = 50L, p1 = 0.4, p2 = 0.5, sig_level = 0.05, power = quote(NULL)
)
SPECIAL[["estimate_propensity_scores"]] <- function() list(
  data = quote(data.frame(
    t = stats::rbinom(100, 1, 0.4),
    x1 = stats::rnorm(100), x2 = stats::rnorm(100)
  )),
  treatment = "t", covariates = c("x1", "x2"), trim = c(0.01, 0.99)
)
SPECIAL[["estimate_late"]] <- function() list(
  data = quote(data.frame(
    z = stats::rbinom(200, 1, 0.5),
    t = stats::rbinom(200, 1, 0.5),
    y = stats::rnorm(200),
    x1 = stats::rnorm(200)
  )),
  treatment = "t", outcome = "y", instrument = "z",
  covariates = c("x1")
)
SPECIAL[["estimate_gate"]] <- function() list(
  data = quote(data.frame(
    t = stats::rbinom(200, 1, 0.5),
    y = stats::rnorm(200),
    x1 = stats::rnorm(200),
    grp = sample(c("A", "B"), 200, TRUE)
  )),
  treatment = "t", outcome = "y", covariates = c("x1"),
  group_col = "grp"
)
# network-only functions are NOT handled by the generator. They're better
# served by \donttest{} -- handled separately in the manual cleanup pass
# (see tools/apply_donttest_for_network.R).

craft_args <- function(fname, fn) {
  # Special-case override: complete arg list for known multi-arg shapes
  if (!is.null(SPECIAL[[fname]])) return(SPECIAL[[fname]]())
  fmls <- as.list(formals(fn))
  out <- list()
  for (a in names(fmls)) {
    # Detect "no default" — the formal slot holds an empty name. Comparing
    # via identical() against quote(expr=) is fragile; use deparse() == ""
    # which is robust to all-of: empty symbol, "..." dots, normal symbols.
    if (a == "...") next # variadic: leave alone
    deparsed <- deparse(fmls[[a]], control = NULL)
    has_default <- !(length(deparsed) == 1 && deparsed == "")
    if (has_default) next # function fills it
    g <- guess(a)
    if (is.null(g)) return(NULL) # bail — heuristic doesn't cover this arg
    out[[a]] <- g
  }
  out
}

# ---- find candidate functions --------------------------------------------
exports <- ls("package:morie")
fn_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

# Map function name → source file
fn_to_file <- list()
def_rx <- "^([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*function"
for (f in fn_files) {
  s <- readLines(f, warn = FALSE)
  for (line in s) {
    m <- regmatches(line, regexec(def_rx, line))[[1]]
    if (length(m) >= 2 && m[2] %in% exports) {
      fn_to_file[[m[2]]] <- f
    }
  }
}

boilerplate_rx <- "See the package vignettes for usage examples"
boilerplate_fns <- character()
for (nm in names(fn_to_file)) {
  txt <- paste(readLines(fn_to_file[[nm]], warn = FALSE), collapse = "\n")
  # Look for any roxygen \dontrun block in the file containing the boilerplate
  if (grepl(boilerplate_rx, txt)) {
    boilerplate_fns <- c(boilerplate_fns, nm)
  }
}
cat(sprintf("found %d exported functions with boilerplate-dontrun in their source file\n",
            length(boilerplate_fns)))

# ---- probe each candidate ------------------------------------------------
results <- list()
for (i in seq_along(boilerplate_fns)) {
  nm <- boilerplate_fns[i]
  fn <- tryCatch(get(nm, envir = asNamespace("morie")), error = function(e) NULL)
  if (is.null(fn) || !is.function(fn)) {
    results[[nm]] <- list(status = "no-fn", call = NULL)
    next
  }
  args <- craft_args(nm, fn)
  if (is.null(args)) {
    results[[nm]] <- list(status = "no-heuristic", call = NULL)
    next
  }
  # build a deparse'd call for the example
  call_expr <- as.call(c(list(as.symbol(nm)), args))
  call_str <- paste(deparse(call_expr), collapse = " ")
  # probe in a fresh subprocess
  ok <- tryCatch({
    callr::r(
      function(nm, args) {
        suppressMessages(library(morie))
        set.seed(1)
        do.call(nm, args)
        TRUE
      },
      args = list(nm = nm, args = args),
      timeout = 5
    )
    "ok"
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("timeout|reached elapsed time limit", msg, ignore.case = TRUE)) "timeout" else "error"
  })
  results[[nm]] <- list(status = ok, call = call_str)
  if (i %% 20 == 0) cat(sprintf("  probed %d/%d (%s last)\n", i, length(boilerplate_fns), ok))
}

# ---- report --------------------------------------------------------------
status_counts <- table(vapply(results, function(r) r$status, character(1)))
cat("\n=== probe results ===\n")
print(status_counts)

ok_fns <- names(results)[vapply(results, function(r) r$status == "ok", logical(1))]
cat(sprintf("\nRunnable examples generated for %d functions.\n", length(ok_fns)))

# write a JSON map of fn -> example call for downstream apply step
out_path <- "tools/runnable_examples.json"
if (requireNamespace("jsonlite", quietly = TRUE)) {
  jsonlite::write_json(
    lapply(results, function(r) list(status = r$status, call = r$call %||% "")),
    out_path, auto_unbox = TRUE, pretty = TRUE
  )
  cat(sprintf("wrote %s\n", out_path))
}

# ---- apply --------------------------------------------------------------
if (APPLY) {
  cat("\n=== applying to source ===\n")
  applied <- 0
  for (nm in ok_fns) {
    fpath <- fn_to_file[[nm]]
    src <- readLines(fpath, warn = FALSE)
    # find the @examples block for `nm` and rewrite it
    # naive approach: rewrite EVERY boilerplate-dontrun in the file (file-level)
    new <- character()
    i <- 1L
    while (i <= length(src)) {
      ln <- src[i]
      if (grepl("^#' \\\\dontrun\\{\\s*$", ln) && i + 2 <= length(src) &&
          grepl(boilerplate_rx, src[i + 1])) {
        # boilerplate hit — replace 4 lines with the runnable call
        new <- c(new, sprintf("#' %s", results[[nm]]$call))
        i <- i + 4L
        applied <- applied + 1L
      } else {
        new <- c(new, ln); i <- i + 1L
      }
    }
    writeLines(new, fpath)
  }
  cat(sprintf("applied %d replacements\n", applied))
}
