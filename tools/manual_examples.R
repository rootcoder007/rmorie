# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/manual_examples.R
#
# Hand-crafted runnable @example for each of the 62 exported functions
# whose argument signature defeated the formals()-based heuristic in
# tools/generate_runnable_examples.R. Each example is probed in a 5-s
# subprocess; passing examples replace the placeholder
# `# See the package vignettes for usage examples` comment in source.
#
# Usage:  Rscript tools/manual_examples.R [--apply]

suppressMessages({
  library(morie)
  library(callr)
})

APPLY <- "--apply" %in% commandArgs(trailingOnly = TRUE)

# Each entry: function-name -> a one-or-more-line example.
# Examples are written as plain R code; the apply step splits on \n
# and prefixes each line with `#' `.
EX <- list(
  # ---- statistical inference -------------------------------------------
  morie_paired_t_test = "set.seed(1); morie_paired_t_test(rnorm(20), rnorm(20, 0.3))",
  chi_square_test     = "chi_square_test(observed = c(10, 20, 30), expected = c(15, 20, 25))",
  fisher_exact_test   = "fisher_exact_test(matrix(c(8, 2, 1, 5), 2, 2))",
  morie_kruskal_wallis_test = "set.seed(1); morie_kruskal_wallis_test(rnorm(20), rnorm(20, 0.5), rnorm(20, 1))",
  morie_mann_whitney_test = "set.seed(1); morie_mann_whitney_test(rnorm(30), rnorm(30, 0.5))",
  wilcoxon_signed_rank_test = "set.seed(1); wilcoxon_signed_rank_test(rnorm(20), rnorm(20, 0.4))",
  morie_levene_test   = "set.seed(1); morie_levene_test(rnorm(30), rnorm(30, sd = 2))",
  odds_ratio_ci       = "odds_ratio_ci(matrix(c(15, 5, 8, 12), 2, 2))",
  risk_ratio_ci       = "risk_ratio_ci(matrix(c(15, 5, 8, 12), 2, 2))",
  risk_difference_ci  = "risk_difference_ci(matrix(c(15, 5, 8, 12), 2, 2))",
  morie_cohens_d      = "set.seed(1); morie_cohens_d(rnorm(30), rnorm(30, 0.5))",
  morie_hedges_g      = "set.seed(1); morie_hedges_g(rnorm(30), rnorm(30, 0.5))",
  morie_eta_squared   = "morie_eta_squared(f_stat = 5.2, df_between = 2, df_within = 27)",
  morie_cramers_v     = "morie_cramers_v(matrix(c(10, 5, 4, 11), 2, 2))",
  point_biserial_r    = "set.seed(1); point_biserial_r(rbinom(50, 1, 0.5), rnorm(50))",
  sample_size_logistic = "sample_size_logistic(p0 = 0.3, or = 2, alpha = 0.05, power = 0.8)",

  # ---- sampling --------------------------------------------------------
  morie_cluster_sample = paste(sep = "\n",
    "set.seed(1)",
    'df <- data.frame(id = 1:200, cluster = rep(letters[1:10], each = 20), x = rnorm(200))',
    'morie_cluster_sample(df, cluster_col = "cluster", n_clusters = 3)'),
  morie_pps_sample = paste(sep = "\n",
    "set.seed(1)",
    'df <- data.frame(id = 1:50, size = runif(50, 1, 10))',
    'morie_pps_sample(df, size_col = "size", n = 10)'),
  jackknife_estimate = paste(sep = "\n",
    "set.seed(1)",
    'df <- data.frame(x = rnorm(50))',
    'jackknife_estimate(df, statistic = function(d) mean(d$x))'),
  morie_effective_sample_size = "morie_effective_sample_size(c(1, 1, 2, 2, 3, 3, 4, 4))",
  morie_design_effect = "morie_design_effect(c(1, 1, 2, 2, 3, 3, 4, 4))",
  compute_design_weights = paste(sep = "\n",
    'df <- data.frame(stratum = sample(c("A", "B"), 100, TRUE))',
    'compute_design_weights(df, strata_col = "stratum",',
    '  population_sizes = c(A = 500, B = 800))'),

  # ---- spatial / time-series -------------------------------------------
  johansen_cointegration = "set.seed(1); johansen_cointegration(matrix(cumsum(rnorm(200)), 100, 2), k_ar_diff = 1)",
  midas_regression = paste(sep = "\n",
    "set.seed(1)",
    'midas_regression(x = matrix(rnorm(60), 15, 4), y = rnorm(15), K = 4)'),
  sarla = paste(sep = "\n",
    "set.seed(1); n <- 30",
    'w <- matrix(0, n, n); for (i in 1:(n - 1)) w[i, i + 1] <- w[i + 1, i] <- 1',
    'sarla(x = matrix(rnorm(n * 2), n, 2), y = rnorm(n), w = w)'),
  sarre = paste(sep = "\n",
    "set.seed(1); n <- 30",
    'w <- matrix(0, n, n); for (i in 1:(n - 1)) w[i, i + 1] <- w[i + 1, i] <- 1',
    'sarre(x = matrix(rnorm(n * 2), n, 2), y = rnorm(n), w = w)'),
  sptau = paste(sep = "\n",
    "set.seed(1); n <- 20",
    'w <- matrix(0, n, n); for (i in 1:(n - 1)) w[i, i + 1] <- w[i + 1, i] <- 1',
    'sptau(x = rnorm(n), w = w)'),

  # ---- ML / neural -----------------------------------------------------
  diffu_diffusion_forward = paste(sep = "\n",
    "set.seed(1)",
    'diffu_diffusion_forward(x0 = rnorm(20), t = 5, betas = rep(0.01, 100),',
    '  num_steps = 100, noise = 1, seed = 1)'),
  gxe_interaction_model = paste(sep = "\n",
    "set.seed(1); n <- 60",
    'gxe_interaction_model(x = rnorm(n), y = rnorm(n), env = rbinom(n, 1, 0.5))'),
  heinz_he_initialization = "heinz_he_initialization(fan_in = 20, fan_out = 10, seed = 1)",
  mhatf_multi_head_attention_full = paste(sep = "\n",
    "set.seed(1)",
    'mhatf_multi_head_attention_full(x = matrix(rnorm(64), 8, 8), num_heads = 2,',
    '  W_q = matrix(rnorm(64), 8, 8), W_k = matrix(rnorm(64), 8, 8),',
    '  W_v = matrix(rnorm(64), 8, 8), W_o = matrix(rnorm(64), 8, 8))'),
  mxpol_maxpool_forward = "mxpol_maxpool_forward(matrix(rnorm(16), 4, 4), kernel_size = 2L, stride = 2L)",
  posab_positional_encoding_abs = "posab_positional_encoding_abs(seq_len = 10L, d_model = 8L)",
  rotrp_rotary_position_embedding = "rotrp_rotary_position_embedding(matrix(rnorm(16), 4, 4))",
  trfbl_transformer_block = "set.seed(1); trfbl_transformer_block(matrix(rnorm(32), 4, 8), num_heads = 2L, d_ff = 16L)",
  tsne_reduction = paste(sep = "\n",
    "set.seed(1)",
    'tsne_reduction(matrix(rnorm(60), 20, 3), n_components = 2L, perplexity = 5,',
    '  n_iter = 100L, seed = 1L)'),
  vaenc_vae_elbo = paste(sep = "\n",
    "set.seed(1)",
    'vaenc_vae_elbo(x = rnorm(20), x_recon = rnorm(20),',
    '  mu = rnorm(20), log_var = rnorm(20))'),
  xavir_xavier_init = "xavir_xavier_init(fan_in = 20L, fan_out = 10L, seed = 1L)",

  # ---- misc ------------------------------------------------------------
  agset = "agset(options = c('A', 'B', 'C'), setter_ideal = 0.5, reversion = 0.5)",
  fzmrb = "set.seed(1); fzmrb(x = rnorm(30), t = seq(0, 1, length.out = 30), h = 0.1)",
  idlpt = "set.seed(1); idlpt(X_r = matrix(rnorm(60), 20, 3), X_s = matrix(rnorm(60), 20, 3))",
  penalized_regression = "set.seed(1); penalized_regression(x = matrix(rnorm(60), 20, 3), y = rnorm(20), alpha = 0.5, lam = 0.1)",
  morie_build_prompt = "morie_build_prompt(question = 'What is the mean?', context = 'CPADS data summary')",
  regularization_path = "set.seed(1); regularization_path(x = matrix(rnorm(60), 20, 3), y = rnorm(20), penalty = 'lasso')",
  random_search_cv = paste(sep = "\n",
    "set.seed(1)",
    'random_search_cv(x = matrix(rnorm(60), 20, 3), y = rnorm(20), method = "lm",',
    '  n_iter = 5L, cv = 3L, task = "regression", seed = 1L)'),
  roc_auc_score = "set.seed(1); roc_auc_score(y_true = rbinom(50, 1, 0.5), y_score = runif(50))",
  svm_hinge_primal = "set.seed(1); svm_hinge_primal(x = matrix(rnorm(60), 30, 2), y = sample(c(-1, 1), 30, TRUE), C = 1)",
  unfdl = "set.seed(1); unfdl(x = matrix(rnorm(60), 20, 3), k = 2L)",

  # ---- longitudinal_sim -----------------------------------------------
  morie_generate_ar_coefficients = "morie_generate_ar_coefficients(p = 2L)",
  morie_generate_var_coefficients = "morie_generate_var_coefficients(p = 2L, lags = 1L)",
  morie_mvn_with_covariance = "morie_mvn_with_covariance(n = 30L, p = 3L)",

  # ---- workflow / data --------------------------------------------------
  validate_cpads_data = paste(sep = "\n",
    "set.seed(1); n <- 50",
    'df <- data.frame(',
    '  weight = runif(n, 0.5, 2),',
    '  alcohol_past12m = rbinom(n, 1, 0.8),',
    '  heavy_drinking_30d = rbinom(n, 1, 0.3),',
    '  ebac_tot = abs(rnorm(n, 0.05, 0.03)),',
    '  ebac_legal = rbinom(n, 1, 0.7),',
    '  cannabis_any_use = rbinom(n, 1, 0.3),',
    '  age_group = sample(1:6, n, TRUE),',
    '  gender = sample(1:2, n, TRUE),',
    '  province_region = sample(1:5, n, TRUE),',
    '  mental_health = sample(1:5, n, TRUE),',
    '  physical_health = sample(1:5, n, TRUE)',
    ')',
    'validate_cpads_data(df)'),
  canonicalize_cpads_data = paste(sep = "\n",
    "set.seed(1); n <- 50",
    'raw <- data.frame(',
    '  wtpumf = runif(n, 0.5, 2),',
    '  alc05 = sample(c(1, 2), n, TRUE),',
    '  alc12_30d_prev_total = rbinom(n, 1, 0.3),',
    '  alc12_30d_prev = rbinom(n, 1, 0.3),',
    '  can05 = sample(c(1, 2), n, TRUE),',
    '  age_groups = sample(1:6, n, TRUE),',
    '  dvdemq01 = sample(c(1, 2), n, TRUE),',
    '  region = sample(1:5, n, TRUE),',
    '  hwbq01 = sample(1:5, n, TRUE),',
    '  hwbq02 = sample(1:5, n, TRUE),',
    '  ebac_tot = abs(rnorm(n, 0.05, 0.03)),',
    '  ebac_legal = rbinom(n, 1, 0.7)',
    ')',
    'head(canonicalize_cpads_data(raw))'),
  validate_outputs_manifest = paste(sep = "\n",
    'man <- data.frame(',
    '  output = "results.csv", public_path = "data/manifest/outputs/results.csv",',
    '  size_kb = 0.01, modified = "2026-05-20"',
    ')',
    'validate_outputs_manifest(man)'),
  summarize_output_audit = paste(sep = "\n",
    'audit <- data.frame(',
    '  output = c("a.csv", "b.csv"), declared = c(TRUE, TRUE),',
    '  exists = c(TRUE, FALSE), size_diff_kb = c(0, NA)',
    ')',
    'summarize_output_audit(audit)')
)

cat(sprintf("hand-crafted examples for %d functions\n", length(EX)))

# Probe each
results <- list()
for (i in seq_along(EX)) {
  fn <- names(EX)[i]
  code <- EX[[i]]
  ok <- tryCatch({
    callr::r(
      function(src) {
        suppressMessages(library(morie)); set.seed(1)
        eval(parse(text = src), envir = new.env())
        TRUE
      },
      args = list(src = code), timeout = 8
    )
    "ok"
  }, error = function(e) {
    msg <- conditionMessage(e)
    cat(sprintf("  FAIL %s: %s\n", fn, substr(msg, 1, 150)))
    "error"
  })
  results[[fn]] <- list(status = ok, code = code)
  if (i %% 10 == 0) cat(sprintf("  probed %d/%d\n", i, length(EX)))
}

n_ok <- sum(vapply(results, function(r) r$status == "ok", logical(1)))
cat(sprintf("\n%d / %d examples pass\n", n_ok, length(EX)))

# Apply
if (APPLY) {
  cat("\n=== applying ===\n")
  # Map fn -> source file
  fn_to_file <- list()
  def_rx <- "^([A-Za-z_.][A-Za-z0-9_.]*)[[:space:]]*<-[[:space:]]*function"
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
    for (line in readLines(f, warn = FALSE)) {
      m <- regmatches(line, regexec(def_rx, line))[[1]]
      if (length(m) >= 2 && is.null(fn_to_file[[m[2]]])) {
        fn_to_file[[m[2]]] <- f
      }
    }
  }

  applied <- 0L
  for (fn in names(results)) {
    if (results[[fn]]$status != "ok") next
    fp <- fn_to_file[[fn]]
    if (is.null(fp)) next
    src <- readLines(fp, warn = FALSE)
    # Find the `#' # See the package vignettes` line whose NEXT function
    # definition matches our fn. Replace it (and the 2nd boilerplate line
    # `#'   vignette(package = "morie")`) with the crafted example lines.
    for (i in seq_along(src)) {
      if (grepl("# See the package vignettes for usage", src[i], fixed = TRUE)) {
        # find next function def
        next_fn <- NULL
        for (j in (i + 1L):min(i + 15L, length(src))) {
          m <- regmatches(src[j], regexec(def_rx, src[j]))[[1]]
          if (length(m) >= 2) { next_fn <- m[2]; break }
        }
        if (identical(next_fn, fn)) {
          # The placeholder is 2 lines: `#' # See...` and `#' #   vignette(...)`
          new_lines <- paste0("#' ", strsplit(results[[fn]]$code, "\n", fixed = TRUE)[[1]])
          src <- c(src[seq_len(i - 1L)], new_lines, src[seq.int(i + 2L, length(src))])
          applied <- applied + 1L
          break
        }
      }
    }
    writeLines(src, fp)
  }
  cat(sprintf("applied %d real examples\n", applied))
}
