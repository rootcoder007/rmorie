# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native genetic matching (feat/native-specializations, module 6).
# Diamond & Sekhon (2013): search over a diagonal weight matrix W for
# weighted-Mahalanobis matching, maximizing the worst covariate
# balance across matched pairs. The GA is real-coded on log-weights;
# every fitness evaluation is one run of the module-2 streaming kd
# kernel, so the loop stays fast without Matching/rgenoud.

# One weighted matching: scale whitened dims by sqrt(w), match 1:1.
#' Internal helper: weighted match for a candidate weight vector
#' @noRd
.morie_match_genetic_eval <- function(Wt, Wc, w) {
  sw <- sqrt(w)
  mm <- .morie_match_greedy_kd_cpp(
    Wt * rep(sw, each = nrow(Wt)),
    Wc * rep(sw, each = nrow(Wc)),
    1L, Inf, FALSE
  )
  as.integer(mm[, 1L])
}

# Fitness: minimum across covariates of the paired-t balance p-value
# (higher = better worst-case balance, the GenMatch loss).
#' Internal helper: worst-covariate balance fitness
#' @noRd
.morie_match_genetic_fitness <- function(Xt, Xc, mm) {
  ok <- !is.na(mm)
  if (!any(ok)) return(-Inf)
  pmin_val <- Inf
  mc <- mm[ok]
  nok <- length(mc)
  for (j in seq_len(ncol(Xt))) {
    dlt <- Xt[ok, j] - Xc[mc, j]
    s <- stats::sd(dlt)
    if (s < 1e-12) next
    # One-sample paired-t p-value in closed form -- identical to
    # stats::t.test(dlt)$p.value but without building the htest object,
    # which dominates the GA inner loop (pop_size x n_gen x n_cov calls).
    tstat <- mean(dlt) / (s / sqrt(nok))
    p <- 2 * stats::pt(-abs(tstat), nok - 1L)
    if (p < pmin_val) pmin_val <- p
  }
  if (is.infinite(pmin_val)) 1 else pmin_val
}

#' Internal helper: native genetic matching engine
#' @srrstats {G1.0} Primary reference: Diamond & Sekhon (2013, REStat
#'   95(3)) — genetic search over the Mahalanobis weight matrix
#'   maximizing worst-case covariate balance.
#' @srrstats {G3.0} Fitness comparisons operate on p-values with
#'   explicit degenerate-variance guards; no exact FP equality.
#' @noRd
.morie_match_genetic_native <- function(data, treatment, covariates,
                                        n_neighbors = 1L,
                                        pop_size = 50L,
                                        n_generations = 20L,
                                        seed = 42L) {
  df <- .morie_matching_drop_na(data, c(treatment, covariates))
  tr <- df[[treatment]] == 1
  idx_t <- which(tr)
  idx_c <- which(!tr)
  X <- as.matrix(df[, covariates, drop = FALSE])
  W <- .morie_match_whiten(X, X[idx_c, , drop = FALSE])
  Wt <- W[idx_t, , drop = FALSE]
  Wc <- W[idx_c, , drop = FALSE]
  Xt <- X[idx_t, , drop = FALSE]
  Xc <- X[idx_c, , drop = FALSE]
  k <- ncol(X)

  set.seed(seed)
  # real-coded GA on log10-weights in [-2, 2]; row 1 = equal weights
  # (plain Mahalanobis) so the search can never do worse than module 2
  pop <- matrix(stats::runif(pop_size * k, -2, 2), nrow = pop_size)
  pop[1L, ] <- 0
  fit <- vapply(seq_len(pop_size), function(i) {
    .morie_match_genetic_fitness(
      Xt, Xc, .morie_match_genetic_eval(Wt, Wc, 10^pop[i, ]))
  }, numeric(1))

  for (g in seq_len(n_generations)) {
    ord <- order(fit, decreasing = TRUE)
    elite <- pop[ord[seq_len(max(2L, pop_size %/% 5L))], , drop = FALSE]
    child <- matrix(0, nrow = pop_size, ncol = k)
    child[seq_len(nrow(elite)), ] <- elite
    for (i in (nrow(elite) + 1L):pop_size) {
      pa <- elite[sample.int(nrow(elite), 2L), , drop = FALSE]
      a <- stats::runif(k)
      kid <- a * pa[1L, ] + (1 - a) * pa[2L, ]           # blend crossover
      mut <- stats::runif(k) < 0.2
      kid[mut] <- pmin(2, pmax(-2, kid[mut] + stats::rnorm(sum(mut), 0, 0.3)))
      child[i, ] <- kid
    }
    pop <- child
    fit <- vapply(seq_len(pop_size), function(i) {
      .morie_match_genetic_fitness(
        Xt, Xc, .morie_match_genetic_eval(Wt, Wc, 10^pop[i, ]))
    }, numeric(1))
  }

  best <- which.max(fit)
  w_best <- 10^pop[best, ]
  sw <- sqrt(w_best)
  # final match honours n_neighbors (GA fitness is evaluated 1:1)
  mm_full <- .morie_match_greedy_kd_cpp(
    Wt * rep(sw, each = nrow(Wt)),
    Wc * rep(sw, each = nrow(Wc)),
    as.integer(n_neighbors), Inf, FALSE
  )
  rn <- rownames(df)
  if (is.null(rn)) rn <- as.character(seq_len(nrow(df)))
  ti_rep <- rep(seq_len(nrow(mm_full)), times = ncol(mm_full))
  ci_all <- as.vector(mm_full)
  okp <- !is.na(ci_all)
  ti_ok <- ti_rep[okp]
  ci_ok <- ci_all[okp]
  dvec <- sqrt(rowSums(((Wt[ti_ok, , drop = FALSE] -
                           Wc[ci_ok, , drop = FALSE]) *
                          rep(sw, each = length(ti_ok)))^2))
  pairs_df <- if (length(ti_ok)) data.frame(
    treated_idx = rn[idx_t[ti_ok]],
    control_idx = rn[idx_c[ci_ok]],
    distance    = dvec,
    stringsAsFactors = FALSE
  ) else .morie_matching_empty_pairs()
  ok <- !is.na(mm_full[, 1L])
  keep <- sort(unique(c(idx_t[unique(ti_ok)], idx_c[ci_ok])))
  .morie_matching_result(
    matched_data      = df[keep, , drop = FALSE],
    n_treated         = length(unique(ti_ok)),
    n_matched_control = length(unique(ci_ok)),
    match_pairs       = pairs_df,
    method            = "genetic (rmorie native)",
    details           = list(
      engine        = "native-genetic-ga",
      best_weights  = stats::setNames(w_best, covariates),
      best_fitness  = fit[best],
      pop_size      = pop_size,
      n_generations = n_generations
    )
  )
}
