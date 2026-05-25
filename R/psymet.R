# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie psymet -- Psychometric analysis (CTT, reliability, factor analysis).
#
# R port of src/morie/psymet.py. Delegates to package `psych` where available
# and hand-rolls fallbacks for Cronbach alpha and KMO using base R only.
#
# References:
#   Cronbach (1951). Psychometrika, 16(3), 297-334.
#   McDonald (1999). Test Theory: A Unified Treatment.
#   Revelle (2024). psych R package.

.has_psych <- function() requireNamespace("psych", quietly = TRUE)

.psych_or_stop <- function(fn) {
  if (!.has_psych()) {
    stop(sprintf(
      "morie_psymet_%s requires the 'psych' package. Install with: install.packages('psych')",
      fn
    ), call. = FALSE)
  }
}

.as_item_matrix <- function(data) {
  X <- as.matrix(data)
  storage.mode(X) <- "double"
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("i", seq_len(ncol(X)))
  }
  X
}

#' Cronbach's coefficient alpha with Feldt CI
#'
#' Hand-rolled base-R implementation. When the `psych` package is installed,
#' results agree with `psych::alpha()$total` to numerical precision.
#'
#' @param data Numeric matrix or data.frame: items as columns, respondents as rows.
#' @param ci Confidence level (default 0.95).
#' @return A list with components `raw`, `std`, `avgr`, `k`, `n`, `ci_lo`, `ci_hi`.
#' @export
morie_psymet_alpha <- function(data, ci = 0.95) {
  X <- .as_item_matrix(data)
  n <- nrow(X)
  k <- ncol(X)
  if (k < 2L) {
    return(list(raw = NA_real_, std = NA_real_, avgr = NA_real_,
                k = k, n = n, ci_lo = NA_real_, ci_hi = NA_real_))
  }
  item_var <- apply(X, 2, var)
  total_var <- var(rowSums(X))
  if (total_var < 1e-15) {
    return(list(raw = NA_real_, std = NA_real_, avgr = 0,
                k = k, n = n, ci_lo = NA_real_, ci_hi = NA_real_))
  }
  raw <- (k / (k - 1)) * (1 - sum(item_var) / total_var)
  R <- cor(X)
  off <- R[row(R) != col(R)]
  avgr <- mean(off)
  if (is.na(avgr)) avgr <- 0
  std <- (k * avgr) / (1 + (k - 1) * avgr)

  df1 <- n - 1
  df2 <- (n - 1) * (k - 1)
  f_lo <- qf(1 - (1 - ci) / 2, df1, df2)
  f_hi <- qf((1 - ci) / 2, df1, df2)
  list(
    raw = as.numeric(raw), std = as.numeric(std), avgr = as.numeric(avgr),
    k = k, n = n,
    ci_lo = as.numeric(1 - (1 - raw) * f_lo),
    ci_hi = as.numeric(1 - (1 - raw) * f_hi)
  )
}

#' McDonald's omega total and hierarchical
#'
#' Delegates to `psych::omega()` when available; otherwise uses a single-factor
#' principal-axis approximation.
#'
#' @param data Numeric matrix / data.frame of items.
#' @param nf Number of factors (default 1).
#' @return list with `total`, `hier`, `alpha`, `nf`, `expvar`.
#' @export
morie_psymet_omega <- function(data, nf = 1) {
  X <- .as_item_matrix(data)
  if (.has_psych()) {
    res <- tryCatch(
      psych::omega(X, nfactors = nf, plot = FALSE, flip = FALSE),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(list(
        total = as.numeric(res$omega.tot),
        hier = as.numeric(res$omega_h),
        alpha = as.numeric(res$alpha),
        nf = nf,
        expvar = NA_real_
      ))
    }
  }
  # Fallback: PCA-style approximation matching the Python module.
  R <- cor(X)
  eig <- eigen(R, symmetric = TRUE)
  evals <- eig$values
  evecs <- eig$vectors
  loads <- evecs[, seq_len(nf), drop = FALSE] *
    matrix(sqrt(pmax(evals[seq_len(nf)], 0)), nrow = nrow(evecs),
           ncol = nf, byrow = TRUE)
  comm <- rowSums(loads^2)
  uniq <- 1 - comm
  omg_t <- 1 - sum(uniq) / sum(R)
  omg_h <- (sum(loads[, 1]))^2 / sum(R)
  a <- morie_psymet_alpha(X)$raw
  list(
    total = max(0, min(1, omg_t)),
    hier = max(0, min(1, omg_h)),
    alpha = a,
    nf = nf,
    expvar = sum(evals[seq_len(nf)]) / sum(evals)
  )
}

#' Corrected item-total correlations
#'
#' @param data Numeric matrix / data.frame.
#' @return data.frame with columns `item`, `r_total`, `r_corr`.
#' @export
morie_psymet_itemtotal <- function(data) {
  X <- .as_item_matrix(data)
  total <- rowSums(X)
  nm <- colnames(X)
  out <- data.frame(
    item = nm,
    r_total = vapply(seq_len(ncol(X)), function(j) cor(X[, j], total), numeric(1)),
    r_corr = vapply(seq_len(ncol(X)), function(j) cor(X[, j], total - X[, j]), numeric(1))
  )
  out
}

#' Alpha if item deleted
#'
#' @param data Numeric matrix / data.frame.
#' @return data.frame with `item`, `adel`.
#' @export
morie_psymet_alphadel <- function(data) {
  X <- .as_item_matrix(data)
  nm <- colnames(X)
  out <- data.frame(
    item = nm,
    adel = vapply(seq_len(ncol(X)),
                  function(j) morie_psymet_alpha(X[, -j, drop = FALSE])$raw,
                  numeric(1))
  )
  out
}

#' Composite reliability from standardized factor loadings.
#' CR = (sum lambda)^2 / ((sum lambda)^2 + sum(1 - lambda^2))
#' @export
morie_psymet_cr <- function(loads) {
  lam <- as.numeric(loads)
  sl <- sum(lam)
  se <- sum(1 - lam^2)
  sl^2 / (sl^2 + se)
}

#' Average variance extracted (AVE) from factor loadings. Mean(lambda^2).
#' @export
morie_psymet_ave <- function(loads) {
  mean(as.numeric(loads)^2)
}

#' Kaiser-Meyer-Olkin sampling adequacy.
#'
#' Delegates to `psych::KMO()` when available; otherwise computes from the
#' partial-correlation anti-image matrix using base R.
#' @return list with `msa` (overall) and named numeric vector `items`.
#' @export
morie_psymet_kmo <- function(data) {
  X <- .as_item_matrix(data)
  if (.has_psych()) {
    res <- tryCatch(psych::KMO(cor(X)), error = function(e) NULL)
    if (!is.null(res)) {
      return(list(msa = as.numeric(res$MSA),
                  items = setNames(as.numeric(res$MSAi), colnames(X))))
    }
  }
  R <- cor(X)
  k <- ncol(R)
  Ri <- tryCatch(solve(R), error = function(e) MASS::ginv(R))
  D <- diag(1 / sqrt(diag(Ri)))
  Q <- -D %*% Ri %*% D
  diag(Q) <- 1
  off <- row(R) != col(R)
  sr2 <- sum(R[off]^2)
  sq2 <- sum(Q[off]^2)
  overall <- sr2 / (sr2 + sq2)
  items <- vapply(seq_len(k), function(j) {
    mj <- off[j, ]
    r2 <- sum(R[j, mj]^2)
    q2 <- sum(Q[j, mj]^2)
    if ((r2 + q2) > 0) r2 / (r2 + q2) else 0
  }, numeric(1))
  names(items) <- colnames(X)
  list(msa = as.numeric(overall), items = items)
}

#' Bartlett's test of sphericity.
#' @return list with `chisq`, `df`, `pval`.
#' @export
morie_psymet_bartlett <- function(data) {
  X <- .as_item_matrix(data)
  n <- nrow(X)
  k <- ncol(X)
  R <- cor(X)
  det_R <- max(det(R), 1e-15)
  chisq <- -(n - 1 - (2 * k + 5) / 6) * log(det_R)
  df <- k * (k - 1) %/% 2
  list(chisq = as.numeric(chisq), df = df,
       pval = as.numeric(pchisq(chisq, df, lower.tail = FALSE)))
}

#' Horn's parallel analysis -- suggested number of factors.
#'
#' Delegates to `psych::fa.parallel()` when available; otherwise compares
#' observed eigenvalues to the 95th percentile of random-data eigenvalues.
#' @export
morie_psymet_parallel <- function(data, nsim = 100, seed = 42) {
  X <- .as_item_matrix(data)
  if (.has_psych()) {
    res <- tryCatch(
      psych::fa.parallel(X, n.iter = nsim, plot = FALSE, fa = "pc",
                         quant = 0.95, error.bars = FALSE),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      return(max(as.integer(res$ncomp), 1L))
    }
  }
  set.seed(seed)
  n <- nrow(X)
  k <- ncol(X)
  obs <- sort(eigen(cor(X), symmetric = TRUE, only.values = TRUE)$values,
              decreasing = TRUE)
  sim <- matrix(0, nrow = nsim, ncol = k)
  for (i in seq_len(nsim)) {
    sim[i, ] <- sort(eigen(cor(matrix(rnorm(n * k), n, k)),
                            symmetric = TRUE, only.values = TRUE)$values,
                     decreasing = TRUE)
  }
  thresh <- apply(sim, 2, quantile, probs = 0.95)
  max(sum(obs > thresh), 1L)
}

#' Spearman-Brown split-half reliability.
#' @param method "first_last" or "odd_even".
#' @export
morie_psymet_splithalf <- function(data, method = c("first_last", "odd_even")) {
  method <- match.arg(method)
  X <- .as_item_matrix(data)
  k <- ncol(X)
  if (method == "odd_even") {
    idx_a <- seq(1, k, by = 2)
    idx_b <- seq(2, k, by = 2)
  } else {
    mid <- k %/% 2
    idx_a <- seq_len(mid)
    idx_b <- seq.int(mid + 1, k)
  }
  h1 <- rowSums(X[, idx_a, drop = FALSE])
  h2 <- rowSums(X[, idx_b, drop = FALSE])
  r <- cor(h1, h2)
  2 * r / (1 + r)
}

#' Item discrimination (D-statistic).
#'
#' Upper/lower groups by total score (default 27% per Kelley).
#' @return data.frame with `item`, `d`.
#' @export
morie_psymet_discrimination <- function(data, pct = 0.27) {
  X <- .as_item_matrix(data)
  n <- nrow(X)
  total <- rowSums(X)
  cut <- max(as.integer(n * pct), 1L)
  si <- order(total)
  lo <- si[seq_len(cut)]
  hi <- si[seq.int(length(si) - cut + 1, length(si))]
  d <- vapply(seq_len(ncol(X)), function(j) {
    mx <- max(X[, j])
    if (mx <= 0) return(0)
    mean(X[hi, j]) / mx - mean(X[lo, j]) / mx
  }, numeric(1))
  data.frame(item = colnames(X), d = d)
}
