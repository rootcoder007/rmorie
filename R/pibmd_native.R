# R arm of pibmd -- prior-data conflict and prior informativeness from
# posterior and prior draws. Evans, M. & Moshonov, H. (2006) Bayesian
# Analysis 1(4), 893-914; Kullback, S. & Leibler, R. A. (1951) Ann. Math.
# Statist. 22(1), 79-86; Silverman, B. W. (1986) Density Estimation for
# Statistics and Data Analysis, Sec. 3.4.
# Mirrors src/morie/fn/pibmd.py.

.pibmd_EPS <- 1e-12

.pibmd_moments <- function(v) {
  n <- length(v)
  m <- sum(v) / n
  s2 <- sum((v - m) ^ 2) / max(n - 1L, 1L)
  c(m, s2)
}

# KL(q || p) for two normals -- exact when both are normal.
.pibmd_kl_gaussian <- function(mq, sq2, mp, sp2) {
  sp2 <- max(sp2, 1e-300); sq2 <- max(sq2, 1e-300)
  0.5 * log(sp2 / sq2) + (sq2 + (mq - mp) ^ 2) / (2.0 * sp2) - 0.5
}

# Type-7 quantile of an already-sorted vector.
.pibmd_quantile <- function(sorted_v, u) {
  n <- length(sorted_v)
  if (n == 1L) return(sorted_v[1])
  h <- (n - 1L) * u
  lo <- floor(h)
  hi <- min(lo + 1, n - 1)
  sorted_v[lo + 1L] + (h - lo) * (sorted_v[hi + 1L] - sorted_v[lo + 1L])
}

# Silverman's rule of thumb, robustified by the interquartile range.
.pibmd_bandwidth <- function(v) {
  n <- length(v)
  s <- sort(v)
  sd_ <- sqrt(max(.pibmd_moments(v)[2], 0.0))
  iqr <- .pibmd_quantile(s, 0.75) - .pibmd_quantile(s, 0.25)
  a <- if (iqr > 0.0) min(sd_, iqr / 1.34) else sd_
  if (a <= 0.0) a <- max(sd_, 1e-8)
  0.9 * a * n ^ (-0.2)
}

.pibmd_kde <- function(x, sample_, h) {
  cst <- 1.0 / (length(sample_) * h * sqrt(2.0 * pi))
  z <- (x - sample_) / h
  cst * sum(exp(-0.5 * z[abs(z) < 38.0] ^ 2))
}

# KL(q || p) from Gaussian kernel densities integrated on a grid.
.pibmd_kl_kde <- function(q, p, n_grid) {
  n <- length(q); m <- length(p)
  if (n < 2L || m < 2L) return(NaN)
  hq <- .pibmd_bandwidth(q); hp <- .pibmd_bandwidth(p)
  lo <- min(min(q) - 4.0 * hq, min(p) - 4.0 * hp)
  hi <- max(max(q) + 4.0 * hq, max(p) + 4.0 * hp)
  if (hi - lo <= .pibmd_EPS) return(0.0)
  dx <- (hi - lo) / n_grid
  xs <- lo + (seq_len(n_grid) - 0.5) * dx
  fq <- vapply(xs, function(x) .pibmd_kde(x, q, hq), 0)
  fp <- vapply(xs, function(x) .pibmd_kde(x, p, hp), 0)
  # renormalise on the grid so truncation does not masquerade as
  # divergence -- the two densities must each integrate to one here
  zq <- sum(fq) * dx; zp <- sum(fp) * dx
  if (zq <= .pibmd_EPS || zp <= .pibmd_EPS) return(NaN)
  a <- fq / zq; b <- fp / zp
  keep <- a > 1e-300
  sum(a[keep] * log(a[keep] / pmax(b[keep], 1e-300)) * dx)
}

.pibmd_ecdf <- function(sorted_v, x) sum(sorted_v <= x) / length(sorted_v)

morie_pibmd_prior_informativeness_bias_diagnostic <- function(samples, prior,
                                                              n_grid = 512L) {
  q <- as.numeric(samples)
  nq <- length(q)
  if (nq < 2L)
    stop(paste0("pibmd: at least two posterior draws are needed to estimate ",
                "a variance"))

  moments_only <- FALSE
  if (is.list(prior) && !is.null(names(prior)) &&
      all(c("mean", "sd") %in% names(prior))) {
    mp <- as.numeric(prior$mean); sdp <- as.numeric(prior$sd)
    if (sdp <= 0.0)
      stop("pibmd: the prior standard deviation must be positive")
    sp2 <- sdp * sdp
    p <- numeric(0)
    moments_only <- TRUE
  } else if (is.list(prior) && !is.null(names(prior))) {
    stop("pibmd: a mapping prior must give 'mean' and 'sd'")
  } else {
    p <- as.numeric(prior)
    if (length(p) < 2L)
      stop(paste0("pibmd: at least two prior draws are needed -- pass ",
                  "list(mean = ..., sd = ...) for a normal prior known only ",
                  "by its moments"))
    mm <- .pibmd_moments(p); mp <- mm[1]; sp2 <- mm[2]
  }

  mq2 <- .pibmd_moments(q); mq <- mq2[1]; sq2 <- mq2[2]
  if (sp2 <= .pibmd_EPS)
    stop(paste0("pibmd: the prior has no spread, so every divergence from ",
                "it is infinite"))

  kl_g <- .pibmd_kl_gaussian(mq, sq2, mp, sp2)
  kl_kde <- if (moments_only) NaN else
    .pibmd_kl_kde(q, p, as.integer(n_grid))
  # the reverse divergence: KL is not symmetric, and the prior-to-posterior
  # direction is the one that blows up when the posterior has mass where the
  # prior has none
  kl_rev <- .pibmd_kl_gaussian(mp, sp2, mq, sq2)
  sym <- 0.5 * (kl_g + kl_rev)

  shrink <- 1.0 - sq2 / sp2
  bias <- (mq - mp) / sqrt(sp2)

  z <- bias
  pval <- 2.0 * min(pnorm(z), 1.0 - pnorm(z))
  if (moments_only) {
    pval_emp <- NaN
    wass <- NaN
  } else {
    ps <- sort(p)
    f <- .pibmd_ecdf(ps, mq)
    pval_emp <- 2.0 * min(f, 1.0 - f)
    qs <- sort(q)
    ng <- as.integer(n_grid)
    if (ng < 2L) stop("pibmd: n_grid must be at least 2")
    us <- (seq_len(ng) - 0.5) / ng
    wass <- sum(vapply(us, function(u) abs(.pibmd_quantile(qs, u) -
                                             .pibmd_quantile(ps, u)), 0)) / ng
  }

  conflict <- if (moments_only) pval else pval_emp
  verdict <- if (!is.nan(conflict) && conflict < 0.05)
    "prior-data conflict: the posterior mean sits in the tail of the prior"
  else if (!is.nan(conflict)) "no evidence of prior-data conflict"
  else "conflict not assessable from moments alone"
  informative <- if (shrink < 0.05)
    "the prior dominated -- the data barely narrowed it"
  else if (shrink > 0.95)
    "the data dominated -- the prior is nearly irrelevant"
  else "prior and data both contributed"

  list(estimate = kl_g, kl_divergence = kl_g,
       kl_divergence_kde = kl_kde,
       kl_divergence_reverse = kl_rev, kl_symmetric = sym,
       shrinkage = shrink, bias_in_prior_sd = bias,
       wasserstein_1 = wass,
       conflict_p_value = conflict,
       conflict_p_value_gaussian = pval,
       conflict_p_value_empirical = pval_emp,
       posterior_mean = mq, posterior_var = sq2,
       posterior_sd = sqrt(max(sq2, 0.0)),
       prior_mean = mp, prior_var = sp2, prior_sd = sqrt(sp2),
       n_posterior = as.integer(nq), n_prior = as.integer(length(p)),
       moments_only = moments_only,
       verdict = verdict, informativeness = informative,
       method = paste0("prior-data conflict and prior informativeness: ",
                       "Gaussian and kernel-density KL(posterior || prior), ",
                       "shrinkage, and the tail probability of the ",
                       "posterior mean under the prior (Evans & Moshonov ",
                       "2006; Silverman 1986)"),
       note = paste0("kl_divergence is the Gaussian route, which is exact ",
                     "for normal pairs and blind to shape; ",
                     "kl_divergence_kde is shape-aware and will disagree ",
                     "when the posterior is not unimodal -- the ",
                     "disagreement is the signal, not an error"))
}

.pibmd_cheatsheet <- function() {
  paste0("pibmd: morie_pibmd_prior_informativeness_bias_diagnostic(samples, ",
         "prior) -> KL(posterior||prior) two ways, shrinkage and a ",
         "prior-data conflict p-value (Evans & Moshonov 2006)")
}

morie_pibmd <- morie_pibmd_prior_informativeness_bias_diagnostic
