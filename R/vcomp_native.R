# Variance-components estimation, ANOVA or REML (Searle et al. 1992).
#
# Sources
# -------
# Searle, S. R., Casella, G. & McCulloch, C. E. (1992). *Variance
# Components*. Wiley: ANOVA estimators Eq. (21) and Chs. 4-5, REML
# Sec. 6.6, balanced-data equivalence Sec. 4.8, interval estimation
# Sec. 3.5 (local copy fetched-wave3/Variance_components_FULL.pdf).

.vcomp_f_cdf <- function(x, d1, d2) {
  # F CDF via the regularized incomplete beta:
  # P(F <= x) = I_{d1 x / (d1 x + d2)}(d1/2, d2/2)
  if (x <= 0) return(0.0)
  pbeta(d1 * x / (d1 * x + d2), d1 / 2.0, d2 / 2.0)
}

.vcomp_f_ppf <- function(p, d1, d2, iters = 300) {
  # monotone bisection on the CDF (same convention as the R arm)
  if (p <= 0.0) return(0.0)
  if (p >= 1.0) return(Inf)
  lo <- 0.0
  hi <- 1.0
  while (.vcomp_f_cdf(hi, d1, d2) < p && hi < 1e12) {
    hi <- hi * 2.0
  }
  for (i in seq_len(iters)) {
    mid <- 0.5 * (lo + hi)
    if (.vcomp_f_cdf(mid, d1, d2) < p) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  return(0.5 * (lo + hi))
}

morie_vcomp <- function(y, group, method = "reml", conf_level = 0.95) {
  if (!(method %in% c("reml", "anova"))) {
    stop("method must be 'reml' or 'anova'")
  }
  if (!(0 < conf_level && conf_level < 1)) {
    stop("vcomp: conf_level must lie in (0, 1), got ", conf_level)
  }
  av <- morie_ranova(y, group)
  fit <- if (method == "reml") morie_remlfn(y, group) else av
  s2a <- as.numeric(fit$sigma2_a)
  s2e <- as.numeric(fit$sigma2_e)
  denom <- s2a + s2e
  icc <- if (denom > 0) s2a / denom else 0.0
  lo <- hi <- NULL
  if (isTRUE(av$balanced)) {
    n <- av$n_i[1]
    a <- as.integer(av$a)
    N <- as.integer(av$N)
    alpha <- 1.0 - conf_level
    Fval <- if (av$mse > 0) av$msa / av$mse else Inf
    f_hi <- .vcomp_f_ppf(1.0 - alpha / 2.0, a - 1, N - a)
    f_lo <- .vcomp_f_ppf(alpha / 2.0, a - 1, N - a)
    FL <- Fval / f_hi
    FU <- Fval / f_lo
    lo <- (FL - 1.0) / (FL - 1.0 + n)
    hi <- (FU - 1.0) / (FU - 1.0 + n)
    if (lo < 0.0) lo <- 0.0
    if (hi > 1.0) hi <- 1.0
  }
  list(
    sigma2_a = s2a,
    sigma2_e = s2e,
    icc = icc,
    icc_lower = lo,
    icc_upper = hi,
    method_used = method,
    balanced = isTRUE(av$balanced),
    a = as.integer(av$a),
    N = as.integer(av$N),
    fit = fit,
    method = sprintf("variance components, %s (Searle et al. 1992)", method)
  )
}

# long descriptive alias (stub-era name)
variance_components <- morie_vcomp

.vcomp_cheatsheet <- function() {
  return("vcomp: ANOVA or REML variance components + exact ICC F interval on balanced data (Searle Sec. 3.5)")
}

# public names resolved by fn/_lazy_map.json
variance_components_henderson3 <- morie_vcomp
