# R arm of morie/fn/macn.py -- Cochran's Q for meta-analytic heterogeneity.
#
# This mirror was missing entirely. Two agents worked `macn` concurrently;
# one implemented the richer Python version (adding DerSimonian-Laird
# tau^2 and Higgins I^2), the other deleted its own R file to avoid a
# write war and reported the gap rather than leaving a silent one-armed
# module. Restoring the R side here closes it.
#
# Cochran (1954) Biometrics 10(1):101-129; DerSimonian & Laird (1986)
# Controlled Clinical Trials 7(3):177-188; Higgins & Thompson (2002)
# Statistics in Medicine 21(11):1539-1558.

#' @noRd
morie_ma_cochran_q <- function(yi, vi) {
  y <- as.numeric(yi)
  v <- as.numeric(vi)
  if (length(y) != length(v)) stop("yi and vi must have the same length.")
  k <- length(y)
  if (k < 2L) stop("need at least 2 studies.")
  if (any(v <= 0)) stop("sampling variances must be strictly positive.")

  w <- 1 / v
  sw <- sum(w)
  theta <- sum(w * y) / sw
  q <- sum(w * (y - theta)^2)
  df <- k - 1L

  # C = sum(w) - sum(w^2)/sum(w); the DerSimonian-Laird scaling constant
  cc <- sw - sum(w * w) / sw
  tau2 <- if (cc > 0) max(0, (q - df) / cc) else 0
  i2 <- if (q > 0) max(0, (q - df) / q) else 0

  list(
    statistic = q,
    pvalue = stats::pchisq(q, df, lower.tail = FALSE),
    df = df,
    k = k,
    theta_fe = theta,
    se_fe = sqrt(1 / sw),
    tau2 = tau2,
    i2 = i2,
    h2 = q / df,
    weights = w,
    c_constant = cc,
    method = "Cochran (1954) Q; DerSimonian-Laird tau^2; Higgins I^2"
  )
}

#' @noRd
Cochranq <- morie_ma_cochran_q
