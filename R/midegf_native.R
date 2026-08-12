# Barnard-Rubin small-sample MI degrees of freedom.
# Source: Barnard & Rubin (1999), Biometrika 86, 948-955; van Buuren
# (2018), Sec. 2.3.6, Eqs. 2.30-2.32
# (fetched-wave3/vanbuuren-fimd-ch2-rubins-rules.html).  Mirrors
# Python morie.fn.midegf exactly.  Cross-checked test-only against
# mice:::barnard.rubin (fetched-wave3/mice-barnard-rubin-source.R).

#' Barnard-Rubin adjusted degrees of freedom for MI inference
#'
#' With lambda = (1 + 1/m) B / T, the classical df is
#' nu_old = (m - 1)/lambda^2; the Barnard-Rubin small-sample value is
#' nu = nu_old * nu_obs / (nu_old + nu_obs) with
#' nu_obs = ((nu_com + 1)/(nu_com + 3)) nu_com (1 - lambda), always
#' at most nu_com.
#'
#' @param b Between-imputation variance.
#' @param t Total variance.
#' @param m Number of imputations (>= 2).
#' @param nu_com Optional complete-data degrees of freedom.
#' @return A list with elements \code{df}, \code{df_old},
#'   \code{nu_obs}, \code{lambda_}, \code{m}, \code{method}.
#' @references Barnard, J. and Rubin, D. B. (1999). Small-sample
#'   degrees of freedom with multiple imputation. Biometrika, 86,
#'   948-955.  van Buuren, S. (2018). Flexible Imputation of Missing
#'   Data, 2nd ed., Sec. 2.3.6.
#' @export
morie_midegf <- function(b, t, m, nu_com = NULL) {
  b <- as.numeric(b)
  t <- as.numeric(t)
  m <- as.integer(m)
  if (m < 2) stop("m must be at least 2")
  if (b < 0 || t <= 0) stop("need b >= 0 and t > 0")
  lam <- (1 + 1 / m) * b / t
  lam <- min(max(lam, 0), 1)
  df_old <- if (lam == 0) Inf else (m - 1) / lam^2
  nu_obs <- NULL
  if (is.null(nu_com)) {
    df <- df_old
  } else {
    nc <- as.numeric(nu_com)
    if (nc <= 0) stop("nu_com must be positive")
    nu_obs <- (nc + 1) / (nc + 3) * nc * (1 - lam)
    df <- if (is.infinite(df_old)) nu_obs else {
      if (df_old + nu_obs > 0) df_old * nu_obs / (df_old + nu_obs) else 0
    }
  }
  list(df = df,
       df_old = df_old,
       nu_obs = nu_obs,
       lambda_ = lam,
       m = m,
       method = "Barnard-Rubin (1999) adjusted MI df")
}
