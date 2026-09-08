# Rubin's rules for combining multiple-imputation estimates.
# Source: Rubin (1987), Multiple Imputation for Nonresponse in
# Surveys, Sec. 3.3; van Buuren (2018), Flexible Imputation of
# Missing Data, 2nd ed., Sec. 2.3, Eqs. 2.16-2.32
# (fetched-wave3/vanbuuren-fimd-ch2-rubins-rules.html).  Mirrors
# Python morie.fn.miefcl exactly.  Cross-checked test-only against
# mice::pool.scalar.

#' Pool a scalar estimate across multiply imputed data sets
#'
#' Rubin's rules: Qbar = mean(Q_l), Ubar = mean(U_l),
#' B = var(Q_l), T = Ubar + (1 + 1/m) B, with the relative increase
#' in variance, the proportion of variance attributable to missing
#' data lambda, the fraction of missing information, and the degrees
#' of freedom (Barnard-Rubin adjusted when \code{nu_com} is given).
#'
#' @param estimates Numeric vector of complete-data estimates.
#' @param variances Numeric vector of complete-data variances.
#' @param nu_com Optional complete-data degrees of freedom (n - k).
#' @return A list with elements \code{estimate}, \code{se}, \code{t},
#'   \code{ubar}, \code{b}, \code{m}, \code{riv}, \code{lambda_},
#'   \code{fmi}, \code{df}, \code{method}.
#' @references Rubin, D. B. (1987). Multiple Imputation for
#'   Nonresponse in Surveys. Wiley.  van Buuren, S. (2018). Flexible
#'   Imputation of Missing Data, 2nd ed., Chapman & Hall/CRC.
#'   Barnard, J. and Rubin, D. B. (1999). Biometrika, 86, 948-955.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_miefcl(V, V)
morie_miefcl <- function(estimates, variances, nu_com = NULL) {
  q <- as.numeric(estimates)
  u <- as.numeric(variances)
  m <- length(q)
  if (m < 2) stop("need at least two imputations")
  if (length(u) != m) {
    stop("estimates and variances must have equal length")
  }
  if (any(u < 0)) stop("variances must be non-negative")
  qbar <- mean(q)
  ubar <- mean(u)
  b <- sum((q - qbar)^2) / (m - 1)
  t <- ubar + (1 + 1 / m) * b
  if (t <= 0) stop("total variance is not positive")
  lam <- (b + b / m) / t
  riv <- if (ubar == 0) Inf else (1 + 1 / m) * b / ubar
  df_old <- if (lam <= 0) Inf else (m - 1) / lam^2
  if (is.null(nu_com)) {
    df <- df_old
  } else {
    nc <- as.numeric(nu_com)
    if (nc <= 0) stop("nu_com must be positive")
    nu_obs <- (nc + 1) / (nc + 3) * nc * (1 - lam)
    df <- if (is.infinite(df_old)) min(nu_obs, nc) else {
      df_old * nu_obs / (df_old + nu_obs)
    }
  }
  fmi <- if (is.infinite(riv)) 1 else (riv + 2 / (df + 3)) / (1 + riv)
  list(estimate = qbar,
       se = sqrt(t),
       t = t,
       ubar = ubar,
       b = b,
       m = m,
       riv = riv,
       lambda_ = lam,
       fmi = fmi,
       df = df,
       method = "Rubin's rules (Rubin 1987; van Buuren 2018 Sec. 2.3)")
}
