# SPDX-License-Identifier: AGPL-3.0-or-later
.dunnett_cdf <- function(cval, lam, df) {
  gl <- .schab_gauss_legendre(16L)
  panels <- function(a, b, m) {
    h <- (b - a) / m
    lo <- a + (seq_len(m) - 1L) * h
    list(x = as.vector(outer(0.5 * h * (gl$nodes + 1), lo, "+")), w = rep(0.5 * h * gl$weights, m))
  }
  zq <- panels(-9, 9, 36L)
  phi <- stats::dnorm(zq$x)
  sq <- sqrt(1 - lam^2)
  inner <- function(cs) {
    prod <- rep(1, length(zq$x))
    for (j in seq_along(lam)) {
      prod <- prod * (stats::pnorm((lam[j] * zq$x + cs) / sq[j]) - stats::pnorm((lam[j] * zq$x - cs) / sq[j]))
    }
    sum(zq$w * phi * prod)
  }
  up <- sqrt(160 / df) + 1
  sq2 <- panels(0, up, 48L)
  logc <- 0.5 * df * log(df) - lgamma(0.5 * df) - (0.5 * df - 1) * log(2)
  keep <- sq2$x > 0
  s <- sq2$x[keep]
  dens <- exp(logc + (df - 1) * log(s) - 0.5 * df * s^2)
  sum(sq2$w[keep] * dens * vapply(cval * s, inner, numeric(1)))
}

#' Dunnett's many-to-one comparisons with exact multivariate-t p-values
#'
#' Each treatment mean is compared with the control by t_i = (ybar_i -
#' ybar_0) / sqrt(MSE (1/n_i + 1/n_0)) on the pooled within-group mean square
#' with N - k - 1 degrees of freedom; jointly the t_i are multivariate t with
#' correlations lambda_i lambda_j, lambda_i = sqrt(n_i / (n_i + n_0)) (Dunnett
#' 1955). The two-sided adjusted p-value is 1 - P(max_j |T_j| <= |t_i|),
#' computed by quadrature of Dunnett's double integral: multcomp's
#' glht(..., mcp(g = "Dunnett")).
#'
#' @param control Numeric control observations.
#' @param ... Numeric vectors, one per treatment group.
#' @return Named list: comparisons (data frame of group, diff, se, t, p_adj),
#'   mse, df, n_control.
#' @references Dunnett, C. W. (1955). JASA 50, 1096-1121. Hothorn, T., Bretz,
#'   F. & Westfall, P. (2008). Biometrical Journal 50, 346-363.
#' @examples
#' morie_dunnett_test(c(5.1, 4.8, 5.5, 5), c(5.9, 6.1, 5.7, 6.3), c(5.2, 5.4, 4.9, 5.6))$comparisons
#' @export
morie_dunnett_test <- function(control, ...) {
  groups <- list(...)
  if (length(groups) < 1L) stop("Need >= 1 treatment group.", call. = FALSE)
  control <- as.numeric(control)
  groups <- lapply(groups, as.numeric)
  k <- length(groups)
  n0 <- length(control)
  df <- n0 + sum(lengths(groups)) - k - 1
  if (df < 1) stop("Need more observations than groups.", call. = FALSE)
  mse <- (sum((control - mean(control))^2) + sum(vapply(groups, function(g) sum((g - mean(g))^2), 1))) / df
  lam <- sqrt(lengths(groups) / (lengths(groups) + n0))
  diff <- vapply(groups, mean, 1) - mean(control)
  se <- sqrt(mse * (1 / lengths(groups) + 1 / n0))
  tt <- diff / se
  p <- vapply(abs(tt), function(cv) min(1, max(0, 1 - .dunnett_cdf(cv, lam, df))), numeric(1))
  list(comparisons = data.frame(group = seq_len(k), diff = diff, se = se, t = tt, p_adj = p),
       mse = mse, df = df, n_control = n0)
}
