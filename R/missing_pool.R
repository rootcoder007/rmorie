#' Pool multiply imputed estimates by Rubin's rules
#'
#' Rubin (1987) pooling with the Barnard and Rubin (1999) degrees of
#' freedom, as \code{mice::pool.scalar}: the fraction of missing
#' information is \eqn{(r + 2/(\nu + 3)) / (r + 1)} and the relative
#' efficiency \eqn{1 / (1 + \gamma / m)}.
#'
#' @param estimates Numeric vector of the m point estimates.
#' @param variances Numeric vector of the m within-imputation variances.
#' @param confidence Confidence level of the interval. Default 0.95.
#' @param dfcom Complete-data degrees of freedom for the small-sample
#'   adjustment; \code{Inf} gives Rubin's large-sample df.
#' @return A list with \code{estimate}, \code{within_variance},
#'   \code{between_variance}, \code{total_variance}, \code{standard_error},
#'   \code{df}, \code{ci_lower}, \code{ci_upper}, \code{fmi},
#'   \code{relative_efficiency}, \code{lambda_hat} and \code{method}.
#' @references Rubin, D. B. (1987). Multiple Imputation for Nonresponse in
#'   Surveys. Wiley. Barnard, J. and Rubin, D. B. (1999). Small-sample
#'   degrees of freedom with multiple imputation. Biometrika 86, 948-955.
#' @examples
#' rubins_rules(c(1.2, 1.5, 1.1, 1.4, 1.3), c(0.04, 0.05, 0.045, 0.05, 0.042))
#' @export
rubins_rules <- function(estimates, variances, confidence = 0.95, dfcom = Inf) {
  Q <- as.numeric(estimates)
  U <- as.numeric(variances)
  m <- length(Q)
  if (m < 2L) stop("Need at least 2 imputations for pooling.", call. = FALSE)
  if (length(U) != m) stop("estimates and variances must have the same length.", call. = FALSE)
  qbar <- mean(Q)
  ubar <- mean(U)
  b <- stats::var(Q)
  tv <- ubar + (1 + 1 / m) * b
  r <- if (ubar > 0) (1 + 1 / m) * b / ubar else 0
  lambda <- if (tv > 0) (1 + 1 / m) * b / tv else 0
  # lambda floored at 1e-4 so the old df stays finite, as mice
  lam <- max(lambda, 1e-4)
  df_old <- (m - 1) / lam^2
  df <- if (is.infinite(dfcom)) df_old else {
    df_obs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lam)
    df_old * df_obs / (df_old + df_obs)
  }
  fmi <- (r + 2 / (df + 3)) / (r + 1)
  tc <- stats::qt((1 + confidence) / 2, df)
  list(estimate = qbar, within_variance = ubar, between_variance = b,
       total_variance = tv, standard_error = sqrt(tv), df = df,
       ci_lower = qbar - tc * sqrt(tv), ci_upper = qbar + tc * sqrt(tv),
       fmi = fmi, relative_efficiency = 1 / (1 + fmi / m),
       lambda_hat = lambda, method = "Rubin's rules")
}

#' Fraction of missing information of multiply imputed estimates
#'
#' @inheritParams rubins_rules
#' @return The fraction of missing information from \code{\link{rubins_rules}}.
#' @examples
#' fraction_missing_information(c(1.2, 1.5, 1.1), c(0.04, 0.05, 0.045))
#' @export
fraction_missing_information <- function(estimates, variances) {
  rubins_rules(estimates, variances)$fmi
}

#' Relative efficiency of m imputations
#'
#' \eqn{RE = 1 / (1 + \gamma / m)} with \eqn{\gamma} the fraction of
#' missing information (Rubin 1987, p. 114).
#'
#' @param m Number of imputations.
#' @param fmi Fraction of missing information.
#' @return Numeric relative efficiency.
#' @examples
#' relative_efficiency(5, 0.3)
#' @export
relative_efficiency <- function(m, fmi) {
  if (m > 0) 1 / (1 + fmi / m) else 0
}

#' Maximum-likelihood mean and covariance under missing data by EM
#'
#' Dempster, Laird and Rubin (1977); Little and Rubin (2002, sec. 11.2).
#' @param X Numeric matrix with NA for missing entries; no all-missing rows.
#' @return list(mu, sigma) of the multivariate normal ML estimates.
#' @noRd
.em_mvn <- function(X, tol = 1e-12, max_iter = 10000L) {
  n <- nrow(X)
  p <- ncol(X)
  mu <- colMeans(X, na.rm = TRUE)
  sigma <- diag(apply(X, 2, function(v) mean((v - mean(v, na.rm = TRUE))^2, na.rm = TRUE)), p)
  for (it in seq_len(max_iter)) {
    t1 <- numeric(p)
    t2 <- matrix(0, p, p)
    for (i in seq_len(n)) {
      x <- X[i, ]
      mi <- is.na(x)
      cc <- matrix(0, p, p)
      if (any(mi)) {
        o <- !mi
        soo <- sigma[o, o, drop = FALSE]
        smo <- sigma[mi, o, drop = FALSE]
        x[mi] <- mu[mi] + smo %*% solve(soo, x[o] - mu[o])
        cc[mi, mi] <- sigma[mi, mi, drop = FALSE] - smo %*% solve(soo, t(smo))
      }
      t1 <- t1 + x
      t2 <- t2 + tcrossprod(x) + cc
    }
    new_mu <- t1 / n
    new_sigma <- t2 / n - tcrossprod(new_mu)
    change <- max(abs(new_mu - mu), abs(new_sigma - sigma))
    mu <- new_mu
    sigma <- new_sigma
    if (change < tol) break
  }
  list(mu = mu, sigma = sigma)
}

#' Little's test of missing completely at random
#'
#' Little (1988, eq. 2): \eqn{d^2 = \sum_j n_j (\bar y_j - \mu_j)^T
#' \Sigma_j^{-1} (\bar y_j - \mu_j)} over the missingness patterns, with
#' the maximum-likelihood mean and covariance from EM restricted to each
#' pattern's observed variables, referred to chi-square on
#' \eqn{\sum_j p_j - p} df. Rows with nothing observed are dropped.
#'
#' @param data A data frame or matrix; numeric columns are used.
#' @return A list with \code{test_statistic}, \code{p_value}, \code{df},
#'   \code{n_patterns} and \code{method}.
#' @references Little, R. J. A. (1988). A test of missing completely at
#'   random for multivariate data with missing values. JASA 83, 1198-1202.
#' @examples
#' X <- data.frame(a = c(1, 2, NA, 4, 5, 6), b = c(2, NA, 3, 5, 4, 7))
#' littles_mcar_test(X)
#' @export
littles_mcar_test <- function(data) {
  df <- as.data.frame(data)
  X <- as.matrix(df[, vapply(df, is.numeric, logical(1)), drop = FALSE])
  p <- ncol(X)
  if (p < 2L) stop("Little's MCAR test requires at least 2 numeric variables.", call. = FALSE)
  X <- X[rowSums(!is.na(X)) > 0, , drop = FALSE]
  pat <- apply(!is.na(X), 1, paste, collapse = "")
  up <- unique(pat)
  if (length(up) <= 1L) {
    return(list(test_statistic = 0, p_value = 1, df = 0L, n_patterns = length(up),
                method = "Little's MCAR test"))
  }
  em <- .em_mvn(X)
  d2 <- 0
  dfv <- 0L
  for (q in up) {
    g <- X[pat == q, , drop = FALSE]
    o <- !is.na(g[1, ])
    dm <- colMeans(g[, o, drop = FALSE]) - em$mu[o]
    d2 <- d2 + nrow(g) * sum(dm * solve(em$sigma[o, o, drop = FALSE], dm))
    dfv <- dfv + sum(o)
  }
  dfv <- dfv - p
  list(test_statistic = d2, p_value = stats::pchisq(d2, dfv, lower.tail = FALSE),
       df = dfv, n_patterns = length(up), method = "Little's MCAR test")
}
