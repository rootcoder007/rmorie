#' Impacts, predictions and Wald test of spatial-lag count models
#'
#' \code{scpmf} and \code{scnbmf}: average direct, indirect and total impacts
#' of the spatial-lag Poisson and NB2 models with mean
#' \eqn{\mu = \exp((I - \rho W)^{-1} X \beta)}. The derivative of
#' \eqn{\mu_i} in regressor k of unit j is
#' \eqn{S_k(i, j) = \mu_i A_{ij} \beta_k} with \eqn{A = (I - \rho W)^{-1}}; the direct impact is
#' the trace of \eqn{S_k} over n, the total impact the sum of all its entries
#' over n, and the indirect impact their difference (the NB2 mean is the
#' Poisson mean, so both give the same impacts). \code{scpprd}: fitted means.
#' \code{scpwld}: Wald chi-square(1) test of rho = rho0. Identical to the
#' Python arms \code{morie.fn.scpmf}, \code{scnbmf}, \code{scpprd} and
#' \code{scpwld}.
#'
#' @param coef Coefficients beta.
#' @param rho Spatial lag parameter.
#' @param X Design matrix (n by length(coef)).
#' @param W Spatial weights matrix.
#' @param se_rho Standard error of rho.
#' @param rho0 Null value of rho.
#' @return List: direct, indirect, total, fitted, linear_predictor
#'   (\code{scpmf}, \code{scnbmf}); fitted, linear_predictor, total
#'   (\code{scpprd}); statistic, df, p_value, z (\code{scpwld}).
#' @references LeSage, J. P. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. CRC Press, section 2.7.
#'
#'   Lambert, D. M., Brown, J. P. and Florax, R. J. G. M. (2010). A two-step
#'   estimator for a spatial lag model of counts. Regional Science and Urban
#'   Economics 40, 241-252.
#' @examples
#' W <- rbind(c(0, 1, 0), c(0.5, 0, 0.5), c(0, 1, 0))
#' X <- cbind(1, c(0.1, 0.4, 0.9))
#' scpmf(c(0.2, 0.5), 0.3, X, W)$total
#' scpprd(c(0.2, 0.5), X, W, rho = 0.3)$fitted
#' scpwld(0.3, 0.1)$p_value
#' @export
scpmf <- function(coef, rho, X, W) {
  X <- as.matrix(X)
  W <- as.matrix(W)
  b <- as.numeric(coef)
  n <- nrow(X)
  if (ncol(X) != length(b) || nrow(W) != n || ncol(W) != n) stop("X must be n x p with p = length(coef) and W n x n")
  Ai <- solve(diag(n) - rho * W)
  eta <- as.vector(Ai %*% (X %*% b))
  mu <- exp(eta)
  md <- sum(mu * diag(Ai)) / n
  mt <- sum(mu * rowSums(Ai)) / n
  list(direct = md * b, indirect = mt * b - md * b, total = mt * b, fitted = mu, linear_predictor = eta)
}

#' @rdname scpmf
#' @export
scnbmf <- function(coef, rho, X, W) scpmf(coef, rho, X, W)

#' @rdname scpmf
#' @export
scpprd <- function(coef, X, W, rho = 0.2) {
  r <- scpmf(coef, rho, X, W)
  list(fitted = r$fitted, linear_predictor = r$linear_predictor, total = sum(r$fitted))
}

#' @rdname scpmf
#' @export
scpwld <- function(rho, se_rho, rho0 = 0) {
  if (!(se_rho > 0)) stop("se_rho must be positive")
  z <- (rho - rho0) / se_rho
  w <- z * z
  list(statistic = w, df = 1L, p_value = stats::pchisq(w, 1, lower.tail = FALSE), z = z)
}
