#' Linear mixed model with serial correlation and measurement error
#'
#' \code{SerialCorrelation}: exponential, Gaussian or discrete AR(1) serial
#' correlation. \code{LmmSerialCovariance}: marginal covariance of one subject
#' (random intercept, serial process and measurement error).
#' \code{LmmSerialLoglik}: profile log-likelihood (ML or REML) with the fixed
#' effects and total scale profiled out. \code{LmmSerialFit}: Nelder-Mead fit of
#' the Diggle (1988) model, as \code{nlme::lme} with a random intercept and
#' \code{corExp} or \code{corGaus} with a nugget. Identical to the Python arm
#' \code{morie.fn.serlmm}.
#'
#' @param u Time lags.
#' @param phi Correlation parameter.
#' @param kind \code{"exponential"}, \code{"gaussian"} or \code{"ar1"}.
#' @param times Observation times.
#' @param sigma_b2 Random-intercept variance.
#' @param tau2 Serial process variance.
#' @param nu2 Measurement-error variance.
#' @param y Response.
#' @param X Design matrix including the intercept.
#' @param subject Subject identifiers (rows grouped in any order).
#' @param reml Use restricted maximum likelihood.
#' @param start Starting values of the square-root variance ratio, log phi and
#'   the nugget parameter r with share r^2 / (1 + r^2).
#' @return A vector, matrix or list.
#' @references Diggle, P. J. (1988). An approach to the analysis of repeated
#'   measurements. Biometrics 44, 959-971.
#'
#'   Diggle, P. J., Heagerty, P., Liang, K.-Y. and Zeger, S. L. (2002).
#'   Analysis of Longitudinal Data, 2nd edn. Oxford University Press.
#' @examples
#' SerialCorrelation(0:2, 0.5)
#' LmmSerialCovariance(c(0, 1), 1, 2, 0.5, 0.25)
#' @export
SerialCorrelation <- function(u, phi, kind = "exponential") {
  a <- abs(u)
  switch(kind,
    exponential = exp(-phi * a),
    gaussian = exp(-phi * a^2),
    ar1 = phi^a,
    stop("kind must be 'exponential', 'gaussian' or 'ar1'")
  )
}

#' @rdname SerialCorrelation
#' @export
LmmSerialCovariance <- function(times, sigma_b2, tau2, phi, nu2, kind = "exponential") {
  sigma_b2 + tau2 * SerialCorrelation(outer(times, times, "-"), phi, kind) + diag(nu2, length(times))
}

.sl_profile <- function(y, X, idx, t, lam_b, phi, cc, kind, reml) {
  p <- ncol(X)
  ld <- 0
  Xw <- NULL
  yw <- NULL
  for (s in seq_len(max(idx))) {
    rows <- which(idx == s)
    L <- t(chol(LmmSerialCovariance(t[rows], lam_b, 1 - cc, phi, cc, kind)))
    ld <- ld + 2 * sum(log(diag(L)))
    yw <- c(yw, forwardsolve(L, y[rows]))
    Xw <- rbind(Xw, forwardsolve(L, X[rows, , drop = FALSE]))
  }
  Lx <- t(chol(crossprod(Xw)))
  beta <- backsolve(t(Lx), forwardsolve(Lx, crossprod(Xw, yw)))
  rss <- sum((yw - Xw %*% beta)^2)
  m <- if (reml) length(y) - p else length(y)
  s2 <- rss / m
  ll <- -0.5 * (m * log(2 * pi * s2) + ld + m)
  if (reml) ll <- ll - sum(log(diag(Lx)))
  list(loglik = ll, beta = as.vector(beta), s2 = s2)
}

#' @rdname SerialCorrelation
#' @export
LmmSerialLoglik <- function(y, X, subject, times, sigma_b2, tau2, phi, nu2, kind = "exponential", reml = FALSE) {
  s <- tau2 + nu2
  r <- .sl_profile(y, as.matrix(X), match(subject, unique(subject)), times, sigma_b2 / s, phi, nu2 / s, kind, reml)
  list(loglik = r$loglik, beta = r$beta, sigma2 = r$s2)
}

#' @rdname SerialCorrelation
#' @export
LmmSerialFit <- function(y, X, subject, times, kind = "exponential", reml = FALSE, start = NULL) {
  X <- as.matrix(X)
  idx <- match(subject, unique(subject))
  x0 <- if (is.null(start)) c(1, 0, 0.5) else start
  f <- function(th) {
    r <- tryCatch(.sl_profile(y, X, idx, times, th[1]^2, exp(th[2]), th[3]^2 / (1 + th[3]^2), kind, reml),
                  error = function(e) NULL)
    if (is.null(r)) Inf else -r$loglik
  }
  opt <- NelderMead(f, x0, step = 0.5, xtol = 1e-11, ftol = 1e-14, max_iter = 6000)
  th <- opt$x
  lam <- th[1]^2
  phi <- exp(th[2])
  cc <- th[3]^2 / (1 + th[3]^2)
  r <- .sl_profile(y, X, idx, times, lam, phi, cc, kind, reml)
  list(beta = r$beta, sigma_b2 = lam * r$s2, tau2 = (1 - cc) * r$s2, nu2 = cc * r$s2, phi = phi,
       loglik = r$loglik, converged = opt$converged)
}
