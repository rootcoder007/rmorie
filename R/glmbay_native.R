# R arm of glmbay -- Bayesian GLM by penalised IRLS to the posterior mode
# with a normal prior, Laplace covariance. Gelman, A. et al. (2013) Bayesian
# Data Analysis, 3rd ed., CRC Press, Ch. 16 and Sec. 4.1.
# Mirrors src/morie/fn/glmbay.py.

.glmbay_EPS <- 1e-12

#' .glmbay_links
#'
#' A step of the glmbay_native implementation. Called by \code{morie_glmbay_bayesian_glm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param family One of \code{"binomial"}, \code{"gaussian"}, \code{"poisson"}.
#' @return Nothing; this branch always raises.
#' @export
.glmbay_links <- function(family) {
  if (family == "binomial")
    return(list(inv = function(e) 1.0 / (1.0 + exp(-pmax(-500, pmin(500, e)))),
                var = function(m) pmax(m * (1.0 - m), 1e-10),
                ll = function(y, m) {
                  m <- pmin(pmax(m, 1e-12), 1.0 - 1e-12)
                  y * log(m) + (1.0 - y) * log(1.0 - m)
                }))
  if (family == "poisson")
    return(list(inv = function(e) exp(pmax(-500, pmin(500, e))),
                var = function(m) pmax(m, 1e-10),
                ll = function(y, m) {
                  m <- pmax(m, 1e-12)
                  y * log(m) - m - lgamma(y + 1.0)
                }))
  if (family == "gaussian")
    return(list(inv = function(e) e, var = function(m) rep(1.0, length(m)),
                ll = function(y, m) -0.5 * (log(2.0 * pi) + (y - m) ^ 2)))
  stop(sprintf(paste0("glmbay: family must be binomial, poisson or ",
                      "gaussian, got '%s'"), family))
}

#' morie_glmbay_bayesian_glm
#'
#' A step of the glmbay_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param family Passed to \code{.glmbay_links}. Defaults to \code{"binomial"}.
#' @param prior_sd Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.5}.
#' @param add_intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @param tol Passed to \code{<}. Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{coefficients}, \code{posterior_sd}, \code{std_error}, \code{ci_lower}, \code{ci_upper}, \code{fitted}, \code{linear_predictor}, \code{loglik}, \code{log_prior}, \code{log_marginal}, \code{log_det_hessian}, \code{iterations}, \code{converged}, \code{family}, \code{prior_sd}, \code{n}, \code{p}, \code{method}, \code{note}.
#' @export
morie_glmbay_bayesian_glm <- function(X, y, family = "binomial",
                                      prior_sd = 2.5, add_intercept = TRUE,
                                      max_iter = 100, tol = 1e-10) {
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  n <- nrow(Xm)
  if (n == 0L) stop("glmbay: no observations")
  if (length(yv) != n)
    stop(sprintf("glmbay: %d rows but %d responses", n, length(yv)))
  if (isTRUE(add_intercept)) Xm <- cbind(1.0, Xm)
  p <- ncol(Xm)
  lk <- .glmbay_links(family)
  ps <- as.numeric(prior_sd)
  if (ps <= 0.0)
    stop("glmbay: the prior standard deviation must be positive")
  tau <- 1.0 / (ps * ps)

  beta <- rep(0.0, p)
  it <- 0L
  converged <- FALSE
  H <- NULL
  for (it in seq_len(as.integer(max_iter))) {
    eta <- as.numeric(Xm %*% beta)
    mu <- lk$inv(eta)
    w <- lk$var(mu)
    z <- eta + (yv - mu) / w
    A <- crossprod(Xm * w, Xm)
    diag(A) <- diag(A) + tau
    b <- as.numeric(crossprod(Xm, w * z))
    Lc <- chol(A)
    new <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), b)))
    shift <- max(abs(new - beta))
    beta <- new
    H <- A
    if (shift < tol) { converged <- TRUE
    break }
  }

  Lc <- chol(H)
  cov <- matrix(0.0, p, p)
  for (a in seq_len(p)) {
    e <- numeric(p)
    e[a] <- 1.0
    cov[, a] <- as.numeric(backsolve(Lc, forwardsolve(t(Lc), e)))
  }
  se <- sqrt(pmax(diag(cov), 0.0))

  eta <- as.numeric(Xm %*% beta)
  mu <- lk$inv(eta)
  loglik <- sum(lk$ll(yv, mu))
  logprior <- sum(-0.5 * tau * beta ^ 2 - 0.5 * log(2.0 * pi * ps * ps))
  logdet <- 2.0 * sum(log(diag(Lc)))
  log_marginal <- loglik + logprior + 0.5 * p * log(2.0 * pi) - 0.5 * logdet

  list(estimate = beta, coefficients = beta, posterior_sd = se,
       std_error = se,
       ci_lower = beta - 1.959963984540054 * se,
       ci_upper = beta + 1.959963984540054 * se,
       fitted = mu, linear_predictor = eta,
       loglik = loglik, log_prior = logprior,
       log_marginal = log_marginal, log_det_hessian = logdet,
       iterations = as.integer(it), converged = converged,
       family = family, prior_sd = ps, n = as.integer(n), p = as.integer(p),
       method = paste0("Bayesian GLM: penalised IRLS to the posterior mode ",
                       "with a normal prior, Laplace covariance (Gelman et ",
                       "al. BDA3 Ch. 16, Sec. 4.1)"),
       note = paste0("the prior is what separates this from ML -- under ",
                     "separation the ML coefficient diverges while the ",
                     "posterior mode stays finite; the Laplace ",
                     "approximation is exact for the Gaussian family"))
}

#' .glmbay_cheatsheet
#'
#' A step of the glmbay_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.glmbay_cheatsheet <- function() {
  paste0("glmbay: morie_glmbay_bayesian_glm(X, y, family, prior_sd) -> ",
         "posterior mode, Laplace covariance and log marginal likelihood ",
         "(Gelman et al. 2013 BDA3 Ch. 16)")
}

morie_glmbay <- morie_glmbay_bayesian_glm
