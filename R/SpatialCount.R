#' Spatial count regression
#'
#' \code{SarPoisson}, \code{SarNegbin}, \code{SarZip}, \code{SarZinb}:
#' Poisson, negative binomial (NB2), zero-inflated Poisson and zero-inflated
#' negative binomial regressions with the spatially lagged mean
#' \eqn{\exp((I - \rho W)^{-1} X \beta)}, by profile maximum likelihood over
#' rho (IRLS, dispersion ML and EM for fixed rho). \code{SarPoissonLmTest}:
#' score test of rho = 0. \code{CountModelIc}: AIC, BIC, AICc.
#' \code{BymVarianceFraction}: spatial share of the BYM variance with
#' Sorbye-Rue scaling. Identical to the Python arm \code{morie.fn.spcount}.
#'
#' @param y Counts.
#' @param X Design matrix (with intercept).
#' @param W Spatial weights matrix.
#' @param rho_bounds Search interval for rho.
#' @param Z Zero-model design (default intercept).
#' @param loglik,k,n Log-likelihood, number of parameters, sample size.
#' @param var_spatial,var_unstructured Spatial and unstructured variances.
#' @param Q ICAR structure matrix (optional).
#' @return List.
#' @references Lambert, D. M., Brown, J. P. and Florax, R. J. G. M. (2010). A
#'   two-step estimator for a spatial lag model of counts. Regional Science
#'   and Urban Economics 40, 241-252.
#'
#'   Lambert, D. (1992). Zero-inflated Poisson regression. Technometrics 34,
#'   1-14.
#'
#'   Riebler, A., Sorbye, S. H., Simpson, D. and Rue, H. (2016). An intuitive
#'   Bayesian spatial model for disease mapping that accounts for scaling.
#'   Statistical Methods in Medical Research 25, 1145-1165.
#' @examples
#' W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
#' X <- cbind(1, c(0.1, 0.4, 0.5, 0.9))
#' SarPoisson(c(1, 3, 4, 7), X, W)$rho
#' CountModelIc(-10, 3, 20)$BIC
#' @export
SarPoisson <- function(y, X, W, rho_bounds = c(-0.99, 0.99)) {
  d <- .sc_prep(y, X, W)
  prof <- function(rho) {
    f <- .sc_glm(d$y, .sc_lag(d$X, d$W, rho))
    .sc_ll(d$y, f$mu, NULL, rep(1, length(d$y)))
  }
  rho <- if (rho_bounds[1] == rho_bounds[2]) rho_bounds[1] else .sc_golden(prof, rho_bounds[1], rho_bounds[2])
  f <- .sc_glm(d$y, .sc_lag(d$X, d$W, rho))
  list(coefficients = f$b, rho = rho, fitted = f$mu, loglik = .sc_ll(d$y, f$mu, NULL, rep(1, length(d$y))),
       k = length(f$b) + 1)
}

.sc_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.sc_prep <- function(y, X, W) {
  y <- as.numeric(y)
  X <- as.matrix(X) + 0
  W <- as.matrix(W) + 0
  if (any(y < 0 | y != round(y))) stop("y must be non-negative counts")
  if (nrow(X) != length(y) || nrow(W) != length(y)) stop("X and W must have n rows")
  list(y = y, X = X, W = W)
}

.sc_lag <- function(X, W, rho) solve(diag(nrow(W)) - rho * W, X)

.sc_wls <- function(X, z, w) {
  p <- ncol(X)
  M <- matrix(0, p, p)
  r <- numeric(p)
  for (a in seq_len(p)) {
    for (b in seq_len(p)) M[a, b] <- .sc_ss(w * X[, a] * X[, b])
    r[a] <- .sc_ss(w * X[, a] * z)
  }
  solve(M, r)
}

.sc_ll <- function(y, mu, theta, pw) {
  s <- 0
  for (i in seq_along(y)) {
    li <- if (is.null(theta)) {
      if (mu[i] > 0) y[i] * log(mu[i]) - mu[i] - lgamma(y[i] + 1) else if (y[i] == 0) 0 else -Inf
    } else {
      lgamma(y[i] + theta) - lgamma(theta) - lgamma(y[i] + 1) + theta * log(theta / (theta + mu[i])) +
        y[i] * log(mu[i] / (theta + mu[i]))
    }
    s <- s + pw[i] * li
  }
  s
}

.sc_glm <- function(y, X, prior = NULL, theta = NULL, beta = NULL) {
  n <- length(y)
  pw <- if (is.null(prior)) rep(1, n) else prior
  if (is.null(beta)) {
    m0 <- .sc_ss(pw * y) / .sc_ss(pw)
    b <- .sc_wls(X, rep(log(max(m0, 1e-10)), n), pw)
  } else {
    b <- beta
  }
  dev_old <- Inf
  for (it in 1:200) {
    eta <- as.vector(X %*% b)
    mu <- exp(eta)
    w <- pw * (if (is.null(theta)) mu else mu / (1 + mu / theta))
    z <- eta + (y - mu) / mu
    b <- .sc_wls(X, z, w)
    mu <- exp(as.vector(X %*% b))
    dev <- -2 * .sc_ll(y, mu, theta, pw)
    if (abs(dev - dev_old) <= 1e-12 * (abs(dev) + 0.1)) break
    dev_old <- dev
  }
  list(b = b, mu = mu)
}

.sc_theta <- function(y, mu, pw) {
  score <- function(lt) {
    t <- exp(lt)
    .sc_ss(pw * (.s03digamma(y + t) - .s03digamma(t) + log(t) + 1 - log(t + mu) - (y + t) / (t + mu)))
  }
  lo <- log(1e-4)
  hi <- log(1e6)
  slo <- score(lo)
  if ((slo > 0) == (score(hi) > 0)) return(if (slo > 0) exp(hi) else exp(lo))
  for (it in 1:200) {
    mid <- 0.5 * (lo + hi)
    sm <- score(mid)
    if ((sm > 0) == (slo > 0)) {
      lo <- mid
      slo <- sm
    } else {
      hi <- mid
    }
    if (hi - lo < 1e-13) break
  }
  exp(0.5 * (lo + hi))
}

.sc_nb <- function(y, X, prior = NULL) {
  pw <- if (is.null(prior)) rep(1, length(y)) else prior
  f <- .sc_glm(y, X, pw)
  theta <- .sc_theta(y, f$mu, pw)
  ll_old <- -Inf
  for (it in 1:100) {
    f <- .sc_glm(y, X, pw, theta, f$b)
    theta <- .sc_theta(y, f$mu, pw)
    ll <- .sc_ll(y, f$mu, theta, pw)
    if (abs(ll - ll_old) <= 1e-12 * (abs(ll) + 0.1)) break
    ll_old <- ll
  }
  list(b = f$b, mu = f$mu, theta = theta)
}

.sc_logit <- function(tau, Z, g) {
  for (it in 1:200) {
    eta <- as.vector(Z %*% g)
    pi_ <- 1 / (1 + exp(-eta))
    w <- pmax(pi_ * (1 - pi_), 1e-12)
    z <- eta + (tau - pi_) / w
    new <- .sc_wls(Z, z, w)
    done <- max(abs(new - g)) < 1e-12
    g <- new
    if (done) break
  }
  g
}

.sc_zill <- function(y, mu, pi_, theta) {
  s <- 0
  for (i in seq_along(y)) {
    if (is.null(theta)) {
      p0 <- exp(-mu[i])
      lc <- y[i] * log(mu[i]) - mu[i] - lgamma(y[i] + 1)
    } else {
      p0 <- (theta / (theta + mu[i]))^theta
      lc <- lgamma(y[i] + theta) - lgamma(theta) - lgamma(y[i] + 1) + theta * log(theta / (theta + mu[i])) +
        y[i] * log(mu[i] / (theta + mu[i]))
    }
    s <- s + if (y[i] == 0) log(pi_[i] + (1 - pi_[i]) * p0) else log(1 - pi_[i]) + lc
  }
  s
}

.sc_zifit <- function(y, X, Z, nb) {
  f <- .sc_glm(y, X)
  b <- f$b
  mu <- f$mu
  theta <- NULL
  g <- numeric(ncol(Z))
  ll_old <- -Inf
  ll <- -Inf
  for (it in 1:1000) {
    pi_ <- 1 / (1 + exp(-as.vector(Z %*% g)))
    p0 <- if (is.null(theta)) exp(-mu) else (theta / (theta + mu))^theta
    tau <- ifelse(y == 0, pi_ / (pi_ + (1 - pi_) * p0), 0)
    g <- .sc_logit(tau, Z, g)
    wt <- 1 - tau
    if (nb) {
      f <- .sc_glm(y, X, wt, if (is.null(theta)) 1 else theta, b)
      theta <- .sc_theta(y, f$mu, wt)
      f <- .sc_glm(y, X, wt, theta, f$b)
    } else {
      f <- .sc_glm(y, X, wt, NULL, b)
    }
    b <- f$b
    mu <- f$mu
    pi_ <- 1 / (1 + exp(-as.vector(Z %*% g)))
    ll <- .sc_zill(y, mu, pi_, theta)
    if (abs(ll - ll_old) <= 1e-11 * (abs(ll) + 0.1)) break
    ll_old <- ll
  }
  list(b = b, g = g, theta = theta, ll = ll)
}

.sc_golden <- function(f, lo, hi) {
  gr <- (sqrt(5) - 1) / 2
  x1 <- hi - gr * (hi - lo)
  x2 <- lo + gr * (hi - lo)
  f1 <- f(x1)
  f2 <- f(x2)
  for (it in 1:200) {
    if (f1 >= f2) {
      hi <- x2
      x2 <- x1
      f2 <- f1
      x1 <- hi - gr * (hi - lo)
      f1 <- f(x1)
    } else {
      lo <- x1
      x1 <- x2
      f1 <- f2
      x2 <- lo + gr * (hi - lo)
      f2 <- f(x2)
    }
    if (hi - lo < 1e-9) break
  }
  0.5 * (lo + hi)
}

#' @rdname SarPoisson
#' @export
SarNegbin <- function(y, X, W, rho_bounds = c(-0.99, 0.99)) {
  d <- .sc_prep(y, X, W)
  prof <- function(rho) {
    f <- .sc_nb(d$y, .sc_lag(d$X, d$W, rho))
    .sc_ll(d$y, f$mu, f$theta, rep(1, length(d$y)))
  }
  rho <- if (rho_bounds[1] == rho_bounds[2]) rho_bounds[1] else .sc_golden(prof, rho_bounds[1], rho_bounds[2])
  f <- .sc_nb(d$y, .sc_lag(d$X, d$W, rho))
  list(coefficients = f$b, rho = rho, theta = f$theta, fitted = f$mu,
       loglik = .sc_ll(d$y, f$mu, f$theta, rep(1, length(d$y))), k = length(f$b) + 2)
}

.sc_zi <- function(y, X, W, Z, rho_bounds, nb) {
  d <- .sc_prep(y, X, W)
  Zm <- if (is.null(Z)) matrix(1, length(d$y), 1) else as.matrix(Z) + 0
  prof <- function(rho) .sc_zifit(d$y, .sc_lag(d$X, d$W, rho), Zm, nb)$ll
  rho <- if (rho_bounds[1] == rho_bounds[2]) rho_bounds[1] else .sc_golden(prof, rho_bounds[1], rho_bounds[2])
  f <- .sc_zifit(d$y, .sc_lag(d$X, d$W, rho), Zm, nb)
  out <- list(count_coefficients = f$b, zero_coefficients = f$g, rho = rho, loglik = f$ll,
              k = length(f$b) + length(f$g) + 1)
  if (nb) {
    out$theta <- f$theta
    out$k <- out$k + 1
  }
  out
}

#' @rdname SarPoisson
#' @export
SarZip <- function(y, X, W, Z = NULL, rho_bounds = c(-0.99, 0.99)) .sc_zi(y, X, W, Z, rho_bounds, FALSE)

#' @rdname SarPoisson
#' @export
SarZinb <- function(y, X, W, Z = NULL, rho_bounds = c(-0.99, 0.99)) .sc_zi(y, X, W, Z, rho_bounds, TRUE)

#' @rdname SarPoisson
#' @export
SarPoissonLmTest <- function(y, X, W) {
  d <- .sc_prep(y, X, W)
  f <- .sc_glm(d$y, d$X)
  mu <- f$mu
  eta <- as.vector(d$X %*% f$b)
  dd <- as.vector(d$W %*% eta)
  s <- .sc_ss((d$y - mu) * dd)
  XDX <- crossprod(d$X * mu, d$X)
  XDd <- as.vector(crossprod(d$X, mu * dd))
  info <- .sc_ss(mu * dd * dd) - sum(XDd * solve(XDX, XDd))
  lm <- s * s / info
  list(statistic = lm, df = 1, p_value = stats::pchisq(lm, 1, lower.tail = FALSE), score = s)
}

#' @rdname SarPoisson
#' @export
CountModelIc <- function(loglik, k, n) {
  aic <- -2 * loglik + 2 * k
  list(AIC = aic, BIC = -2 * loglik + k * log(n), AICc = aic + 2 * k * (k + 1) / (n - k - 1))
}

#' @rdname SarPoisson
#' @export
BymVarianceFraction <- function(var_spatial, var_unstructured, Q = NULL) {
  .morie_arg(var_spatial, "n")
  s <- 1
  if (!is.null(Q)) {
    Q <- as.matrix(Q)
    n <- nrow(Q)
    Qi <- solve(Q + 1 / n)
    s <- exp(mean(log(diag(Qi) - 1 / n)))
  }
  list(fraction = s * var_spatial / (s * var_spatial + var_unstructured), scale = s)
}
