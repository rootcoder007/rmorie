.sr_logdet <- function(W, a) as.numeric(determinant(diag(nrow(W)) - a * W, logarithm = TRUE)$modulus)

.sr_profile <- function(y, X, W, rho, lam) {
  yl <- y - rho * as.vector(W %*% y)
  if (lam != 0) {
    ys <- yl - lam * as.vector(W %*% yl)
    Xs <- X - lam * (W %*% X)
  } else {
    ys <- yl
    Xs <- X
  }
  b <- as.vector(solve(crossprod(Xs), crossprod(Xs, ys)))
  e <- ys - as.vector(Xs %*% b)
  list(beta = b, s2 = sum(e^2) / length(y))
}

#' Spatial lag, spatial error and SAC models by maximum likelihood
#'
#' \code{lag}: \eqn{y = \rho W y + X\beta + e}; \code{error}: \eqn{y =
#' X\beta + u}, \eqn{u = \lambda W u + e}; \code{sac}: both. Profiling
#' \eqn{\beta} and \eqn{\sigma^2} leaves \eqn{-2\log L = n\log(2\pi\sigma^2)
#' + n - 2\log|I - \rho W| - 2\log|I - \lambda W|}, maximised by
#' \code{stats::optimize} (lag, error) or by alternating one-dimensional
#' searches (SAC); log-determinants by LU decomposition. Standard errors:
#' the analytic information matrix for lag and error (Ord 1975; Anselin
#' 1988), a central-difference Hessian of the full log-likelihood for SAC.
#' These match \code{spatialreg::lagsarlm}, \code{errorsarlm} and
#' \code{sacsarlm} with \code{method = "LU"}.
#'
#' @param y Response.
#' @param X Design matrix including any intercept column.
#' @param W Spatial weights matrix.
#' @param model \code{"lag"}, \code{"error"} or \code{"sac"}.
#' @param interval Search interval for the autoregressive parameters.
#' @return List with \code{beta}, \code{rho} and/or \code{lambda},
#'   \code{sigma2}, \code{loglik}, \code{aic}, \code{bic}, \code{se}.
#' @references Ord, K. (1975). Estimation methods for models of spatial
#'   interaction. Journal of the American Statistical Association 70,
#'   120-126.
#'
#'   Anselin, L. (1988). Spatial Econometrics: Methods and Models. Kluwer.
#' @examples
#' W <- matrix(c(0, .5, 0, .5, .5, 0, .5, 0, 0, .5, 0, .5, .5, 0, .5, 0), 4)
#' SpatialRegressionML(c(1, 2, 1.5, 3), cbind(1, c(.1, .5, .2, .9)), W, model = "error")$loglik
#' @export
SpatialRegressionML <- function(y, X, W, model = c("lag", "error", "sac"), interval = c(-0.999, 0.999)) {
  model <- match.arg(model)
  y <- as.numeric(y)
  X <- as.matrix(X)
  W <- as.matrix(W)
  n <- length(y)
  p <- ncol(X)
  nll <- function(rho, lam) {
    s2 <- .sr_profile(y, X, W, rho, lam)$s2
    v <- 0.5 * n * log(2 * pi * s2) + 0.5 * n
    if (rho != 0) v <- v - .sr_logdet(W, rho)
    if (lam != 0) v <- v - .sr_logdet(W, lam)
    v
  }
  tol <- 1e-12
  rho <- 0
  lam <- 0
  if (model == "lag") {
    rho <- stats::optimize(function(a) nll(a, 0), interval, tol = tol)$minimum
  } else if (model == "error") {
    lam <- stats::optimize(function(a) nll(0, a), interval, tol = tol)$minimum
  } else {
    f <- nll(0, 0)
    for (it in 1:200) {
      rho <- stats::optimize(function(a) nll(a, lam), interval, tol = tol)$minimum
      o <- stats::optimize(function(a) nll(rho, a), interval, tol = tol)
      lam <- o$minimum
      if (abs(f - o$objective) < 1e-13 * max(1, abs(o$objective))) break
      f <- o$objective
    }
  }
  pr <- .sr_profile(y, X, W, rho, lam)
  beta <- pr$beta
  s2 <- pr$s2
  loglik <- -nll(rho, lam)
  if (model %in% c("lag", "error")) {
    a <- if (model == "lag") rho else lam
    WA <- W %*% solve(diag(n) - a * W)
    trWA <- sum(diag(WA))
    trWA2 <- sum(WA * t(WA))
    trWtW <- sum(WA^2)
    if (model == "lag") {
      g <- as.vector(WA %*% (X %*% beta))
      I <- matrix(0, p + 2, p + 2)
      I[1:p, 1:p] <- crossprod(X) / s2
      I[1:p, p + 1] <- I[p + 1, 1:p] <- crossprod(X, g) / s2
      I[p + 1, p + 1] <- trWA2 + trWtW + sum(g^2) / s2
      I[p + 1, p + 2] <- I[p + 2, p + 1] <- trWA / s2
      I[p + 2, p + 2] <- n / (2 * s2^2)
      se <- sqrt(diag(solve(I)))
    } else {
      Xs <- X - lam * (W %*% X)
      L <- rbind(c(trWA2 + trWtW, trWA / s2), c(trWA / s2, n / (2 * s2^2)))
      se <- c(sqrt(diag(solve(crossprod(Xs) / s2))), sqrt(diag(solve(L))))
    }
  } else {
    theta <- c(beta, rho, lam, s2)
    fn <- function(t) {
      b <- t[1:p]
      r <- t[p + 1]
      l <- t[p + 2]
      sg <- t[p + 3]
      res <- (y - r * as.vector(W %*% y)) - as.vector(X %*% b)
      res <- res - l * as.vector(W %*% res)
      0.5 * n * log(2 * pi * sg) + sum(res^2) / (2 * sg) - .sr_logdet(W, r) - .sr_logdet(W, l)
    }
    k <- length(theta)
    h <- 1e-4 * pmax(1, abs(theta))
    H <- matrix(0, k, k)
    f0 <- fn(theta)
    for (a in seq_len(k)) {
      for (b in a:k) {
        if (a == b) {
          e <- numeric(k)
          e[a] <- h[a]
          H[a, a] <- (fn(theta + e) - 2 * f0 + fn(theta - e)) / h[a]^2
        } else {
          ea <- numeric(k)
          eb <- numeric(k)
          ea[a] <- h[a]
          eb[b] <- h[b]
          H[a, b] <- H[b, a] <- (fn(theta + ea + eb) - fn(theta + ea - eb) - fn(theta - ea + eb) + fn(theta - ea - eb)) / (4 * h[a] * h[b])
        }
      }
    }
    se <- sqrt(diag(solve(H)))
  }
  npar <- p + (model == "sac") + 2
  out <- list(beta = beta, sigma2 = s2, loglik = loglik, aic = -2 * loglik + 2 * npar,
              bic = -2 * loglik + log(n) * npar, se = se, model = model)
  if (model %in% c("lag", "sac")) out$rho <- rho
  if (model %in% c("error", "sac")) out$lambda <- lam
  out
}
