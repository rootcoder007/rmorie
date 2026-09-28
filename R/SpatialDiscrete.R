.sd_cdf <- function(e, link) if (link == "logit") 1 / (1 + exp(-e)) else stats::pnorm(e)

.sd_pdf <- function(e, link) {
  if (link == "logit") {
    p <- 1 / (1 + exp(-e))
    return(p * (1 - p))
  }
  stats::dnorm(e)
}

.sd_ls <- function(G, u) as.vector(solve(crossprod(G), crossprod(G, u)))

.sd_project <- function(Z, v) as.vector(Z %*% .sd_ls(Z, v))

.sd_instruments <- function(X, W, Z) if (is.null(Z)) cbind(X, W %*% X[, -1, drop = FALSE]) else as.matrix(Z)

#' Spatial binary-choice models
#'
#' \code{BinaryGlm}: logit or probit by iteratively reweighted least squares.
#' \code{SpatialLogitGmm}: Klier-McMillen linearized GMM spatial logit with
#' HC3 standard errors. \code{SpatialProbitGmm}: Pinkse-Slade GMM spatial
#' autoregressive probit with McMillen's Gauss-Newton iteration.
#' \code{DiscreteMoranTest}: Kelejian-Prucha Moran test on generalized
#' residuals. Identical to the Python arm \code{morie.fn.spdiscrete}.
#'
#' @param y Binary response.
#' @param X Design matrix with the intercept column first.
#' @param W Spatial weights.
#' @param link \code{"logit"} or \code{"probit"}.
#' @param tol Convergence tolerance.
#' @param maxit Iteration limit.
#' @param Z Optional instrument matrix (default X and W X).
#' @param start_rho Starting rho.
#' @return A list.
#' @references Klier, T. and McMillen, D. P. (2008). Clustering of auto
#'   supplier plants in the United States. Journal of Business and Economic
#'   Statistics 26, 460-471.
#'
#'   Pinkse, J. and Slade, M. E. (1998). Contracting in space. Journal of
#'   Econometrics 85, 125-154.
#'
#'   Kelejian, H. H. and Prucha, I. R. (2001). On the asymptotic distribution
#'   of the Moran I test statistic with applications. Journal of Econometrics
#'   104, 219-257.
#' @examples
#' X <- cbind(1, c(2, -1, 0.5, 1.5, 0.3, -0.4))
#' BinaryGlm(c(1, 0, 1, 1, 0, 0), X)$coefficients
#' @export
BinaryGlm <- function(y, X, link = "logit", tol = 1e-12, maxit = 100) {
  X <- as.matrix(X)
  beta <- numeric(ncol(X))
  dev_old <- Inf
  for (it in seq_len(maxit)) {
    eta <- as.vector(X %*% beta)
    mu <- pmin(pmax(.sd_cdf(eta, link), 1e-15), 1 - 1e-15)
    d <- .sd_pdf(eta, link)
    w <- d^2 / (mu * (1 - mu))
    z <- eta + (y - mu) / d
    beta <- as.vector(solve(crossprod(X * w, X), crossprod(X * w, z)))
    eta <- as.vector(X %*% beta)
    mu <- pmin(pmax(.sd_cdf(eta, link), 1e-15), 1 - 1e-15)
    dev <- -2 * sum(y * log(mu) + (1 - y) * log(1 - mu))
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < tol) break
    dev_old <- dev
  }
  list(coefficients = beta, fitted = mu, linear_predictor = eta, deviance = dev)
}

.sd_hc3 <- function(G, u, coef) {
  B <- solve(crossprod(G))
  e <- as.vector(u - G %*% coef)
  h <- rowSums((G %*% B) * G)
  V <- B %*% crossprod(G * (e / (1 - h)), G * (e / (1 - h))) %*% B
  sqrt(diag(V))
}

#' @rdname BinaryGlm
#' @export
SpatialLogitGmm <- function(y, X, W, Z = NULL) {
  X <- as.matrix(X)
  k <- ncol(X)
  fit <- BinaryGlm(y, X, "logit")
  p <- fit$fitted
  g <- p * (1 - p)
  Zm <- .sd_instruments(X, W, Z)
  cols <- cbind(g * X, g * as.vector(W %*% fit$linear_predictor))
  u <- y - p + as.vector((g * X) %*% fit$coefficients)
  G <- apply(cols, 2, .sd_project, Z = Zm)
  coef <- .sd_ls(G, u)
  list(coefficients = coef[1:k], rho = coef[k + 1], se = .sd_hc3(G, u, coef), logit = fit$coefficients)
}

#' @rdname BinaryGlm
#' @export
SpatialProbitGmm <- function(y, X, W, Z = NULL, start_rho = 0, tol = 1e-10, maxit = 500) {
  X <- as.matrix(X)
  n <- nrow(X)
  k <- ncol(X)
  Zm <- .sd_instruments(X, W, Z)
  b <- c(BinaryGlm(y, X, "probit")$coefficients, start_rho)
  Mid0 <- W + t(W)
  WtW <- crossprod(W)
  step <- function(bb) {
    beta <- bb[1:k]
    rho <- bb[k + 1]
    Ai <- solve(diag(n) - rho * W)
    V <- tcrossprod(Ai)
    sv <- sqrt(diag(V))
    AX <- (Ai %*% X) / sv
    xs <- as.vector(AX %*% beta)
    cp <- stats::pnorm(xs)
    u <- (y - cp) * stats::dnorm(xs) / (cp * (1 - cp))
    g1 <- as.vector(Ai %*% (W %*% xs))
    g2 <- diag(V %*% (Mid0 - 2 * rho * WtW) %*% V) * xs / (2 * sv^2)
    du <- u * (xs + u)
    G <- apply(cbind(du * AX, du * (g1 - g2)), 2, .sd_project, Z = Zm)
    list(ch = .sd_ls(G, u), G = G, u = u)
  }
  it <- 0
  ch <- numeric(k + 1)
  repeat {
    it <- it + 1
    b <- b + ch
    s <- step(b)
    ch <- s$ch
    if ((max(abs(ch)) <= tol && it > 1) || it >= maxit) break
  }
  gg <- solve(crossprod(s$G))
  Gu <- abs(s$u) * s$G
  list(coefficients = b[1:k], rho = b[k + 1], se = sqrt(diag(gg %*% crossprod(Gu) %*% gg)), iterations = it)
}

#' @rdname BinaryGlm
#' @export
DiscreteMoranTest <- function(y, X, W, link = "probit") {
  fit <- BinaryGlm(y, as.matrix(X), link)
  F <- fit$fitted
  if (link == "logit") {
    u <- y - F
    s2 <- F * (1 - F)
  } else {
    f <- stats::dnorm(fit$linear_predictor)
    u <- (y - F) * f / (F * (1 - F))
    s2 <- f^2 / (F * (1 - F))
  }
  num <- sum(u * as.vector(W %*% u))
  M <- (W^2 + W * t(W)) * outer(s2, s2)
  diag(M) <- 0
  z <- num / sqrt(sum(M))
  list(statistic = z, p_value = 2 * stats::pnorm(-abs(z)), numerator = num, variance = sum(M))
}
