.gwrb_kernel <- function(d, b, kernel) {
  switch(kernel,
    gaussian = exp(d^2 / (-2 * b^2)),
    exponential = exp(-d / b),
    bisquare = ifelse(d > b, 0, (1 - d^2 / b^2)^2),
    tricube = ifelse(d > b, 0, (1 - d^3 / b^3)^3),
    boxcar = ifelse(d > b, 0, 1),
    stop("unknown kernel")
  )
}

#' Geographical kernel weights of one regression location
#'
#' As \code{GWmodel::gw.weight}: with \code{adaptive} the bandwidth is the
#' \code{bw}-th smallest distance (self included), or \code{bw/n} times the
#' largest when \code{bw > n}.
#'
#' @param dists Distances from the location to every data point.
#' @param bw Bandwidth (distance, or neighbour count when adaptive).
#' @param kernel \code{"gaussian"}, \code{"exponential"}, \code{"bisquare"},
#'   \code{"tricube"} or \code{"boxcar"}.
#' @param adaptive Adaptive bandwidth.
#' @return Numeric vector of weights.
#' @examples
#' GWRKernelWeights(c(0, 1, 2, 3), 2.5)
#' @export
GWRKernelWeights <- function(dists, bw, kernel = "bisquare", adaptive = FALSE) {
  d <- as.numeric(dists)
  b <- if (adaptive) {
    if (bw / length(d) <= 1) sort(d)[as.integer(bw)] else bw / length(d) * max(d)
  } else {
    bw
  }
  .gwrb_kernel(d, b, kernel)
}

#' Geographically weighted regression with its diagnostics
#'
#' Local weighted least squares \eqn{\beta_i = (X'W_iX)^{-1}X'W_iy} with
#' \eqn{C_i = (X'W_iX)^{-1}X'W_i}, hat matrix rows \eqn{x_i'C_i},
#' \eqn{\sigma^2 = RSS/(n - 2trS + trS'S)}, standard errors
#' \eqn{\sqrt{\sigma^2 diag(C_iC_i')}}, AICc, edf, enp and local R2 exactly as
#' \code{GWmodel::gwr.basic} (Euclidean distances; Brunsdon, Fotheringham
#' and Charlton 1996).
#'
#' @param y Response.
#' @param X Design matrix including any intercept column.
#' @param coords Two-column matrix of coordinates.
#' @param bw Bandwidth.
#' @param kernel Kernel name.
#' @param adaptive Adaptive bandwidth.
#' @return List with \code{betas}, \code{se}, \code{tvalues}, \code{fitted},
#'   \code{residuals}, \code{local_r2}, \code{diagnostics}.
#' @references Brunsdon, C., Fotheringham, A. S. and Charlton, M. E. (1996).
#'   Geographically weighted regression: a method for exploring spatial
#'   nonstationarity. Geographical Analysis 28, 281-298.
#'
#'   Fotheringham, A. S., Brunsdon, C. and Charlton, M. (2002).
#'   Geographically Weighted Regression. Wiley, Chichester.
#' @examples
#' P <- as.matrix(expand.grid(y = 0:3, x = 0:3)[, 2:1])
#' X <- cbind(1, (0.3 * (0:15)) %% 1.7)
#' y <- 1 + 2 * X[, 2] + 0.1 * P[, 1]
#' GWRBasic(y, X, P, 3, kernel = "gaussian")$diagnostics$AICc
#' @export
GWRBasic <- function(y, X, coords, bw, kernel = "bisquare", adaptive = FALSE) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  P <- as.matrix(coords)
  n <- length(y)
  p <- ncol(X)
  D <- as.matrix(stats::dist(P))
  Wm <- sapply(seq_len(n), function(i) GWRKernelWeights(D[, i], bw, kernel, adaptive))
  betas <- se2 <- matrix(0, n, p)
  S <- matrix(0, n, n)
  for (i in seq_len(n)) {
    w <- Wm[, i]
    xtw <- t(X * w)
    Ci <- solve(xtw %*% X) %*% xtw
    betas[i, ] <- Ci %*% y
    se2[i, ] <- rowSums(Ci^2)
    S[i, ] <- X[i, ] %*% Ci
  }
  fitted <- rowSums(X * betas)
  resid <- y - fitted
  rss <- sum(resid^2)
  trS <- sum(diag(S))
  trStS <- sum(S^2)
  edf <- n - 2 * trS + trStS
  s2 <- rss / edf
  se <- sqrt(s2 * se2)
  dyb <- (y - mean(y))^2
  tssw <- as.vector(Wm %*% dyb)
  rssw <- as.vector(Wm %*% resid^2)
  r2 <- 1 - rss / sum(dyb)
  lg <- n * log(rss / n) + n * log(2 * pi)
  list(betas = betas, se = se, tvalues = betas / se, fitted = fitted, residuals = resid,
       local_r2 = (tssw - rssw) / tssw,
       diagnostics = list(RSS = rss, AIC = lg + n + trS, AICc = lg + n * (n + trS) / (n - 2 - trS),
                          BIC = lg + log(n) * trS, edf = edf, enp = 2 * trS - trStS, R2 = r2,
                          R2_adj = 1 - (1 - r2) * (n - 1) / (edf - 1), trS = trS, trStS = trStS, sigma2 = s2))
}
