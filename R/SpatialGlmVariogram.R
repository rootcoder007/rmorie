#' Spatial GLM, robust semivariogram, loss and weight normalisation
#'
#' R arm of \code{morie.fn.sglm}, \code{sgcrh}, \code{sglss} and
#' \code{sgrwn}. \code{Sglm}: spatial linear model by ML (\code{\link{Likfit}})
#' with generalised-least-squares standard errors for \code{family =
#' "gaussian"}, or the Laplace-approximation spatial GLMM
#' (\code{\link{SpatialGlmmFit}}) for Poisson and binomial responses.
#' \code{Sgcrh}: Cressie-Hawkins robust semivariogram, with the
#' 0.045/N^2 term of Cressie (1993) or without it (\code{method = "gstat"}),
#' gstat's default bins. \code{Sglss}: mean squared error loss.
#' \code{Sgrwn}: row-standardised weights (zero rows kept).
#'
#' @param x Design matrix (include an intercept column for an intercept).
#' @param y Responses.
#' @param coords Locations (n x 2, or a vector for points on a line).
#' @param family \code{"gaussian"}, \code{"poisson"} or \code{"binomial"}.
#' @param model Correlation model.
#' @param data Observations.
#' @param lags Number of distance bins.
#' @param cutoff Largest distance considered.
#' @param method \code{"cressie"} or \code{"gstat"}.
#' @param predicted,observed Predictions and observations.
#' @param W Weights matrix.
#' @return A list (the Python result's fields).
#' @references Cressie, N. and Hawkins, D. M. (1980). Robust estimation of
#'   the variogram: I. Mathematical Geology 12, 115-125.
#'
#'   Cressie, N. (1993). Statistics for Spatial Data, revised edition.
#'   Wiley.
#'
#'   Schabenberger, O. and Gotway, C. A. (2005). Statistical Methods for
#'   Spatial Data Analysis. Chapman and Hall/CRC.
#' @examples
#' xy <- rbind(c(0, 0), c(1, 0.2), c(2.1, 0), c(0.1, 1), c(1.2, 1.1), c(2, 0.9))
#' Sgcrh(c(1, 2.4, 1.3, 3.1, 1.9, 2.2), xy, lags = 3, cutoff = 2)$gamma
#' Sgrwn(rbind(c(0, 2, 2), c(1, 0, 0), c(0, 0, 0)))$W_normalized
#' @export
Sglm <- function(x, y, coords, family = "gaussian", model = "Exp") {
  X <- as.matrix(x)
  y <- as.numeric(y)
  P <- as.matrix(coords)
  if (ncol(P) == 1) P <- cbind(P, 0)
  n <- nrow(X)
  if (length(y) != n || nrow(P) != n) stop("shape mismatch among x, y, coords")
  D <- as.matrix(stats::dist(P))
  if (family == "gaussian") {
    f <- Likfit(y, P, list(model = model, range = max(D) / 3), X = X)
    V <- matrix(vapply(D, function(h) KrigingCovariance(h, list(model = model, psill = f$psill, range = f$range)),
                       0), n, n) + diag(f$nugget, n)
    C <- solve(crossprod(X, solve(V, X)))
    return(list(estimate = f$beta, se = sqrt(diag(C)), sigma2 = f$psill, phi = f$range, tau2 = f$nugget,
                loglik = f$loglik, n = n))
  }
  if (!family %in% c("poisson", "binomial")) stop("family must be 'gaussian', 'poisson' or 'binomial'")
  g <- SpatialGlmmFit(y, X, family = family, coords = P, model = model)
  list(estimate = g$beta, se = NULL, sigma2 = g$sigma2, phi = g$range, tau2 = 0, loglik = g$loglik, n = n)
}

#' @rdname Sglm
#' @export
Sgcrh <- function(data, coords, lags = 15, cutoff = NULL, method = "cressie") {
  if (!method %in% c("cressie", "gstat")) stop("method must be 'cressie' or 'gstat'")
  P <- as.matrix(coords)
  if (ncol(P) == 1) P <- cbind(P, 0)
  if (is.null(cutoff)) cutoff <- sqrt(sum(apply(P, 2, function(v) diff(range(v)))^2)) / 3
  w <- cutoff / lags
  d <- as.matrix(stats::dist(P))
  pr <- which(upper.tri(d) & d <= cutoff & d > 0, arr.ind = TRUE)
  k <- pmin(ceiling(d[pr] / w), lags)
  a <- abs(data[pr[, 1]] - data[pr[, 2]])^0.5
  N <- tabulate(k, lags)
  keep <- which(N > 0)
  m <- vapply(keep, function(j) sum(a[k == j]) / N[j], 0)
  corr <- 0.457 + 0.494 / N[keep] + (if (method == "cressie") 0.045 / N[keep]^2 else 0)
  gam <- m^4 / corr / 2
  list(statistic = if (length(gam)) gam[1] else NaN, gamma = gam, np = N[keep],
       dist = vapply(keep, function(j) sum(d[pr][k == j]) / N[j], 0), cutoff = cutoff, width = w)
}

#' @rdname Sglm
#' @export
Sglss <- function(predicted, observed) {
  l <- (as.numeric(observed) - as.numeric(predicted))^2
  list(statistic = mean(l), losses = l, total_loss = sum(l))
}

#' @rdname Sglm
#' @export
Sgrwn <- function(W) {
  W <- as.matrix(W)
  rs <- rowSums(W)
  rs[rs == 0] <- 1
  Wn <- W / rs
  list(statistic = mean(rowSums(Wn)), W_normalized = Wn)
}
