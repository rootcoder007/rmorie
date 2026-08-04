# SPDX-License-Identifier: AGPL-3.0-or-later
#' BIC order selection for an autoregression, on the Schwarz penalty
#'
#' Schwarz, G. (1978), "Estimating the dimension of a model", The Annals of
#' Statistics 6(2), 461-464, doi:10.1214/aos/1176344136. The paper was opened
#' directly (Project Euclid) and its Proposition, page 462, read off a rendered
#' page image: \code{S(Y, n, j) = n sup(Y . theta - b(theta)) - (1/2) k_j log n
#' + R} with R bounded in n. The Bayes solution maximises S, so on the
#' log-likelihood scale the penalty is \code{(1/2) k log n} and on the usual
#' deviance scale it is \code{BIC = -2 log L_max + k log n}.
#'
#' That factor is the entire content of the criterion and the entire difference
#' from AIC: the penalty per parameter GROWS with the sample, log n against 2,
#' so BIC is consistent for the true order where AIC is not, and for any n > 7
#' it is the stricter of the two.
#'
#' For an autoregression of order p fitted by conditional least squares with an
#' intercept, k = p + 2 -- intercept, p coefficients and the innovation
#' variance. Every candidate order is fitted on the SAME \code{T = n - max_p}
#' observations, so the likelihoods being compared are likelihoods of the same
#' data; refitting each order on as many observations as it can use makes the
#' criteria incomparable, which is the standard way this selection is got wrong.
#'
#' Two scalings are returned. \code{bic_raw} is the Schwarz form
#' \code{-2 log L + k log T}. \code{bic} is the per-observation form quoted in
#' the time-series literature, \code{BIC(p) = log(sigma_p^2) + p log(T)/T},
#' which differs from \code{bic_raw / T} by \code{log(2 pi) + 1 + 2 log(T)/T} --
#' a constant in p -- and therefore selects the same order. The two are computed
#' independently here and their agreement on the argmin is a check, not an
#' assumption.
#'
#' @param x The series, in time order.
#' @param max_p Largest order considered; orders 0, 1, ..., max_p are compared.
#' @return List with \code{estimate} (the selected order), \code{order},
#'   \code{bic}, \code{bic_raw}, \code{sigma2}, \code{coefficients}, \code{n},
#'   \code{T}, \code{max_p}, \code{method}.
#' @references Schwarz, G. (1978), Annals of Statistics 6(2):461-464,
#'   doi:10.1214/aos/1176344136, Proposition, p. 462.
#' @examples
#' set.seed(1)
#' Bicarp(as.numeric(filter(rnorm(200), 0.7, method = "recursive")), 4)$order
#' @export
Bicarp <- function(x, max_p) {
  xv <- .s03vec(x)
  n <- length(xv)
  P <- as.integer(max_p)
  if (is.na(P) || P != max_p || P < 0L) {
    stop("bic_ar_order: max_p must be a non-negative integer")
  }
  Tn <- n - P
  if (Tn < P + 3L) {
    stop("bic_ar_order: too few observations; ", n, " points leave T = ", Tn,
         " for order ", P, ", which cannot support ", P + 2L, " parameters")
  }
  y <- xv[(P + 1L):n]
  logT <- log(Tn)
  bic <- numeric(P + 1L)
  bic_raw <- numeric(P + 1L)
  sig2 <- numeric(P + 1L)
  coefs <- vector("list", P + 1L)
  for (p in 0:P) {
    X <- matrix(1, Tn, p + 1L)
    if (p > 0L) {
      for (lag in seq_len(p)) {
        X[, lag + 1L] <- xv[(P + 1L - lag):(n - lag)]
      }
    }
    beta <- .s03lstsq(X, y, ridge = 0)
    rss <- sum((y - as.vector(X %*% beta))^2)
    s2 <- rss / Tn
    if (!(s2 > 0)) {
      stop("bic_ar_order: the order-", p, " fit is exact, so the Gaussian ",
           "likelihood is unbounded and no BIC exists")
    }
    sig2[p + 1L] <- s2
    coefs[[p + 1L]] <- as.numeric(beta)
    bic[p + 1L] <- log(s2) + p * logT / Tn
    # the Schwarz scale: k = p + 2 (intercept, p lags, sigma^2)
    bic_raw[p + 1L] <- Tn * (log(2 * pi * s2) + 1) + (p + 2L) * logT
  }
  best <- which.min(bic) - 1L
  best_raw <- which.min(bic_raw) - 1L
  if (best != best_raw) {
    stop("bic_ar_order: the two scalings of the criterion disagree on the ",
         "argmin, which is arithmetically impossible; the fit is degenerate")
  }
  list(estimate = as.integer(best), order = as.integer(best),
       bic = bic, bic_raw = bic_raw, sigma2 = sig2,
       coefficients = coefs[[best + 1L]],
       n = as.integer(n), T = as.integer(Tn), max_p = P,
       method = "Schwarz (1978) BIC = -2 log L + k log n, AR order selection")
}
