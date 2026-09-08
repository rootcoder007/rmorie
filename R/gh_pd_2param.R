# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pitman-Yor stick-breaking weights and the Gibbs factor V_\{n,k\}
#'
#' sigma = 0 recovers DP(M, G); sigma > 0 makes the weights decay
#' polynomially. The weights returned are EXPECTED weights, not a draw,
#' so the result is deterministic in both language arms.
#'
#' Formula: V_j ~ Beta(1 - sigma, M + j sigma);
#'   W_j = V_j prod_\{l<j\}(1 - V_l);
#'   E\[V_j\] = (1 - sigma)/(M + 1 + (j - 1) sigma);
#'   V_\{n,k\} = prod_\{i=1\}^\{k-1\}(M + i sigma) / (M + 1)^\{\[n-1\]\}
#'
#' @param sigma Discount parameter; restricted to [0, 1).
#' @param M Concentration parameter, M > -sigma.
#' @param k Number of weights returned.
#' @param n Optional sample size for V_\{n,k\}.
#' @return List with \code{weights}, \code{expected_stick},
#'   \code{remaining}, \code{log_Vnk}, \code{Vnk}, \code{sigma},
#'   \code{M}, \code{k}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, Section 14.4, Definition 14.31,
#'   equation (14.20). Read from the copy of the book held in the corpus.
#'   Only the sigma in [0, 1) branch is implemented; the negative-sigma
#'   branch is a finite-support family and is refused.
#' @export
#' @examples
#' Poisdir(sigma = 0.5, M = 5L, k = 5L)
Poisdir <- function(sigma, M, k, n = NULL) {
  sigma <- as.numeric(sigma)
  M <- as.numeric(M)
  k <- as.integer(k)
  if (sigma < 0 || sigma >= 1)
    stop("only the sigma in [0, 1) branch is implemented; the negative-sigma branch has finite support and is refused")
  if (M <= -sigma) stop("M must exceed -sigma")
  if (k < 1L) stop("k must be at least 1")
  j <- seq_len(k)
  ev <- (1 - sigma) / (M + 1 + (j - 1) * sigma)
  w <- numeric(k)
  rest <- 1
  for (i in j) { w[i] <- rest * ev[i]
  rest <- rest * (1 - ev[i]) }
  lv <- NaN
  vv <- NaN
  if (!is.null(n)) {
    n <- as.integer(n)
    if (n < 1L) stop("n must be at least 1")
    if (k > n) stop("k cannot exceed n")
    num <- if (k > 1L) sum(log(M + seq_len(k - 1L) * sigma)) else 0
    den <- if (n > 1L) sum(log(M + 1 + (seq_len(n - 1L) - 1L))) else 0
    lv <- num - den
    vv <- exp(lv)
  }
  .t1_result(weights = w, expected_stick = ev, remaining = rest,
             log_Vnk = lv, Vnk = vv, sigma = sigma, M = M,
             k = as.numeric(k),
             method = "Pitman-Yor weights and V_{n,k}, Ghosal Definition 14.31")
}
