# SPDX-License-Identifier: AGPL-3.0-or-later
#' Replace the largest importance weights by fitted Pareto quantiles
#'
#' Truncating the big weights caps the variance but adds bias; leaving
#' them alone keeps it unbiased and useless. Smoothing replaces the
#' extreme weights by order statistics of a generalised Pareto fitted to
#' that same tail, stabilising the variance while keeping the shape the
#' data showed.
#'
#' Formula: with \code{M = min(0.2 S, 3 sqrt(S))}, substitute
#' \code{F^-1((z - 0.5)/M)} for the M largest weights, then cap at
#' \code{S^(3/4) mean(w)}.
#'
#' @param log_lik Pointwise log likelihood.
#' @return List with \code{estimate}, \code{k}, \code{weights},
#'   \code{S}, \code{n}.
#' @references Vehtari, Simpson, Gelman, Yao & Gabry (2024) JMLR 25:1-58.
#' @export
Loopr <- function(log_lik) {
  L <- as.matrix(log_lik); Sn <- nrow(L); n <- ncol(L)
  W <- matrix(0, Sn, n); ks <- numeric(n)
  for (i in seq_len(n)) {
    ps <- .s4_psis(-L[, i])
    ks[i] <- ps$k
    w <- exp(ps$lw - max(ps$lw))
    W[, i] <- w / sum(w)
  }
  .t1_result(estimate = max(ks), k = ks, weights = W, S = Sn, n = n,
             method = "Pareto-smoothed importance weights")
}
