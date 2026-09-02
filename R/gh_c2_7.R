# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bernstein-Feller approximation of a CDF
#'
#' F_K(x) = sum_k F(k/K) C(K, k) x^k (1 - x)^(K-k).  The Bernstein
#' operator applied to a CDF converges UNIFORMLY, and it maps CDFs to
#' CDFs, which is why it turns a prior on the finite vector
#' (F(0/K), ..., F(K/K)) into a prior on genuine distribution functions.
#'
#' Formula: as displayed; the reported error is the sup over the supplied
#'   points of |F_K(x) - F(x)|.
#'
#' @param x Evaluation points; clipped to [0, 1].
#' @param F A CDF on [0, 1]; t^2 when NULL.
#' @param K Bernstein degree, at least 1.
#' @return List with \code{estimate} (value at the middle point),
#'   \code{F_K}, \code{sup_error}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 2.3.4.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalbernsteinfeller(V)
Ghosalbernsteinfeller <- function(x, F = NULL, K = 30) {
  xs <- as.numeric(x)
  K <- as.integer(K)
  if (length(xs) == 0L) stop("x must be non-empty")
  if (K < 1L) stop("K must be at least 1")
  if (is.null(F)) F <- function(t) t * t
  k <- 0:K
  Fk <- vapply(k / K, F, numeric(1))
  cl <- pmin(pmax(xs, 0), 1)
  vals <- vapply(cl, function(u)
    sum(Fk * choose(K, k) * u^k * (1 - u)^(K - k)), numeric(1))
  err <- max(abs(vals - vapply(cl, F, numeric(1))))
  .t1_result(estimate = vals[length(vals) %/% 2L + 1L], F_K = vals,
             sup_error = err,
             method = "Bernstein-Feller CDF approximation (GvdV 2017 sec. 2.3.4)")
}
