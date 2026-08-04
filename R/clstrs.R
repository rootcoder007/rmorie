# SPDX-License-Identifier: AGPL-3.0-or-later
#' Choose the cluster size that buys the most precision per unit cost.
#'
#' The optimum is reported both exactly and rounded to the better of its
#' two neighbouring integers, compared on the achieved variance rather
#' than on the rounding.
#'
#' Formula: k_opt = sqrt( c1 (1 - rho) / (c2 rho) );
#'   m = budget / (c1 + c2 k); V(ybar) = (S^2 / (m k)) [1 + (k - 1) rho]
#'
#' @param rho Intraclass correlation, 0 < rho <= 1.
#' @param S2 Element variance in the population.
#' @param c1 Cost of adding one cluster.
#' @param c2 Cost of adding one element within a cluster.
#' @param budget Total budget.
#' @return List with \code{k_opt}, \code{k}, \code{m}, \code{variance},
#'   \code{deff}, \code{cost}, \code{elements}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   9, which develops the design effect 1 + (k - 1) rho and optimises k
#'   against the linear cost function c1 m + c2 m k. Chapter 9 was NOT in
#'   the scanned excerpt available to this batch, so the standard
#'   published form is used.
#' @export
Clusdes <- function(rho, S2, c1, c2, budget) {
  rho <- as.numeric(rho); S2 <- as.numeric(S2)
  c1 <- as.numeric(c1); c2 <- as.numeric(c2); budget <- as.numeric(budget)
  if (rho <= 0 || rho > 1) stop("rho must satisfy 0 < rho <= 1")
  if (S2 <= 0) stop("S2 must be positive")
  if (c1 <= 0 || c2 <= 0) stop("costs must be positive")
  if (budget <= c1 + c2)
    stop("the budget cannot buy even one cluster of one")
  kopt <- if (rho < 1) sqrt(c1 * (1 - rho) / (c2 * rho)) else 1
  Vf <- function(kk) {
    mm <- budget / (c1 + c2 * kk)
    if (mm <= 0) return(c(Inf, mm))
    c((S2 / (mm * kk)) * (1 + (kk - 1) * rho), mm)
  }
  lo <- max(1, floor(kopt)); hi <- lo + 1
  k <- if (Vf(lo)[1] <= Vf(hi)[1]) lo else hi
  vm <- Vf(k)
  .t1_result(k_opt = kopt, k = as.numeric(k), m = vm[2], variance = vm[1],
             deff = 1 + (k - 1) * rho, cost = vm[2] * (c1 + c2 * k),
             elements = vm[2] * k,
             method = "Optimal cluster size under a linear cost function")
}
