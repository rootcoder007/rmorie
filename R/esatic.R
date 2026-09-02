# SPDX-License-Identifier: AGPL-3.0-or-later
#' EAP ability estimate with its posterior sd and Fisher information
#'
#' The posterior sd shrinks towards the prior and stays finite for an
#' all-correct pattern; 1/sqrt(I(theta)) is the frequentist error of the
#' ML estimate and diverges there. Both are returned. The quadrature grid
#' is FIXED (trapezoid), never adaptive.
#'
#' Formula: e = exp(D a (theta - b)); P = c + (d - c) e/(1 + e);
#'   dP = D a e (d - c)/(1 + e)^2; I_j = dP^2/(P(1 - P));
#'   EAP = int theta pi L dtheta / int pi L dtheta
#'
#' @param items Item parameters (a, b, c, d), one row per item.
#' @param x Responses in \{0, 1\}.
#' @param D Scaling constant.
#' @param prior_mean,prior_sd Normal prior on theta.
#' @param lower,upper Quadrature range.
#' @param nqp Number of quadrature points.
#' @return List with \code{estimate}, \code{se}, \code{information},
#'   \code{se_ml}, \code{item_information}, \code{prob}, \code{J},
#'   \code{nqp}.
#' @references Verified against the reference implementation in the CRAN
#'   package catR 3.17 (Magis & Raiche), functions Pi, Ii and eapEst.
#'   catR implements the procedures of van der Linden & Pashley, Item
#'   selection and ability estimation in adaptive testing, in Elements of
#'   Adaptive Testing (2010), which this row cites; that chapter was NOT
#'   obtainable, so the package source is used as the reference.
#' @export
#' @examples
#' items <- matrix(c(1, 0, 0.2, 0.95, 1.2, -0.5, 0.1, 0.98, 0.8, 1, 0.15, 0.9),
#'                 3, 4, byrow = TRUE)
#' Eapinfo(items, x = c(1, 0, 1))
Eapinfo <- function(items, x, D = 1, prior_mean = 0, prior_sd = 1,
                    lower = -4, upper = 4, nqp = 33) {
  It <- as.matrix(items); J <- nrow(It)
  if (J < 1L) stop("at least one item is required")
  if (ncol(It) != 4L) stop("item rows must be (a, b, c, d)")
  x <- .t1_vec(x)
  if (length(x) != J) stop("one response per item is required")
  if (any(!(x %in% c(0, 1)))) stop("responses must be 0 or 1")
  ps <- as.numeric(prior_sd)
  if (ps <= 0) stop("the prior sd must be positive")
  nq <- as.integer(nqp)
  if (nq < 3L) stop("at least three quadrature points are required")
  h <- (upper - lower) / (nq - 1)
  grid <- lower + (seq_len(nq) - 1) * h
  probs <- function(th) {
    e <- exp(D * It[, 1] * (th - It[, 2]))
    p <- It[, 3] + (It[, 4] - It[, 3]) * e / (1 + e)
    pmin(1 - 1e-10, pmax(1e-10, p))
  }
  den <- num <- numeric(nq)
  for (i in seq_len(nq)) {
    p <- probs(grid[i])
    L <- prod(ifelse(x == 1, p, 1 - p))
    pr <- exp(-0.5 * ((grid[i] - prior_mean) / ps)^2) / ps
    den[i] <- pr * L
    num[i] <- grid[i] * pr * L
  }
  trap <- function(v) h * (0.5 * v[1] + sum(v[-c(1, length(v))]) +
                           0.5 * v[length(v)])
  d0 <- trap(den)
  if (d0 <= 0) stop("the posterior integrated to zero; check the grid")
  eap <- trap(num) / d0
  var <- trap(grid^2 * den) / d0 - eap^2
  p <- probs(eap)
  e <- exp(D * It[, 1] * (eap - It[, 2]))
  dp <- D * It[, 1] * e * (It[, 4] - It[, 3]) / (1 + e)^2
  info <- dp^2 / (p * (1 - p))
  tot <- sum(info)
  .t1_result(estimate = eap, se = if (var > 0) sqrt(var) else 0,
             information = tot,
             se_ml = if (tot > 0) 1 / sqrt(tot) else Inf,
             item_information = info, prob = p, J = as.numeric(J),
             nqp = as.numeric(nq),
             method = "EAP with posterior sd and test information")
}
