# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of tqipb -- inner-product distortion bounds for TurboQuant-packed
# blocks. Mirrors src/morie/fn/tqipb.py operation for operation.
#
# TurboQuant (Zandieh, Han, Daliri and Karbasi, 2025) rotates a vector at
# random before quantizing it. The rotation is what makes the scheme
# analysable: a random rotation of any unit vector has coordinates
# distributed Beta((d-1)/2, (d-1)/2) rescaled to [-1, 1] -- the same law
# for every input -- so ONE scalar quantizer designed against that law is
# optimal for every input rather than for a training set. The constants
# below are that quantizer's, not a fit.
#
# What the paper gives, and what this module does with it, are kept apart.
#
# From the paper
#   D_prod <= sqrt(3) * pi^2 * ||y||^2 / (d * 4^b)          (Theorem 2)
#       the high-resolution (Panter-Dite) form, loose at b = 1.
#   D_prod >= ||y||^2 / (d * 4^b)                           (Theorem 3)
#       no randomized quantizer at bit-width b beats this on the worst
#       input. The gap is sqrt(3) * pi^2, about 17.1 here.
#   D_prod ~= {1.57, 0.56, 0.18, 0.047} * ||y||^2 / d       (table, b=1..4)
#       the ACTUAL distortion of the optimal b-bit quantizer for the Beta
#       law, from solving the continuous 1-D k-means problem. At one bit
#       that is 1.57 against Panter-Dite's 4.27, which is the size of the
#       error the asymptotic form makes there.
#   Var <= pi * ||y||^2 / (2 * d)                           (Lemma 4)
#       the one-bit QJL transform applied to the residual.
#
# Not from the paper
#   The paper states distortions -- second moments -- and does NOT state
#   a concentration inequality
#       Pr[|<x_hat, y_hat> - <x, y>| > eps ||x|| ||y||] <= delta.
#   Turning a variance into one needs a tail inequality, and which one is
#   a modelling choice, so both are offered and the choice is reported:
#     "chebyshev"     delta = V / (eps^2 ||x||^2 ||y||^2); assumption-free
#                     given the variance, and correspondingly weak.
#     "sub_gaussian"  delta = 2 exp(-eps^2 ||x||^2 ||y||^2 / (2 V));
#                     requires a sub-Gaussian error, which is an
#                     assumption and is labelled as one.
#
# Packed blocks
#   Each block is rotated and quantized on its own, so the per-block
#   errors are independent and the VARIANCES add:
#       V = sum_j c(b, d_j) ||x_j||^2 ||y_j||^2
#   Under the proportional split used here this is EXACTLY neutral:
#   every route's constant goes as 1/d_j while a block of d_j
#   coordinates carries (d_j/d)^2 of the norm product, so the two cancel
#   and the total is c(b, d) ||x||^2 ||y||^2 whatever the blocking. That
#   is a statement about the bound, not about an implementation -- the
#   distortion is per coordinate, so how the coordinates are grouped
#   does not enter it. The sum is computed rather than the cancellation
#   asserted, and the parity harness anchors on it.
#
# Reference
#   Zandieh, A., Han, I., Daliri, M. and Karbasi, A. (2025) "TurboQuant:
#     Online Vector Quantization with Near-optimal Distortion Rate."
#     arXiv:2504.19874. Theorems 2 and 3, Lemma 4, and the b = 1..4
#     distortion table.

.TQIPB_ROUTES <- c("table", "panter_dite", "qjl", "lower_bound")
.TQIPB_TAILS <- c("chebyshev", "sub_gaussian")

# Solved 1-D k-means distortions for the Beta law, b = 1..4 (paper table).
.TQIPB_TABLE <- c(1.57, 0.56, 0.18, 0.047)

# sqrt(3) * pi^2, the Panter-Dite constant in this normalisation.
#' Sqrt(3) * pi^2, the Panter-Dite constant in this normalisation
#'
#' A step of the tqipb_native implementation. Called by \code{morie_tqipb_constant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A numeric value.
#' @export
.tqipb_pd <- function() sqrt(3) * pi * pi

# 4^b by repeated multiplication. Written out because R's `^` on an
# integer exponent is repeated squaring while Python's `**` calls libm
# pow(); the module does not rely on the two agreeing.
#' 4^b by repeated multiplication. Written out because R\'s `^` on an
#'
#' integer exponent is repeated squaring while Python\'s `**` calls libm
#' pow(); the module does not rely on the two agreeing.
#'
#' @param b A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{p}, as built in the body.
#' @export
.tqipb_pow4 <- function(b) {
  p <- 1
  b <- as.integer(b)
  if (b > 0L) for (i in seq_len(b)) p <- p * 4
  p
}

#' Per-unit-norm inner-product distortion constant
#'
#' The returned c satisfies E\[(<x_hat, y> - <x, y>)^2\] <= c ||x||^2
#' ||y||^2 for the chosen route.
#'
#' @param bits Bit-width per coordinate, at least 1.
#' @param d Block dimension.
#' @param route One of "table" (the paper's solved b = 1..4 constants,
#'   falling back to Panter-Dite above 4 bits, where the high-resolution
#'   form is accurate), "panter_dite" (Theorem 2), "qjl" (Lemma 4, one
#'   bit, independent of b) or "lower_bound" (Theorem 3).
#' @return A single numeric constant.
#' @keywords internal
morie_tqipb_constant <- function(bits, d, route = "table") {
  b <- as.integer(bits)
  d <- as.integer(d)
  if (b < 1L) stop("bits must be at least 1")
  if (d < 1L) stop("d must be at least 1")
  if (!(route %in% .TQIPB_ROUTES))
    stop("route must be one of ", paste(.TQIPB_ROUTES, collapse = ", "))
  if (route == "qjl") return(pi / (2 * d))
  if (route == "lower_bound") return(1 / (d * .tqipb_pow4(b)))
  if (route == "table" && b <= 4L) return(.TQIPB_TABLE[b] / d)
  .tqipb_pd() / (d * .tqipb_pow4(b))
}

# Split d coordinates into n_blocks as evenly as possible; the first
# d %% n_blocks blocks get one extra, which is what a packer does when
# the dimension does not divide.
#' Split d coordinates into n_blocks as evenly as possible; the first
#'
#' d %% n_blocks blocks get one extra, which is what a packer does when
#' the dimension does not divide.
#'
#' @param d Numeric; combined arithmetically in the body.
#' @param n_blocks Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{ifelse}.
#' @export
.tqipb_blocks <- function(d, n_blocks) {
  d <- as.integer(d)
  k <- as.integer(n_blocks)
  if (k < 1L || k > d) stop("n_blocks must be in 1..d")
  base <- d %/% k
  rem <- d - base * k
  ifelse(seq_len(k) <= rem, base + 1L, base)
}

#' Variance of the inner-product estimate over packed blocks
#'
#' Mass is split across blocks in proportion to their dimension, which is
#' the right split for a randomly rotated vector: after the rotation no
#' coordinate is special, so a block of d_j coordinates carries d_j / d
#' of the squared norm in expectation.
#'
#' @param bits Bit-width per coordinate.
#' @param d Total dimension.
#' @param norm_sq Squared norm of y.
#' @param x_norm_sq Squared norm of x; the paper states its bounds for x
#'   on the unit sphere and the distortion is homogeneous in this.
#' @param n_blocks Independently rotated and quantized blocks.
#' @param route See morie_tqipb_constant.
#' @return A single numeric variance bound.
#' @keywords internal
morie_tqipb_variance <- function(bits, d, norm_sq = 1, x_norm_sq = 1,
                                 n_blocks = 1, route = "table") {
  d <- as.integer(d)
  dims <- .tqipb_blocks(d, n_blocks)
  total <- 0
  for (j in seq_along(dims)) {
    share <- dims[j] / d
    total <- total + morie_tqipb_constant(bits, dims[j], route) *
      (x_norm_sq * share) * (norm_sq * share)
  }
  total
}

#' Tail probability for the inner-product error
#'
#' delta such that Pr\[|error| > eps ||x|| ||y||\] <= delta.
#'
#' @param var Variance bound, from morie_tqipb_variance.
#' @param eps Relative accuracy, positive.
#' @param norm_sq Squared norm of y.
#' @param x_norm_sq Squared norm of x.
#' @param tail "chebyshev" (nothing assumed beyond the variance) or
#'   "sub_gaussian" (assumes a sub-Gaussian error with the stated
#'   variance proxy -- an assumption the paper does not make).
#' @return A probability bound, clamped at 1 since a bound above 1 says
#'   nothing.
#' @keywords internal
morie_tqipb_tail <- function(var, eps, norm_sq = 1, x_norm_sq = 1,
                             tail = "chebyshev") {
  if (eps <= 0) stop("eps must be positive")
  if (!(tail %in% .TQIPB_TAILS))
    stop("tail must be one of ", paste(.TQIPB_TAILS, collapse = ", "))
  thresh_sq <- eps * eps * x_norm_sq * norm_sq
  if (var <= 0) return(0)
  p <- if (tail == "chebyshev") var / thresh_sq
       else 2 * exp(-thresh_sq / (2 * var))
  if (p < 1) p else 1
}

#' Smallest bit-width meeting a target (eps, delta)
#'
#' Searched rather than inverted: the "table" route is not a closed-form
#' function of b below 5 bits, and a search over at most 32 values costs
#' nothing.
#'
#' @param eps Relative accuracy.
#' @param delta Target failure probability.
#' @param d Total dimension.
#' @param norm_sq Squared norm of y.
#' @param x_norm_sq Squared norm of x.
#' @param n_blocks Independently quantized blocks.
#' @param route Variance route.
#' @param tail Tail inequality.
#' @param max_bits Largest bit-width searched.
#' @return The bit-width, or NULL if none within max_bits.
#' @keywords internal
morie_tqipb_bits_required <- function(eps, delta, d, norm_sq = 1,
                                      x_norm_sq = 1, n_blocks = 1,
                                      route = "table",
                                      tail = "chebyshev", max_bits = 32) {
  for (b in seq_len(as.integer(max_bits))) {
    v <- morie_tqipb_variance(b, d, norm_sq, x_norm_sq, n_blocks, route)
    if (morie_tqipb_tail(v, eps, norm_sq, x_norm_sq, tail) <= delta)
      return(b)
  }
  NULL
}

#' Inner-product distortion and tail bound for TurboQuant blocks
#'
#' @param bits Bit-width per coordinate, at least 1.
#' @param norm_sq Squared norm of the vector the quantized one is dotted
#'   against.
#' @param d Dimension. Required.
#' @param eps Relative accuracy in the tail statement.
#' @param delta If given, the result also carries the smallest bit-width
#'   attaining this on the chosen route and tail.
#' @param x_norm_sq Squared norm of x; the paper's statements take x on
#'   the unit sphere.
#' @param n_blocks Independently rotated blocks the coordinates are
#'   packed into.
#' @param route Variance route: "table", "panter_dite", "qjl" or
#'   "lower_bound".
#' @param tail Tail inequality: "chebyshev" or "sub_gaussian".
#' @return A list with the variance, rmse, relative error, tail bound,
#'   the constant, the block dimensions, and the route, tail and
#'   assumption each carries. bits_needed is present when delta is given.
#' @export
morie_tqipb <- function(bits, norm_sq = 1, d = NULL, eps = 0.1,
                        delta = NULL, x_norm_sq = 1, n_blocks = 1,
                        route = "table", tail = "chebyshev") {
  if (is.null(d)) stop("d (dimension) is required")
  b <- as.integer(bits)
  dims <- .tqipb_blocks(d, n_blocks)
  v <- morie_tqipb_variance(b, d, norm_sq, x_norm_sq, n_blocks, route)
  p <- morie_tqipb_tail(v, eps, norm_sq, x_norm_sq, tail)
  scale <- sqrt(x_norm_sq * norm_sq)
  out <- list(
    variance = v,
    rmse = sqrt(v),
    relative_error = if (scale > 0) sqrt(v) / scale else 0,
    delta_bound = p,
    eps = as.numeric(eps),
    constant = morie_tqipb_constant(b, dims[1], route),
    bits = b,
    d = as.integer(d),
    n_blocks = as.integer(n_blocks),
    block_dims = as.integer(dims),
    route = route,
    tail = tail,
    assumption = if (tail == "chebyshev") "variance only" else
      paste("error sub-Gaussian with the stated variance proxy --",
            "an assumption, not a paper result"),
    method = "TurboQuant inner-product distortion bound")
  if (!is.null(delta)) {
    out$target_delta <- as.numeric(delta)
    out$bits_needed <- morie_tqipb_bits_required(eps, delta, d, norm_sq,
                                                 x_norm_sq, n_blocks,
                                                 route, tail)
  }
  out
}

#' One-line summary of the tqipb module
#'
#' @return A character scalar.
#' @export
morie_tqipb_cheatsheet <- function()
  paste0("tqipb: TurboQuant inner-product distortion bounds. routes ",
         paste(.TQIPB_ROUTES, collapse = ", "), "; tails ",
         paste(.TQIPB_TAILS, collapse = ", "))
