# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared primitives for the Vision Transformer modules.
#
# Dosovitskiy, A., Beyer, L., Kolesnikov, A., Weissenborn, D., Zhai, X.,
# Unterthiner, T., Dehghani, M., Minderer, M., Heigold, G., Gelly, S.,
# Uszkoreit, J. and Houlsby, N. (2021), "An Image is Worth 16x16 Words:
# Transformers for Image Recognition at Scale", ICLR 2021; arXiv:2010.11929v2.
#
# Everything here is infrastructure, not method: a deterministic stand-in for
# the learned parameters, layer normalisation, and the channel convention used
# to read an image.  The method itself lives in vitptm, vitcls, vitatt, vitmlp,
# vitfwd and vitfsv.
#
# The paper describes trained networks.  A reproducible reference
# implementation cannot ship trained weights, so every parameter matrix here is
# filled from the shared deterministic normal stream (.s03normdraws on the
# base-2 van der Corput sequence, mirrored in Python by _s03core.normdraws).
# Both language arms therefore hold the SAME numbers, not merely numbers from
# the same distribution, which is what makes a 1e-9 parity comparison
# meaningful.  A single stream is consumed in a documented order via the skip
# offset, so distinct weight matrices are distinct.
#
# Layer normalisation is Ba, Kiros and Hinton (2016), "Layer Normalization",
# arXiv:1607.06450, which the paper cites as "LN" in Eqs. (2)-(4), p. 4.  The
# paper does not state an epsilon; 1e-6 is used here and is stated as this
# implementation's own choice, not as a quotation.  Gain and bias are the
# identity (gamma = 1, beta = 0).

.vitlneps <- 1e-6

# A deterministic nr-by-nc parameter matrix, row-major off the stream.
#' A deterministic nr-by-nc parameter matrix, row-major off the stream
#'
#' A step of the helpers_vit implementation. Called by \code{Vaean}, \code{Vaeber}, \code{Vaecf} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nr A count; the body uses it as \code{matrix(...)}.
#' @param nc A count; the body uses it as \code{matrix(...)}.
#' @param skip Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param scale Defaults to \code{1}.
#' @return A matrix, from \code{matrix}.
#' @export
.vitdraw <- function(nr, nc, skip = 0, scale = 1) {
  nr <- as.integer(nr)
  nc <- as.integer(nc)
  skip <- as.integer(skip)
  if (nr < 1L || nc < 1L) stop("draw: matrix dimensions must be positive")
  if (skip < 0L) stop("draw: skip must be non-negative")
  d <- .s03normdraws(skip + nr * nc, 2L)
  matrix(as.numeric(scale) * d[skip + seq_len(nr * nc)],
         nrow = nr, ncol = nc, byrow = TRUE)
}

# LN(v) = (v - mean v) / sqrt(pop.var v + eps), gamma = 1, beta = 0.
#' LN(v) = (v - mean v) / sqrt(pop.var v + eps), gamma = 1, beta = 0
#'
#' A step of the helpers_vit implementation. Called by \code{.vitlnrows}, \code{Vitfwd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{.vitlneps}.
#' @return A numeric value.
#' @export
.vitln <- function(v, eps = .vitlneps) {
  n <- length(v)
  if (n == 0L) stop("layernorm: empty vector")
  m <- sum(v) / n
  sd <- sqrt(sum((v - m)^2) / n + eps)
  (v - m) / sd
}

#' .vitlnrows
#'
#' A step of the helpers_vit implementation. Called by \code{Vitfwd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param eps Passed to \code{.vitln}. Defaults to \code{.vitlneps}.
#' @return The value of \code{out}, as built in the body.
#' @export
.vitlnrows <- function(A, eps = .vitlneps) {
  out <- A
  for (i in seq_len(nrow(A))) out[i, ] <- .vitln(A[i, ], eps)
  out
}

# Read an image as a list of C matrices, each H-by-W.  A plain H-by-W matrix is
# the single-channel case, C = 1.
#' Read an image as a list of C matrices, each H-by-W.  A plain H-by-W
#' matrix is
#'
#' the single-channel case, C = 1.
#'
#' @param image Passed to \code{.s03mat}.
#' @return The value of \code{out}, as built in the body.
#' @export
.vitchan <- function(image) {
  out <- if (is.list(image)) lapply(image, .s03mat) else list(.s03mat(image))
  h <- nrow(out[[1L]])
  w <- ncol(out[[1L]])
  for (m in out) {
    if (nrow(m) != h || ncol(m) != w) {
      stop("channels: all channels must have the same H and W")
    }
  }
  out
}

# Index of the first maximum; R's which.max already has this tie rule.
#' Index of the first maximum; R\'s which.max already has this tie rule
#'
#' A step of the helpers_vit implementation. Called by \code{Vitfsv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return The value of \code{which.max}.
#' @export
.vitargmax <- function(v) {
  if (length(v) == 0L) stop("argmax_first: empty vector")
  which.max(v)
}
