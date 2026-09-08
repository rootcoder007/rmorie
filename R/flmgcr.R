# SPDX-License-Identifier: AGPL-3.0-or-later

#' Flamingo gated cross-attention
#'
#' Formula: h <- h + tanh(g) * CrossAttn(h, vision).
#'
#' DUPLICATE: the same method already ships as
#' \code{morie_geron_flamingo_cross_modal_attn}.  Per
#' ledger/wave2/DUPMAP.tsv this is a thin alias, not a second copy of the
#' arithmetic.  When \code{weights} is omitted the three projections
#' default to the identity, making the gated branch plain scaled
#' dot-product cross-attention on the raw features.
#'
#' @param x Language hidden states, T x d_model.
#' @param vision Visual features, Tv x d_model.
#' @param gate Gate parameter g; tanh(g) scales the branch, so g = 0 is
#'   exactly the identity on x.
#' @param weights Optional list (WQ, WK, WV); identity by default.
#' @param mask Optional attention mask, passed through.
#' @return The list returned by
#'   \code{morie_geron_flamingo_cross_modal_attn}, plus \code{estimate}
#'   (mean of the updated hidden states), \code{n} and \code{method}.
#' @references Alayrac et al (2022), NeurIPS 35:23716-23736 (Flamingo).
#' @export
Flmgcr <- function(x, vision, gate, weights = NULL, mask = NULL) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
  d <- ncol(X)
  if (d == 0L) stop("empty input: x has no columns")
  if (is.null(weights)) {
    eye <- diag(1, d)
    weights <- list(WQ = eye, WK = eye, WV = eye)
  }
  res <- morie_geron_flamingo_cross_modal_attn(X, vision, gate, weights,
                                               mask = mask)
  hn <- as.matrix(res$h_new)
  out <- res
  out$estimate <- sum(hn) / length(hn)
  out$n <- nrow(X)
  out$method <- "Flamingo gated cross-attention"
  class(out) <- c("morie_rich_result", "list")
  out
}
