# SPDX-License-Identifier: AGPL-3.0-or-later

#' t-SNE for non-linear dimension reduction (R parity)
#'
#' Native exact t-SNE (van der Maaten & Hinton 2008): perplexity-
#' calibrated Gaussian affinities via per-point binary search, PCA
#' preprocessing to 50 components, early exaggeration, and momentum
#' gradient descent on the KL divergence. O(n^2) -- appropriate for
#' the module-scale inputs this helper serves. Replaces the
#' \pkg{Rtsne} delegation; embedding quality is cross-validated
#' against Rtsne in tests (KL divergence + neighbourhood recall).
#'
#' @param x Numeric matrix.
#' @param n_components Embedding dimension.
#' @param perplexity t-SNE perplexity.
#' @param learning_rate Gradient-descent learning rate; `"auto"`
#'   selects `max(n/12, 50)`.
#' @param n_iter Max iterations.
#' @param seed RNG seed.
#' @param deterministic_seed Integer or NULL.  If supplied, the RNG state
#'   is derived from the SHA-keyed [morie_det_rng()] so Py<->R streams
#'   agree on the canonical fixture.  When `NULL` (default), behaviour
#'   is unchanged: `seed` drives `set.seed()` directly.
#' @return Named list: estimate (shape), embedding, kl_divergence,
#'   perplexity, n_components, n, method.
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "rmorie")
#' @export
morie_tsne_reduction <- function(x, n_components = 2L, perplexity = 30,
                           learning_rate = "auto", n_iter = 1000L,
                           seed = 0L,
                           deterministic_seed = NULL) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  n <- nrow(x)
  max_perplexity <- max(1, floor((n - 1) / 3))
  if (perplexity > max_perplexity) perplexity <- max_perplexity
  if (!is.null(deterministic_seed)) {
    morie_det_rng("tsnrd", deterministic_seed)
  } else {
    set.seed(seed)
  }
  fit <- .morie_tsne(x, dims = as.integer(n_components),
                     perplexity = perplexity,
                     n_iter = as.integer(n_iter),
                     eta = learning_rate)
  list(
    estimate      = dim(fit$Y),
    embedding     = fit$Y,
    kl_divergence = fit$kl,
    perplexity    = as.numeric(perplexity),
    n_components  = as.integer(n_components),
    n             = n,
    method        = "t-SNE (van der Maaten 2008)"
  )
}

# Exact t-SNE core. Returns list(Y, kl).
#' Exact t-SNE core. Returns list(Y, kl)
#'
#' Part of the tsnrd implementation; see the file header for the source
#' it follows.
#'
#' @param x See Usage.
#' @param dims Defaults to \code{2L}.
#' @param perplexity Defaults to \code{30}.
#' @param n_iter Defaults to \code{1000L}.
#' @param eta Defaults to \code{"auto"}.
#' @return A list with \code{Y}, \code{kl}.
#' @export
.morie_tsne <- function(x, dims = 2L, perplexity = 30, n_iter = 1000L,
                        eta = "auto") {
  n <- nrow(x)
  # PCA preprocessing to at most 50 components (Rtsne pca=TRUE parity).
  if (ncol(x) > 50L) {
    x <- stats::prcomp(x, rank. = 50L, center = TRUE, scale. = FALSE)$x
  }
  if (identical(eta, "auto")) eta <- max(n / 12, 50)
  eta <- as.numeric(eta)

  # Pairwise squared distances.
  sq <- rowSums(x^2)
  D2 <- outer(sq, sq, "+") - 2 * tcrossprod(x)
  D2[D2 < 0] <- 0
  diag(D2) <- Inf

  # Per-point binary search for the Gaussian bandwidth matching
  # log(perplexity) entropy.
  target <- log(perplexity)
  P <- matrix(0, n, n)
  for (i in seq_len(n)) {
    lo <- -Inf; hi <- Inf; beta <- 1
    di <- D2[i, -i]
    for (it in seq_len(50L)) {
      w <- exp(-di * beta)
      sw <- sum(w)
      if (sw <= 0) { beta <- beta / 2; next }
      H <- log(sw) + beta * sum(di * w) / sw
      if (abs(H - target) < 1e-5) break
      if (H > target) {
        lo <- beta
        beta <- if (is.finite(hi)) (beta + hi) / 2 else beta * 2
      } else {
        hi <- beta
        beta <- if (is.finite(lo)) (beta + lo) / 2 else beta / 2
      }
    }
    w_full <- numeric(n)
    w_full[-i] <- w / sw
    P[i, ] <- w_full
  }
  P <- (P + t(P)) / (2 * n)
  P[P < 1e-12] <- 1e-12

  # Gradient descent (early exaggeration + momentum/gains) runs in the
  # C++ kernel; see src/morie_smallstats.cpp.
  Y <- matrix(stats::rnorm(n * dims, sd = 1e-4), n, dims)
  fit <- .morie_tsne_descent_cpp(P, Y, as.integer(n_iter), eta)
  list(Y = fit$Y, kl = fit$kl)
}
