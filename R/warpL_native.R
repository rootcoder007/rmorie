# morie.fn -- function file (rootcoder007/morie)
# WARP: optimise the top of the ranking, cheaply.
#
# With tens of thousands of possible labels, the metric that matters is
# precision at k -- what appears in the top few. Pairwise
# ranking losses optimise the *whole* ordering instead, so they spend
# most of their gradient on distinctions far below anything a user sees;
# and losses that do target the top are costly to train.
#
# The trick is to estimate a rank by sampling, not by computing it.
# For a positive label, draw negatives uniformly until one violates
# the margin. If it took N draws out of Y-1 candidates,
# the violating rank is estimated as floor((Y-1)/N) -- so a violation
# found on the first draw means a badly ranked positive, and one that
# took many draws means the positive is already near the top. Nothing
# is sorted; the estimate falls out of the number of attempts.
#
# That estimate is then WEIGHTED, and the weight is where "top of
# the list" enters. With L(r) = sum_{j<=r} alpha_j and
# alpha_1 >= alpha_2 >= ... >= 0, a decreasing alpha makes an error
# at rank 1 cost more than an error at rank 50. Choosing alpha_j = 1/j
# optimises the top; choosing alpha_j constant recovers the ordinary
# pairwise loss that weights every position alike -- and the anchor
# compares the two rather than asserting the difference.
#
# The sampling cost is self-limiting in the right direction. A
# well-ranked positive takes many draws to violate, so it is expensive
# exactly when it has least to teach; capping the draws at Y-1 bounds
# that, and sample_violation reports whether the cap was hit instead
# of silently returning a rank of zero.
#
# References
# ----------
# Weston, J., Bengio, S. & Usunier, N. (2010) "Large Scale Image
# Annotation: Learning to Rank with Joint Word-Image Embeddings",
# Machine Learning and Knowledge Discovery in Databases (ECML PKDD
# 2010), LNCS 6323, 21-35, doi:10.1007/978-3-642-15939-8_2. [PDF
# supplied by Vee.] The WARP (Weighted Approximate-Rank Pairwise) loss:
# that measures optimising for the top annotations, such as precision at
# k, are costly to train; the relation to the Ordered Weighted Pairwise
# Classification loss; the use of stochastic gradient descent with a
# sampling trick to APPROXIMATE ranks, giving an efficient online
# strategy superior to standard SGD on the same loss and able to train
# on datasets that do not fit in memory; and its applicability to
# arbitrary differentiable models, unlike the OWPC loss which relies on
# SVMstruct.
#
# Usunier, N., Buffoni, D. & Gallinari, P. (2009) "Ranking with ordered
# weighted pairwise classification", ICML 2009, 1057-1064,
# doi:10.1145/1553374.1553509. The ordered weighted pairwise loss and
# the alpha weights.
#
# Weston, J., Bengio, S. & Usunier, N. (2011) "WSABIE: Scaling Up to
# Large Vocabulary Image Annotation", IJCAI 2011, 2764-2770. The
# later, more widely cited presentation of the same loss. NOTE: the
# ECML 2010 paper above is the one held locally and is the text this
# module follows.

#' .warpL_vec
#'
#' A step of the warpL_native implementation. Called by \code{morie_warpL_warp_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.warpL_vec <- function(x) {
  as.numeric(x)
}

.warpL_eps <- 1e-12

#' .warpL_alpha_weights
#'
#' A step of the warpL_native implementation. Called by \code{morie_warpL_alpha_weights}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n See Usage.
#' @param scheme Defaults to \code{"reciprocal"}.
#' @return The value of \code{a}, as built in the body.
#' @export
.warpL_alpha_weights <- function(n, scheme = "reciprocal") {
  N <- as.integer(n)
  if (N < 1) stop("warpL: n must be at least 1")
  if (identical(scheme, "reciprocal")) {
    a <- 1.0 / seq_len(N)
  } else if (identical(scheme, "uniform")) {
    a <- rep(1.0, N)
  } else if (identical(scheme, "top1")) {
    a <- c(1.0, rep(0.0, N - 1))
  } else {
    stop(sprintf("warpL: scheme must be reciprocal, uniform or top1, got %s",
                 deparse(scheme)))
  }
  if (N > 1) {
    diffs <- a[-length(a)] - a[-1]
    if (any(diffs < -.warpL_eps)) {
      stop("warpL: the alpha weights must be non-increasing")
    }
  }
  a
}

#' .warpL_rank_weight
#'
#' A step of the warpL_native implementation. Called by \code{.warpL_warp_loss}, \code{morie_warpL_rank_weight}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rank See Usage.
#' @param alphas A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.warpL_rank_weight <- function(rank, alphas) {
  r <- as.integer(rank)
  if (r < 0) stop("warpL: the rank cannot be negative")
  r_capped <- min(r, length(alphas))
  sum(alphas[seq_len(r_capped)])
}

#' .warpL_estimate_rank
#'
#' A step of the warpL_native implementation. Called by \code{.warpL_sample_violation}, \code{morie_warpL_estimate_rank}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_draws See Usage.
#' @param n_labels See Usage.
#' @return The value of \code{as.integer}.
#' @export
.warpL_estimate_rank <- function(n_draws, n_labels) {
  N <- as.integer(n_draws)
  Y <- as.integer(n_labels)
  if (N < 1 || Y < 2) stop("warpL: need at least one draw and two labels")
  as.integer((Y - 1) %/% N)
}

#' .warpL_sample_violation
#'
#' A step of the warpL_native implementation. Called by \code{morie_warpL_sample_violation}, \code{morie_warpL_warp_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param score_positive See Usage.
#' @param negative_scorer See Usage.
#' @param n_labels See Usage.
#' @param rng Passed to \code{.ghc_unif}.
#' @param margin Defaults to \code{1}.
#' @param max_draws Defaults to \code{NULL}.
#' @return A list with \code{violated}, \code{draws}, \code{negative}, \code{estimated_rank}, \code{capped}, \code{note}.
#' @export
.warpL_sample_violation <- function(score_positive, negative_scorer, n_labels,
                                     rng, margin = 1.0, max_draws = NULL) {
  Y <- as.integer(n_labels)
  cap <- if (is.null(max_draws)) Y - 1 else as.integer(max_draws)
  if (cap < 1) stop("warpL: at least one draw is required")

  sp <- as.numeric(score_positive)
  m <- as.numeric(margin)

  for (t in seq_len(cap)) {
    u <- .ghc_unif(rng, 1)
    j <- (as.integer(u * (Y - 1))) %% (Y - 1)
    s <- as.numeric(negative_scorer(j))
    if (s > sp - m) {
      return(list(
        violated = TRUE,
        draws = as.integer(t),
        negative = as.integer(j),
        negative_score = s,
        estimated_rank = .warpL_estimate_rank(t, Y),
        capped = FALSE
      ))
    }
  }
  list(
    violated = FALSE,
    draws = as.integer(cap),
    negative = NULL,
    estimated_rank = 0L,
    capped = TRUE,
    note = "no violator found within the cap: the positive is already well ranked, which is exactly when sampling is most expensive"
  )
}

#' .warpL_warp_loss
#'
#' A step of the warpL_native implementation. Called by \code{morie_warpL_warp_loss}, \code{morie_warpL_warp_step}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param score_positive See Usage.
#' @param score_negative See Usage.
#' @param estimated_rank See Usage.
#' @param alphas Passed to \code{.warpL_rank_weight}.
#' @param margin Defaults to \code{1}.
#' @return A list with \code{loss}, \code{hinge}, \code{rank_weight}, \code{estimated_rank}.
#' @export
.warpL_warp_loss <- function(score_positive, score_negative, estimated_rank,
                              alphas, margin = 1.0) {
  hinge <- max(0, as.numeric(margin) - as.numeric(score_positive) + as.numeric(score_negative))
  w <- .warpL_rank_weight(as.integer(estimated_rank), alphas)
  list(
    loss = w * hinge,
    hinge = hinge,
    rank_weight = w,
    estimated_rank = as.integer(estimated_rank)
  )
}

#' morie_warpL_alpha_weights
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Passed to \code{.warpL_alpha_weights}.
#' @param scheme Passed to \code{.warpL_alpha_weights}. Defaults to \code{"reciprocal"}.
#' @return The value of \code{.warpL_alpha_weights}.
#' @export
morie_warpL_alpha_weights <- function(n, scheme = "reciprocal") {
  .warpL_alpha_weights(n, scheme)
}

#' morie_warpL_rank_weight
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rank Passed to \code{.warpL_rank_weight}.
#' @param alphas Passed to \code{.warpL_rank_weight}.
#' @return The value of \code{.warpL_rank_weight}.
#' @export
morie_warpL_rank_weight <- function(rank, alphas) {
  .warpL_rank_weight(rank, alphas)
}

#' morie_warpL_estimate_rank
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_draws Passed to \code{.warpL_estimate_rank}.
#' @param n_labels Passed to \code{.warpL_estimate_rank}.
#' @return The value of \code{.warpL_estimate_rank}.
#' @export
morie_warpL_estimate_rank <- function(n_draws, n_labels) {
  .warpL_estimate_rank(n_draws, n_labels)
}

#' morie_warpL_sample_violation
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param score_positive Passed to \code{.warpL_sample_violation}.
#' @param negative_scorer Passed to \code{.warpL_sample_violation}.
#' @param n_labels Passed to \code{.warpL_sample_violation}.
#' @param rng Passed to \code{.warpL_sample_violation}.
#' @param margin Passed to \code{.warpL_sample_violation}. Defaults to \code{1}.
#' @param max_draws Passed to \code{.warpL_sample_violation}.
#' @return The value of \code{.warpL_sample_violation}.
#' @export
morie_warpL_sample_violation <- function(score_positive, negative_scorer, n_labels,
                                          rng, margin = 1.0, max_draws = NULL) {
  .warpL_sample_violation(score_positive, negative_scorer, n_labels, rng, margin, max_draws)
}

#' morie_warpL_warp_loss
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param score_positive Passed to \code{.warpL_warp_loss}.
#' @param score_negative Passed to \code{.warpL_warp_loss}.
#' @param estimated_rank Passed to \code{.warpL_warp_loss}.
#' @param alphas Passed to \code{.warpL_warp_loss}.
#' @param margin Passed to \code{.warpL_warp_loss}. Defaults to \code{1}.
#' @return The value of \code{.warpL_warp_loss}.
#' @export
morie_warpL_warp_loss <- function(score_positive, score_negative, estimated_rank,
                                   alphas, margin = 1.0) {
  .warpL_warp_loss(score_positive, score_negative, estimated_rank, alphas, margin)
}

#' morie_warpL_warp_step
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param positive Passed to \code{.warpL_vec}.
#' @param negatives A vector; its length is taken and its elements indexed.
#' @param embed_user Passed to \code{.warpL_vec}.
#' @param rng Passed to \code{.warpL_sample_violation}.
#' @param alphas Passed to \code{.warpL_warp_loss}.
#' @param lr Defaults to \code{0.05}.
#' @param margin Passed to \code{.warpL_sample_violation}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{updated}, \code{loss}, \code{user}, \code{draws}, \code{estimated_rank}, \code{rank_weight}, \code{negative}, \code{method}, \code{note}.
#' @export
morie_warpL_warp_step <- function(positive, negatives, embed_user, rng, alphas,
                                   lr = 0.05, margin = 1.0) {
  u <- .warpL_vec(embed_user)
  P <- .warpL_vec(positive)
  Y <- length(negatives) + 1

  score <- function(v) {
    w <- .warpL_vec(v)
    sum(u * w)
  }

  sp <- score(P)
  v <- .warpL_sample_violation(sp, function(j) score(negatives[[j + 1]]),
                                Y, rng, margin)

  if (!isTRUE(v$violated)) {
    return(list(
      updated = FALSE,
      draws = v$draws,
      loss = 0.0,
      user = u,
      note = "nothing violated the margin, so there is nothing to learn from this positive"
    ))
  }

  neg <- .warpL_vec(negatives[[v$negative + 1]])
  L <- .warpL_warp_loss(sp, score(negatives[[v$negative + 1]]),
                        v$estimated_rank, alphas, margin)
  g <- L$rank_weight * as.numeric(lr)
  new_u <- u + g * (P - neg)

  list(
    estimate = L$loss,
    updated = TRUE,
    loss = L$loss,
    user = new_u,
    draws = v$draws,
    estimated_rank = v$estimated_rank,
    rank_weight = L$rank_weight,
    negative = v$negative,
    method = "WARP sampled rank approximation; Weston, Bengio & Usunier (2010)",
    note = "the step size scales with L(rank), so an error at the top of the list moves the model further"
  )
}

#' morie_warpL_cheatsheet
#'
#' A step of the warpL_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_warpL_cheatsheet <- function() {
  "warpL: with tens of thousands of labels what matters is precision at k, but pairwise losses optimise the WHOLE ordering and top-targeting losses are costly to train. Estimate the rank by SAMPLING: draw negatives until one violates the margin, and if it took N draws the rank is about (Y-1)/N -- a violation on the first draw means a badly ranked positive, many draws means it is already near the top. Nothing is sorted. Then WEIGHT by L(r) = sum_{j<=r} alpha_j with alpha non-increasing: alpha_j = 1/j optimises the top, constant alpha recovers the plain pairwise loss. Cap the draws and SAY when the cap was hit."
}

morie_warpL_warp_rank_loss <- morie_warpL_warp_step
morie_warpL_warp <- morie_warpL_warp_step
