# caltbR_native.R
#
# Calibrated recommendations: keep the user's proportions (Steck, 2018).
#
#   Steck, H. (2018) "Calibrated Recommendations", RecSys '18, 154-162,
#   doi:10.1145/3240323.3240372. Sec. 2.1 (class-imbalance argument,
#   accuracy rewards the majority genre), Sec. 2.2 (varying-movie-
#   probability example, 2.1 measured on MovieLens 20M), Sec. 3
#   (eqs. 2-5: two distributions, rank weights, KL with the
#   alpha-smoothed q-tilde for alpha = 0.01, three desired properties,
#   and the Hellinger alternative), Sec. 4 (eq. 6: MMR re-ranking,
#   greedy optimisation, submodularity and the (1 - 1/e) guarantee at
#   every prefix), Sec. 5.1 / Table 1 (calibration is not diversity),
#   eq. 7 (the diversity prior).
#
#   Carbonell & Goldstein (1998), SIGIR '98, 335-336, doi:10.1145/
#   290941.291025 -- maximum marginal relevance.
#
#   Nemhauser, Wolsey & Fisher (1978), Mathematical Programming 14,
#   265-294, doi:10.1007/BF01588971 -- the (1 - 1/e) guarantee for
#   greedy maximisation of a submodular function.
#
# Native R port of morie.fn.caltbR: the same weighted average of
# p(g|i) (eqs. 2-3), the same KL against (1 - alpha)*q + alpha*p
# (eqs. 4-5), the same Hellinger distance, the same eq. 7 diversity
# prior, and the same greedy maximum-marginal-relevance re-ranker
# (eq. 6). The submodular surrogate gives every prefix the (1 - 1/e)
# optimality guarantee. No random numbers are needed, so the shared
# RNG is not touched.


# ---------------------------------------------------------------------------
# Constants and small helpers
# ---------------------------------------------------------------------------

.CALTBR_EPS  <- 1e-12
.CALTBR_METRICS <- c("kl", "hellinger")


# Normalise a vector to a probability distribution, raising on non-positive
# mass -- mirrors _norm in the Python arm.
.caltbR_norm <- function(v) {
  vv <- as.numeric(v)
  s  <- sum(vv)
  if (!is.finite(s) || s <= .CALTBR_EPS)
    stop("caltbR: a genre distribution has no mass")
  vv / s
}


# Coerce p_g_given_i to a numeric matrix with one row per item and G
# columns, the G determined from the first non-empty row.
.caltbR_to_pgi <- function(p_g_given_i) {
  if (is.matrix(p_g_given_i)) {
    storage.mode(p_g_given_i) <- "double"
    return(p_g_given_i)
  }
  if (is.data.frame(p_g_given_i))
    return(as.matrix(p_g_given_i, mode = "double"))
  if (is.list(p_g_given_i)) {
    rows <- lapply(p_g_given_i, as.numeric)
    G    <- length(rows[[1L]])
    M    <- matrix(0, nrow = length(rows), ncol = G)
    for (i in seq_along(rows)) M[i, ] <- rows[[i]]
    return(M)
  }
  stop("caltbR: p_g_given_i must be a matrix, data.frame or list of rows")
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Weighted average of \eqn{p(g \mid i)} over a set of items.
#'
#' Implements eqs. (2)-(3): a per-genre average of the genre
#' memberships, weighted either by user-supplied weights (recency of
#' the play, say) or by rank (when called from the reranker with a
#' rank_weights argument).
#'
#' @param items Integer vector of item indices (0-based in Python,
#'   1-based in R -- shifted internally).
#' @param p_g_given_i Numeric matrix or list of rows with one row per
#'   item and one column per genre, holding \eqn{p(g \mid i)}.
#' @param weights Optional numeric vector of per-item weights; if
#'   \code{NULL} each item contributes 1.
#' @return Numeric vector \eqn{q(g)} of length G, the weighted
#'   average.
#' @references Steck (2018), RecSys '18, eqs. (2)-(3).
#' @export
genre_distribution <- function(items, p_g_given_i, weights = NULL) {
  it <- as.integer(items)
  if (length(it) == 0L)
    stop("caltbR: no items given")
  M  <- .caltbR_to_pgi(p_g_given_i)
  nI <- nrow(M)
  if (any(it < 1L | it > nI))
    stop(sprintf("caltbR: item index out of range (1..%d)", nI))
  G  <- ncol(M)
  if (is.null(weights)) {
    w  <- rep(1.0, length(it))
  } else {
    w  <- as.numeric(weights)
  }
  if (length(w) != length(it))
    stop(sprintf("caltbR: %d weights for %d items",
                 length(w), length(it)))
  tot <- sum(w)
  if (!is.finite(tot) || tot <= .CALTBR_EPS)
    stop("caltbR: the weights sum to zero")
  # Python uses 0-based indices, so shift to 1-based here.
  idx <- it  # already 1-based in R
  out <- numeric(G)
  for (g in seq_len(G)) {
    s <- 0.0
    for (n in seq_along(idx)) s <- s + w[n] * M[idx[n], g]
    out[g] <- s / tot
  }
  out
}


#' \eqn{KL(p \,\|\, (1-\alpha)q + \alpha p)}.
#'
#' Implements eqs. (4)-(5): the KL divergence from the target
#' distribution \eqn{p} to the alpha-smoothed recommended
#' distribution \eqn{\tilde q = (1-\alpha) q + \alpha p}. The small
#' alpha (0.01 in the paper) is needed because \eqn{q} can be zero
#' where \eqn{p} is not.
#'
#' @param p Target distribution \eqn{p(g \mid u)}.
#' @param q Recommended distribution \eqn{q(g \mid u)}.
#' @param alpha Smoothing weight in \eqn{(0, 1)}.
#' @return Numeric scalar, the KL value.
#' @references Steck (2018), RecSys '18, eqs. (4)-(5).
#' @export
calibration_kl <- function(p, q, alpha = 0.01) {
  pp <- .caltbR_norm(p)
  qq <- .caltbR_norm(q)
  if (length(pp) != length(qq))
    stop(sprintf("caltbR: %d genres in p but %d in q",
                 length(pp), length(qq)))
  a <- as.numeric(alpha)
  if (!is.finite(a) || a <= 0.0 || a >= 1.0)
    stop(sprintf("caltbR: alpha must lie in (0,1), got %r", alpha))
  tot <- 0.0
  for (g in seq_along(pp)) {
    if (pp[g] <= .CALTBR_EPS) next
    qt <- (1.0 - a) * qq[g] + a * pp[g]
    tot <- tot + pp[g] * log(pp[g] / max(qt, .CALTBR_EPS))
  }
  tot
}


#' Hellinger distance \eqn{\|\sqrt p - \sqrt q\|_2 / \sqrt 2}.
#'
#' Well defined at zeros and less brutal than KL where \eqn{p} is
#' small -- the paper's named alternative.
#'
#' @param p Target distribution \eqn{p(g \mid u)}.
#' @param q Recommended distribution \eqn{q(g \mid u)}.
#' @return Numeric scalar, the Hellinger distance.
#' @references Steck (2018), RecSys '18, sec. 3.
#' @export
calibration_hellinger <- function(p, q) {
  pp <- .caltbR_norm(p)
  qq <- .caltbR_norm(q)
  s  <- 0.0
  for (g in seq_along(pp)) {
    d  <- sqrt(pp[g]) - sqrt(qq[g])
    s  <- s + d * d
  }
  sqrt(s) / sqrt(2.0)
}


#' Diversity prior \eqn{\bar p = \beta p_0 + (1-\beta) p(g \mid u)}.
#'
#' Calibration alone never introduces an unplayed genre; mixing in a
#' diversity prior with weight \code{beta} is the explicit escape
#' hatch from the filter bubble.
#'
#' @param p_u User's played distribution \eqn{p(g \mid u)}.
#' @param p0 Population prior \eqn{p_0(g)}.
#' @param beta Mixing weight in \eqn{[0, 1]}.
#' @return Numeric vector \eqn{\bar p(g)} of length G.
#' @references Steck (2018), RecSys '18, eq. (7).
#' @export
diversity_prior <- function(p_u, p0, beta) {
  a <- as.numeric(p_u)
  b <- as.numeric(p0)
  t <- as.numeric(beta)
  if (!is.finite(t) || t < 0.0 || t > 1.0)
    stop(sprintf("caltbR: beta must lie in [0,1], got %r", beta))
  if (length(a) != length(b))
    stop(sprintf("caltbR: prior has %d genres, target %d",
                 length(b), length(a)))
  t * b + (1.0 - t) * a
}


#' Greedy maximum-marginal-relevance re-ranking for calibration.
#'
#' Solves eq. (6) by greedy maximisation of
#' \eqn{(1-\lambda)\,s(I) - \lambda\, C(p, q(I))}. The surrogate is
#' submodular, so every prefix of the returned list is
#' \eqn{(1 - 1/e)} optimal.
#'
#' @param scores Numeric vector of per-item relevance scores.
#' @param p_g_given_i Numeric matrix or list of rows with \eqn{p(g \mid i)}.
#' @param p_target Target distribution \eqn{p(g \mid u)}.
#' @param N Number of items to select.
#' @param lam Trade-off between accuracy (\eqn{1-\lambda}) and
#'   calibration (\eqn{\lambda}).
#' @param metric Either \code{"kl"} (default) or \code{"hellinger"}.
#' @param alpha Smoothing weight for the KL target (ignored for the
#'   Hellinger metric).
#' @param rank_weights Optional numeric vector of per-rank weights
#'   for the recommended list; if \code{NULL} each chosen slot has
#'   weight 1.
#' @return A named list mirroring the Python RichResult payload:
#'   \code{estimate}, \code{ranking} (identical 0-based indices in
#'   Python, 1-based indices in R), \code{objective_path},
#'   \code{q}, \code{p_target}, \code{calibration},
#'   \code{calibration_uncalibrated}, \code{score},
#'   \code{score_uncalibrated}, \code{lambda}, \code{metric},
#'   \code{N}, \code{guarantee}, \code{method}.
#' @references Steck (2018), RecSys '18, eq. (6).
#' @export
calibrated_rerank <- function(scores, p_g_given_i, p_target, N = 10,
                              lam = 0.5, metric = "kl",
                              alpha = 0.01, rank_weights = NULL) {
  metric <- as.character(metric)
  if (!(metric %in% .CALTBR_METRICS))
    stop(sprintf("caltbR: metric must be one of %s, got '%s'",
                 paste(.CALTBR_METRICS, collapse = ", "), metric))
  s <- as.numeric(scores)
  n <- length(s)
  if (n == 0L)
    stop("caltbR: no candidate items")
  Nn <- min(as.integer(N), n)
  if (Nn < 1L)
    stop("caltbR: N must be at least 1")
  lm <- as.numeric(lam)
  if (!is.finite(lm) || lm < 0.0 || lm > 1.0)
    stop(sprintf("caltbR: lambda must lie in [0,1], got %r", lam))
  pt <- .caltbR_norm(p_target)

  M <- .caltbR_to_pgi(p_g_given_i)
  if (nrow(M) != n)
    stop(sprintf("caltbR: %d genre rows for %d scores",
                 nrow(M), n))

  # Rank weights -- one per chosen slot.
  rw <- if (is.null(rank_weights)) NULL else as.numeric(rank_weights)

  # Calibration helper for a given selection.
  cal <- function(sel) {
    w <- if (is.null(rw)) NULL else rw[seq_along(sel)]
    q <- genre_distribution(sel, M, w)
    if (metric == "kl")
      calibration_kl(pt, q, alpha)
    else
      calibration_hellinger(pt, q)
  }

  chosen <- integer(0)
  obj    <- numeric(Nn)
  for (step in seq_len(Nn)) {
    best <- -Inf
    bi   <- NA_integer_
    for (i in seq_len(n)) {
      if (i %in% chosen) next
      cand <- c(chosen, i)
      val  <- (1.0 - lm) * sum(s[cand]) - lm * cal(cand)
      if (val > best) { best <- val; bi <- i }
    }
    chosen <- c(chosen, bi)
    obj[step] <- best
  }

  q_final <- genre_distribution(
    chosen, M,
    if (is.null(rw)) NULL else rw[seq_along(chosen)])
  ord   <- order(-s, seq_len(n))
  top   <- ord[seq_len(Nn)]

  # Python returns 0-based indices; in R the values are already
  # 1-based. We hand back 1-based indices -- consistent with R
  # conventions -- and note the shift in the description.
  list(
    estimate                  = chosen,
    ranking                   = chosen,
    objective_path            = obj,
    q                         = q_final,
    p_target                  = pt,
    calibration               = cal(chosen),
    calibration_uncalibrated  = cal(top),
    score                     = sum(s[chosen]),
    score_uncalibrated        = sum(s[top]),
    lambda                    = lm,
    metric                    = metric,
    N                         = Nn,
    guarantee = paste("(1 - 1/e) optimal at EVERY prefix, by",
                      "submodularity"),
    method = paste("greedy maximum marginal relevance;",
                   "Steck (2018) eq. (6)")
  )
}


#' Convenience alias matching the Python \code{calibratedrecommendations}
#' export.
#' @export
calibratedrecommendations <- calibrated_rerank


#' Convenience alias matching the Python \code{calibrated_rec} export.
#' @export
calibrated_rec <- calibrated_rerank


#' Convenience alias matching the Python \code{calibratedrec} export.
#' @export
calibratedrec <- calibrated_rerank


.caltbR_cheatsheet <- function() {
  paste0(
    "caltbR: ranking by accuracy CROWDS OUT the user's minority ",
    "interests -- with 70/30 genre proportions the top-10 by p(i|u) ",
    "is all romance, because the imbalanced majority label is the ",
    "accuracy-optimal prediction. Calibration therefore costs ",
    "accuracy by construction. C_KL = KL(p || (1-alpha)q + alpha*p), ",
    "alpha = 0.01, chosen because it is zero only at equality, ",
    "punishes errors where p is SMALL, and prefers the less extreme ",
    "deviation. Applied by greedy MMR re-ranking, submodular, so ",
    "every PREFIX is (1-1/e) optimal. Not the same as diversity: ",
    "diversity would return 50/50."
  )
}

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution

#' @rdname genre_distribution
#' @export
morie_caltbR <- genre_distribution
