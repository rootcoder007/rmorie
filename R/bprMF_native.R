# morie.fn -- function file (rootcoder007/morie)
# r"""BPR: optimising the ranking, not the score.
#
# Implicit feedback is positive-only. The usual move is to label the
# observed pairs (u,i) in S as 1, everything else as 0, and fit. The
# paper's objection to that is sharp: the elements the model is
# *supposed to rank in the future* -- all of (U x I) \\ S -- are handed
# to the learner as negatives. A model with enough capacity to fit that
# training data exactly cannot rank at all, because it predicts 0
# everywhere. Such methods only produce rankings because regularisation
# stops them fitting.
#
# **So use pairs.** From S, assume a user prefers what they have seen
# over what they have not:
#
#     D_S := {(u,i,j) | i in I_u^+ and j in I \\ I_u^+}.
#
# Two items both seen, or both unseen, yield nothing -- and the unseen
# pairs are exactly the ones to be ranked at prediction time, so they
# are correctly left out of training rather than labelled negative.
#
# **The criterion.** With p(i >_u j | Theta) = sigma(x_hat_uij) and a
# Gaussian prior Theta ~ N(0, lambda_Theta I),
#
#     BPR-Opt = sum_{(u,i,j) in D_S} ln sigma(x_hat_uij)
#             - lambda_Theta ||Theta||^2.
#
# **Its relation to AUC is exact and worth stating.** Per-user AUC is
# sum_{(u,i,j)} z_u delta(x_hat_uij > 0). Apart from the normalising
# constant, BPR-Opt differs *only* in the loss: AUC uses the
# non-differentiable Heaviside delta(x>0), BPR uses ln sigma(x).
# Replacing the Heaviside with a similarly shaped function is common
# practice and usually heuristic; here the substitution falls out of
# the maximum-likelihood derivation.
#
# **Learning.** LearnBPR is stochastic gradient ascent over triples
# drawn from D_S -- drawn, not enumerated, because |D_S| is enormous
# and sweeping user-wise or item-wise makes consecutive updates
# dependent on the same item. The model only has to supply
# d x_hat_uij / d theta; for matrix factorisation with
# x_hat_ui = <w_u, h_i> and x_hat_uij = x_hat_ui - x_hat_uj, that is
# h_if - h_jf for w_uf, w_uf for h_if, and -w_uf for h_jf.
#
# **One printed sign is wrong, and it is not cosmetic.** Figure 4 gives
#
#     Theta <- Theta + alpha( e^{-x_hat_uij} / (1 + e^{-x_hat_uij})
#                            * d/dTheta x_hat_uij
#                            + lambda_Theta * Theta ),
#
# but ascending sum ln sigma - lambda ||Theta||^2 requires
# -lambda_Theta Theta: as printed, the regulariser *grows* the
# parameters at every step. learn_bpr uses the correct sign and
# regularizer_sign="paper" reproduces the printed one, so the
# divergence can be seen rather than argued about.
#
# References
# ----------
# Rendle, S., Freudenthaler, C., Gantner, Z. & Schmidt-Thieme, L.
# (2009) "BPR: Bayesian Personalized Ranking from Implicit Feedback",
# Proceedings of the Twenty-Fifth Conference on Uncertainty in
# Artificial Intelligence (UAI 2009), 452-461, arXiv:1205.2618.
# Sec. 3 (the objection to labelling all unobserved pairs negative,
# and the construction of D_S). Sec. 4.1 (the likelihood
# sigma(x_uij), the Gaussian prior, and BPR-Opt). Sec. 4.1.1 (the AUC
# analogy: identical but for the Heaviside vs ln sigma loss).
# Sec. 4.2 and Figure 4 (LearnBPR by bootstrap sampling stochastic
# gradient descent). Sec. 4.3 (matrix factorisation, x_uij = x_ui -
# x_uj, and the three derivative cases).
#
# Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
# Techniques for Recommender Systems", Computer 42(8), 30-37,
# doi:10.1109/MC.2009.263. The factorisation model class BPR is
# applied to here.
# """
#
# Bit-identical R mirror of src/morie/fn/bprMF.py. All randomness comes
# from the shared SplitMix64 stream (.ghc_rng / .ghc_unif), consumed
# draw for draw in the same order: U*K uniforms to fill W, I*K to fill
# H, then per LearnBPR iteration three uniforms (user index, positive
# item, negative item) plus up to 100 more for the negative-item
# rejection guard, exactly as the Python arm does.

.bprMF_EPS <- 1e-12
.bprMF_SIGNS <- c("correct", "paper")

.bprMF_sigmoid <- function(x) {
  v <- as.numeric(x)
  if (v >= 0) return(1 / (1 + exp(-v)))
  e <- exp(v)
  e / (1 + e)
}

.bprMF_predict <- function(W, H, u, i) {
  sum(W[[u + 1L]] * H[[i + 1L]])
}

.bprMF_triples <- function(pos, n_items) {
  out <- list()
  users <- sort(names(pos))
  for (nm in users) {
    seen <- pos[[nm]]
    seen_int <- as.integer(seen)
    for (ii in seq_along(seen_int)) {
      i <- seen_int[ii]
      for (j in seq_len(as.integer(n_items)) - 1L) {
        if (!(j %in% seen_int)) {
          out[[length(out) + 1L]] <- list(u = nm, i = i, j = j)
        }
      }
    }
  }
  out
}

# Compute sum ln sigma(x_ui - x_uj) and lambda * ||Theta||^2 over the
# (seen, unseen) pairs per user; both the total BPR-Opt and the two
# pieces are returned so callers that want just the data loglik or just
# the penalty term can pull them out.
.bprMF_bpr_opt <- function(W, H, pos, n_items, lam = 0.01) {
  lm <- as.numeric(lam)
  n_items <- as.integer(n_items)
  users <- sort(names(pos))
  tot <- 0
  ntriples <- 0L
  for (nm in users) {
    seen <- as.integer(pos[[nm]])
    seen_set <- as.integer(unique(seen))
    u_idx <- as.integer(nm)
    for (ii in seq_along(seen_set)) {
      i <- seen_set[ii]
      xi <- .bprMF_predict(W, H, u_idx, i)
      for (j in seq_len(n_items) - 1L) {
        if (!(j %in% seen_set)) {
          xj <- .bprMF_predict(W, H, u_idx, j)
          tot <- tot + log(max(.bprMF_sigmoid(xi - xj), .bprMF_EPS))
          ntriples <- ntriples + 1L
        }
      }
    }
  }
  sq <- 0
  for (r in W) for (v in r) sq <- sq + v * v
  for (r in H) for (v in r) sq <- sq + v * v
  penalty <- lm * sq
  list(bpr_opt = tot - penalty, loglik = tot, penalty = penalty,
       n_triples = ntriples)
}

.bprMF_auc <- function(W, H, pos, n_items) {
  n_items <- as.integer(n_items)
  users <- sort(names(pos))
  if (length(users) == 0L) stop("bprMF: no users with positive feedback")
  per <- list()
  tot <- 0
  for (nm in users) {
    u_idx <- as.integer(nm)
    seen <- as.integer(pos[[nm]])
    seen_set <- as.integer(unique(seen))
    neg <- (seq_len(n_items) - 1L)
    neg <- neg[!(neg %in% seen_set)]
    if (length(seen_set) == 0L || length(neg) == 0L) {
      stop(sprintf("bprMF: user %s has no comparable pair", nm))
    }
    c_ <- 0
    for (i in seen_set) {
      xi <- .bprMF_predict(W, H, u_idx, i)
      for (j in neg) {
        xj <- .bprMF_predict(W, H, u_idx, j)
        if ((xi - xj) > 0) c_ <- c_ + 1
      }
    }
    per[[nm]] <- c_ / (length(seen_set) * length(neg))
    tot <- tot + per[[nm]]
  }
  list(auc = tot / length(users), per_user = per,
       note = "delta(x > 0) is strict: ties count as wrong")
}

# LearnBPR: bootstrap-sampled stochastic gradient ascent over triples
# drawn from D_S. Mirrors morie.fn.bprMF.learn_bpr step for step, with
# the same draw order from the shared SplitMix64 stream: U*K uniforms
# fill W, I*K uniforms fill H, then per iteration three uniforms (user
# index, positive item, negative item) plus up to 100 negative-item
# rejection-guard uniforms.  regularizer_sign="paper" reproduces the
# printed Figure 4 update whose +lambda*Theta term diverges; the
# default is the sign that actually ascends BPR-Opt.
.bprMF_learn_bpr <- function(pos, n_users, n_items, k_dim = 8L,
                             alpha = 0.05, lam = 0.01, iters = 2000L,
                             seed = 0L, regularizer_sign = "correct",
                             init_scale = 0.1) {
  reg <- as.character(regularizer_sign)
  if (!(reg %in% .bprMF_SIGNS)) {
    stop(sprintf("bprMF: regularizer_sign must be one of %s, got %s",
                 paste(.bprMF_SIGNS, collapse = ", "),
                 sQuote(reg)))
  }
  U <- as.integer(n_users); I <- as.integer(n_items); K <- as.integer(k_dim)
  if (U < 1L || I < 2L || K < 1L) {
    stop("bprMF: need at least 1 user, 2 items and 1 factor")
  }
  users <- sort(names(pos))
  if (length(users) == 0L) stop("bprMF: no positive feedback given")
  e <- .ghc_rng(as.integer(seed))
  W <- vector("list", U)
  raw_w <- (.ghc_unif(e, U * K) - 0.5) * 2 * as.numeric(init_scale)
  for (u in seq_len(U)) {
    lo <- (u - 1L) * K
    W[[u]] <- raw_w[(lo + 1L):(lo + K)]
  }
  H <- vector("list", I)
  raw_h <- (.ghc_unif(e, I * K) - 0.5) * 2 * as.numeric(init_scale)
  for (ii in seq_len(I)) {
    lo <- (ii - 1L) * K
    H[[ii]] <- raw_h[(lo + 1L):(lo + K)]
  }
  sgn <- if (reg == "correct") -1 else 1
  a <- as.numeric(alpha); lm <- as.numeric(lam)
  history <- numeric(0)
  checkpoint_every <- max(1L, as.integer(iters) %/% 20L)
  for (it in seq_len(as.integer(iters))) {
    u <- users[as.integer(floor(.ghc_unif(e, 1L) * length(users))) %% length(users) + 1L]
    u_idx <- as.integer(u)
    seen <- as.integer(pos[[u]])
    seen_len <- length(seen)
    i <- seen[as.integer(floor(.ghc_unif(e, 1L) * seen_len)) %% seen_len + 1L]
    j <- as.integer(floor(.ghc_unif(e, 1L) * I)) %% I
    seen_set <- seen
    guard <- 0L
    while (j %in% seen_set && guard < 100L) {
      j <- as.integer(floor(.ghc_unif(e, 1L) * I)) %% I
      guard <- guard + 1L
    }
    if (j %in% seen_set) next
    xi <- .bprMF_predict(W, H, u_idx, i)
    xj <- .bprMF_predict(W, H, u_idx, j)
    g <- .bprMF_sigmoid(-(xi - xj))
    wu <- W[[u_idx + 1L]]; hi <- H[[i + 1L]]; hj <- H[[j + 1L]]
    for (f_ in seq_len(K)) {
      wuf <- wu[f_]; hif <- hi[f_]; hjf <- hj[f_]
      wu[f_] <- wuf + a * (g * (hif - hjf) + sgn * lm * wuf)
      hi[f_] <- hif + a * (g * wuf + sgn * lm * hif)
      hj[f_] <- hjf + a * (g * (-wuf) + sgn * lm * hjf)
    }
    W[[u_idx + 1L]] <- wu; H[[i + 1L]] <- hi; H[[j + 1L]] <- hj
    if (it %% checkpoint_every == 0L) {
      history <- c(history,
                   .bprMF_bpr_opt(W, H, pos, I, lm)$bpr_opt)
    }
  }
  sq <- 0
  for (r in W) for (v in r) sq <- sq + v * v
  for (r in H) for (v in r) sq <- sq + v * v
  norm <- sqrt(sq)
  auc_obj <- .bprMF_auc(W, H, pos, I)
  list(estimate = list(W = W, H = H), W = W, H = H, k = K,
       bpr_opt_history = history,
       final_bpr_opt = if (length(history) > 0L) history[length(history)]
                       else NaN,
       auc = auc_obj$auc, param_norm = norm,
       regularizer_sign = reg,
       method = "LearnBPR, bootstrap SGD; Rendle et al. (2009) Fig. 4",
       caveat = if (reg == "paper")
         "the printed update adds +lambda*Theta, which grows the parameters; this run used that sign"
       else
         "regulariser sign corrected to -lambda*Theta, which is what ascending BPR-Opt requires")
}

# Rank items for one user by x_hat_ui.
.bprMF_recommend <- function(W, H, u, n_items, top_k = 5L,
                             exclude = integer(0)) {
  u_idx <- as.integer(u)
  n_items <- as.integer(n_items)
  top_k <- as.integer(top_k)
  ex <- as.integer(unique(exclude))
  scores <- list()
  nscored <- 0L
  for (i in seq_len(n_items) - 1L) {
    if (i %in% ex) next
    scores[[length(scores) + 1L]] <- list(i = i, s = .bprMF_predict(W, H, u_idx, i))
    nscored <- nscored + 1L
  }
  scores_sorted <- scores[order(-vapply(scores, function(z) z$s, numeric(1)))]
  ranking <- lapply(seq_len(min(top_k, length(scores_sorted))),
                    function(k) scores_sorted[[k]])
  list(ranking = ranking, n_scored = nscored)
}

#' BPR sigmoid
#'
#' Numerically stable \eqn{\sigma(x) = 1 / (1 + e^{-x})}, mirrored from
#' morie.fn.bprMF.sigmoid.
#'
#' @param x Numeric scalar (or vector).
#' @return Numeric of length 1 (or matching length) with \eqn{\sigma(x)}.
#' @references Rendle, S. et al. (2009) UAI 2009, 452-461,
#'   arXiv:1205.2618, Sec. 4.1.
#' @export
bpr_sigmoid <- function(x) .bprMF_sigmoid(x)

#' Predict x_hat_ui = <w_u, h_i>
#'
#' Mirrors morie.fn.bprMF.predict.
#'
#' @param W List of length \code{n_users}; each element a length-k
#'   numeric vector (0-based row indexing).
#' @param H List of length \code{n_items}; each element a length-k
#'   numeric vector (0-based row indexing).
#' @param u 0-based user index.
#' @param i 0-based item index.
#' @return Scalar inner product.
#' @export
bpr_predict <- function(W, H, u, i) .bprMF_predict(W, H, u, i)

#' BPR-Opt and its loglik / penalty pieces
#'
#' \eqn{\sum_{(u,i,j) in D_S} \ln \sigma(\hat x_{uij}) - \lambda \|\Theta\|^2}.
#' Mirrors morie.fn.bprMF.bpr_opt.
#'
#' @param W List of length \code{n_users}; each element a length-k numeric vector.
#' @param H List of length \code{n_items}; each element a length-k numeric vector.
#' @param pos Named list of positive-item indices per user (0-based).
#' @param n_items Number of items.
#' @param lam Regularisation strength \eqn{\lambda}.
#' @return A list with \code{bpr_opt}, \code{loglik}, \code{penalty},
#'   \code{n_triples}.
#' @export
bpr_opt_R <- function(W, H, pos, n_items, lam = 0.01)
  .bprMF_bpr_opt(W, H, pos, n_items, lam)

#' Per-user AUC for a BPR matrix-factorisation
#'
#' Eq. (1) of Rendle et al. (2009): \eqn{\sum z_u \delta(\hat x_{uij} > 0)}.
#' The indicator is strict, so a tie scores zero.
#'
#' @inheritParams bpr_opt_R
#' @return List with \code{auc}, \code{per_user}, \code{note}.
#' @export
bpr_auc_R <- function(W, H, pos, n_items)
  .bprMF_auc(W, H, pos, n_items)

#' LearnBPR: bootstrap-sampled stochastic gradient ascent
#'
#' Mirrors morie.fn.bprMF.learn_bpr step for step, draw for draw.
#' \code{regularizer_sign="paper"} reproduces the printed Figure 4
#' update, whose \code{+lambda*Theta} term diverges; the default
#' \code{"correct"} is the sign that actually ascends BPR-Opt.
#'
#' @param pos Named list of positive-item indices per user (0-based).
#' @param n_users Number of users.
#' @param n_items Number of items.
#' @param k_dim Latent factor dimension.
#' @param alpha Step size.
#' @param lam Regularisation strength.
#' @param iters Number of iterations.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @param regularizer_sign \code{"correct"} (default) or \code{"paper"}.
#' @param init_scale Initial parameter scale on [-init_scale, init_scale].
#' @return List with \code{estimate}, \code{W}, \code{H}, \code{k},
#'   \code{bpr_opt_history}, \code{final_bpr_opt}, \code{auc},
#'   \code{param_norm}, \code{regularizer_sign}, \code{method},
#'   \code{caveat}.
#' @references Rendle, S. et al. (2009) UAI 2009, 452-461,
#'   arXiv:1205.2618, Sec. 4.2 / Fig. 4.
#' @export
bpr_learn_bpr_R <- function(pos, n_users, n_items, k_dim = 8L,
                            alpha = 0.05, lam = 0.01, iters = 2000L,
                            seed = 0L, regularizer_sign = "correct",
                            init_scale = 0.1)
  .bprMF_learn_bpr(pos, n_users, n_items, k_dim, alpha, lam, iters,
                   seed, regularizer_sign, init_scale)

#' Recommend top items for one user by x_hat_ui
#'
#' Mirrors morie.fn.bprMF.recommend.
#'
#' @inheritParams bpr_predict
#' @inheritParams bpr_opt_R
#' @param top_k Number of items to return.
#' @param exclude Integer vector of 0-based item indices to drop.
#' @return List with \code{ranking} (list of \code{list(i=, s=)} up to
#'   \code{top_k}) and \code{n_scored}.
#' @export
bpr_recommend_R <- function(W, H, u, n_items, top_k = 5L,
                            exclude = integer(0))
  .bprMF_recommend(W, H, u, n_items, top_k, exclude)

# Public aliases matching the Python fn/_lazy_map.json names so callers
# can dispatch the R arm under the same identifier.
bpr_mf <- bpr_learn_bpr_R
bprmf <- bpr_learn_bpr_R
bayesianpersonalizedranking <- bpr_learn_bpr_R

.bprMF_cheatsheet <- function() {
  paste("bprMF: implicit feedback is positive-only, and labelling every",
        "unobserved pair NEGATIVE trains the model to predict 0 on",
        "exactly the items it must rank later. Use TRIPLES instead:",
        "D_S = {(u,i,j) : i seen, j unseen}. BPR-Opt = sum ln",
        "sigma(x_ui - x_uj) - lambda||Theta||^2, which is per-user AUC",
        "with the Heaviside replaced by ln sigma -- and that",
        "substitution comes from the MLE, not from convenience.",
        "LearnBPR SAMPLES triples rather than sweeping them. The",
        "printed update's +lambda*Theta is a sign error and diverges.")
}

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid

#' @rdname bpr_sigmoid
#' @export
morie_bprMF <- bpr_sigmoid
