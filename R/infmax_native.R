# morie.fn -- function file (rootcoder007/morie)
# infmax.R -- Deep InfoMax: maximise mutual information LOCALLY.
#
# Maximising the mutual information between an input and its
# representation is an old idea, and on its own it is a bad objective:
# MI is invariant to invertible transformations, so a representation
# that memorises pixel noise scores as well as one that captures
# content.
#
# The fix is where the information is measured. Deep InfoMax
# maximises MI between the global summary vector and local
# patches of the feature map -- so a representation is rewarded for
# containing what is shared across many locations, and a feature
# explaining a single patch of noise earns nothing. That is the paper's
# central result: local structure in the input is what makes the
# representation good for classification, and local_objective
# against global_objective measures the difference rather than
# asserting it.
#
# The estimator matters too. The Donsker-Varadhan bound on the KL
# divergence has an expectation inside a logarithm, so its gradient is
# biased and its variance explodes with the batch. The
# Jensen-Shannon form,
#   I_JSD = E_P[-sp(-T(x,y))] - E_{PxP~}[sp(T(x',y))],
# with sp(z) = log(1+exp(z)), is bounded, stable, and gives
# better results -- the paper's own comparison, and the reason it is
# the default here. Both are implemented, so the instability can be seen.
#
# One global feature, one estimator, one step. Unlike CPC, which
# processes local features sequentially and predicts the "future" of a
# summary with separate estimators, DIM's single global feature predicts
# all local features simultaneously.
#
# References
# ----------
# Hjelm, R. D., Fedorov, A., Lavoie-Marchildon, S., Grewal, K.,
# Bachman, P., Trischler, A. & Bengio, Y. (2019) "Learning deep
# representations by mutual information estimation and maximization",
# International Conference on Learning Representations (ICLR 2019),
# arXiv:1808.06670. Sec. 2-3: that maximising MI between the input and
# output of an encoder can be done with an MI estimator; that a
# Jensen-Shannon-divergence-based alternative to the Donsker-Varadhan
# KL estimator is more stable and provides better results; that
# structure-aware objectives -- maximising MI between the global feature
# and LOCAL patches of the feature map -- improve the suitability of the
# representation for classification; and the comparison with CPC, which
# processes local features sequentially to build summary features and
# predicts specific local features autoregressively with separate
# estimators, whereas DIM uses a single global summary feature that
# predicts all local features simultaneously in one step with one
# estimator.
#
# van den Oord, A., Li, Y. & Vinyals, O. (2018) "Representation
# Learning with Contrastive Predictive Coding", arXiv:1807.03748. CPC,
# the ordered-autoregression alternative.
#
# Belghazi, M. I., Baratin, A., Rajeswar, S., Ozair, S., Bengio, Y.,
# Courville, A. & Hjelm, R. D. (2018) "Mutual Information Neural
# Estimation", ICML 2018, PMLR 80, 531-540, arXiv:1801.04062. The
# Donsker-Varadhan estimator being replaced.

.infmax_EPS <- 1e-12

.infmax_softplus <- function(z) {
  # sp(z) = log(1 + exp(z)), branch-stable so neither branch overflows.
  v <- as.numeric(z)
  out <- numeric(length(v))
  pos <- v > 0
  out[pos]  <- v[pos] + log1p(exp(-v[pos]))
  out[!pos] <- log1p(exp(v[!pos]))
  out
}

.infmax_jsd_estimator <- function(joint_scores, marginal_scores) {
  # I_JSD = E_P[-sp(-T)] - E_{PxP~}[sp(T)].
  J <- as.numeric(unlist(joint_scores))
  M <- as.numeric(unlist(marginal_scores))
  if (length(J) == 0L || length(M) == 0L) {
    stop("infmax: both joint and marginal samples are needed")
  }
  pos <- sum(-.infmax_softplus(-J)) / length(J)
  neg <- sum(.infmax_softplus(M))   / length(M)
  list(estimate = pos - neg,
       positive = pos,
       negative = neg,
       bounded  = TRUE,
       note     = "each term is bounded by construction")
}

.infmax_dv_estimator <- function(joint_scores, marginal_scores) {
  # I_DV = E_P[T] - log E_{PxP~}[exp(T)].
  J <- as.numeric(unlist(joint_scores))
  M <- as.numeric(unlist(marginal_scores))
  if (length(J) == 0L || length(M) == 0L) {
    stop("infmax: both joint and marginal samples are needed")
  }
  mx     <- max(M)
  lse    <- mx + log(sum(exp(M - mx)) / length(M))
  mean_m <- mean(M)
  var    <- sum((M - mean_m)^2) / max(length(M) - 1L, 1L)
  list(estimate          = mean(J) - lse,
       log_sum_exp       = lse,
       negative_variance = var,
       bounded           = FALSE,
       note              = "unbounded above; large scores dominate the log-mean-exp")
}

.infmax_global_objective <- function(global_features, feature_maps, critic,
                                     estimator = "jsd") {
  # MI between the global vector and the WHOLE feature map.
  G <- lapply(global_features, function(r) as.numeric(unlist(r)))
  F <- lapply(feature_maps,    function(m) as.numeric(unlist(m)))
  n <- length(G)
  if (length(F) != n) {
    stop(sprintf("infmax: %d globals but %d feature maps", n, length(F)))
  }
  if (n < 2L) {
    stop("infmax: negatives come from other examples in the batch, so at least 2 are needed")
  }
  joint <- numeric(n)
  for (i in seq_len(n)) {
    joint[i] <- as.numeric(critic(G[[i]], F[[i]]))
  }
  marg <- numeric(n * (n - 1L))
  k <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j != i) {
        k <- k + 1L
        marg[k] <- as.numeric(critic(G[[i]], F[[j]]))
      }
    }
  }
  marg <- marg[seq_len(k)]
  est  <- if (identical(estimator, "jsd")) .infmax_jsd_estimator
          else                            .infmax_dv_estimator
  r    <- est(joint, marg)
  list(objective  = r$estimate,
       estimator  = estimator,
       n_positive = length(joint),
       n_negative = length(marg),
       note       = "one score per image; the spatial structure is discarded")
}

.infmax_local_objective <- function(global_features, feature_maps, critic,
                                    estimator = "jsd") {
  # MI between the global vector and EACH LOCAL patch, averaged.
  G <- lapply(global_features, function(r) as.numeric(unlist(r)))
  M <- lapply(feature_maps, function(m) {
    lapply(m, function(p) as.numeric(unlist(p)))
  })
  n <- length(G)
  if (length(M) != n) {
    stop(sprintf("infmax: %d globals but %d feature maps", n, length(M)))
  }
  if (n < 2L) {
    stop("infmax: at least 2 examples are needed for negatives")
  }
  L <- length(M[[1]])
  if (any(vapply(M, length, integer(1)) != L)) {
    stop("infmax: the feature maps have differing numbers of locations")
  }
  joint <- numeric(n * L)
  marg  <- numeric(n * L * (n - 1L))
  ji <- 0L
  mi <- 0L
  for (i in seq_len(n)) {
    for (l in seq_len(L)) {
      ji <- ji + 1L
      joint[ji] <- as.numeric(critic(G[[i]], M[[i]][[l]]))
      for (j in seq_len(n)) {
        if (j != i) {
          mi <- mi + 1L
          marg[mi] <- as.numeric(critic(G[[i]], M[[j]][[l]]))
        }
      }
    }
  }
  joint <- joint[seq_len(ji)]
  marg  <- marg[seq_len(mi)]
  est   <- if (identical(estimator, "jsd")) .infmax_jsd_estimator
           else                            .infmax_dv_estimator
  r     <- est(joint, marg)
  list(estimate    = r$estimate,
       objective   = r$estimate,
       estimator   = estimator,
       n_locations = L,
       n_positive  = length(joint),
       n_negative  = length(marg),
       method      = "Deep InfoMax local objective; Hjelm et al. (2019)",
       note        = "the global feature predicts ALL locations at once, with ONE estimator and no autoregression")
}

.infmax_cheatsheet <- function() {
  paste0("infmax: maximising MI between input and representation is ",
         "a bad objective alone -- MI is invariant to invertible ",
         "maps, so memorising noise scores as well as capturing ",
         "content. Measure it LOCALLY instead: between the global ",
         "summary and each patch of the feature map, so a feature ",
         "must pay off at many locations. Use the JENSEN-SHANNON ",
         "estimator, -sp(-T) minus sp(T), which is BOUNDED, rather ",
         "than Donsker-Varadhan, whose expectation sits inside a ",
         "log and whose variance explodes. Unlike CPC there is ONE ",
         "global feature, ONE estimator, and no autoregression.")
}

# compact alias per ledger/NAMING.md
.infmax_deepinfomax <- .infmax_local_objective

# public names resolved by fn/_lazy_map.json
.infmax_infomax_objective <- .infmax_local_objective

# entry point -- the headline Deep InfoMax local objective
morie_infmax <- function(global_features, feature_maps, critic,
                         estimator = "jsd") {
  .infmax_local_objective(global_features, feature_maps, critic, estimator)
}
