# morie.fn -- function file (rootcoder007/morie)
# Individualized treatment rules from a causal forest.
#
# Given an estimated conditional treatment effect tau-hat(x), the
# rule that maximises the mean outcome treats exactly where the
# effect is positive,
#
#     d-hat(x) = 1{tau-hat(x) > eta},
#
# with eta = 0 when treatment is free and eta the cost per unit
# otherwise. The value of a rule is V(d) = E[Y(d(X))], estimated by
# the doubly robust score
#
#     Gamma-hat_i = mu-hat_{d(X_i)}(X_i)
#                 + 1{W_i = d(X_i)} / e_{W_i}(X_i)
#                   * (Y_i - mu-hat_{W_i}(X_i)),
#
# so a mistake in either the outcome model or the propensity is
# survivable but a mistake in both is not.
#
# Evaluating a rule on the data that produced it is the trap. The
# rule is the argmax of a noisy surface, so scoring it in sample
# inherits the winner's curse: a rule fitted to pure noise still
# looks profitable. evaluate="split" fits tau-hat on one half and
# scores on the other, and the anchor builds a no-effect design
# where the in-sample value is positive and the split-sample value
# is not.
#
# References
# ----------
# Athey, S. & Wager, S. (2021) "Policy Learning With Observational
#   Data", Econometrica 89(1), 133-161, doi:10.3982/ECTA15732. The
#   doubly robust scores and the regret bound for the learned rule.
#
# Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
#   Forests", The Annals of Statistics 47(2), 1148-1178,
#   doi:10.1214/18-AOS1709, arXiv:1610.01271. The forest that
#   supplies tau-hat.
#
# Zhao, Y., Zeng, D., Rush, A. J. & Kosorok, M. R. (2012)
#   "Estimating Individualized Treatment Rules Using Outcome
#   Weighted Learning", Journal of the American Statistical
#   Association 107(499), 1106-1118, doi:10.1080/01621459.2012.695674.
#   The value-maximisation framing.
#
# Manski, C. F. (2004) "Statistical Treatment Rules for
#   Heterogeneous Populations", Econometrica 72(4), 1221-1246,
#   doi:10.1111/j.1468-0262.2004.00530.x. Treatment rules as the
#   object of inference.

#' .itrgrf_policy_from_tau
#'
#' A step of the itrgrf_native implementation. Called by \code{morie_itrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tau Coerced to numeric by the body, with \code{as.numeric}.
#' @param cost Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return The value of \code{ifelse}.
#' @export
.itrgrf_policy_from_tau <- function(tau, cost = 0.0) {
  ifelse(as.numeric(tau) > as.numeric(cost), 1.0, 0.0)
}

#' .itrgrf_dr_scores
#'
#' A step of the itrgrf_native implementation. Called by \code{.itrgrf_rule_value}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken and its elements indexed.
#' @param W A vector; indexed elementwise.
#' @param mu1 A vector; indexed elementwise.
#' @param mu0 A vector; indexed elementwise.
#' @param e A vector; indexed elementwise.
#' @param d A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.itrgrf_dr_scores <- function(y, W, mu1, mu0, e, d) {
  n <- length(y)
  eps <- 1e-12
  out <- numeric(n)
  for (i in seq_len(n)) {
    pick <- d[i]
    mu <- if (pick == 1.0) mu1[i] else mu0[i]
    ew <- if (W[i] == 1.0) e[i] else 1.0 - e[i]
    if (ew <= eps) {
      stop(sprintf("itrgrf: a propensity of zero at row %d", i))
    }
    resid <- 0.0
    if (W[i] == pick) {
      muw <- if (W[i] == 1.0) mu1[i] else mu0[i]
      resid <- (y[i] - muw) / ew
    }
    out[i] <- mu + resid
  }
  out
}

#' .itrgrf_rule_value
#'
#' A step of the itrgrf_native implementation. Called by \code{morie_itrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{.itrgrf_dr_scores}.
#' @param W Passed to \code{.itrgrf_dr_scores}.
#' @param mu1 Passed to \code{.itrgrf_dr_scores}.
#' @param mu0 Passed to \code{.itrgrf_dr_scores}.
#' @param e Passed to \code{.itrgrf_dr_scores}.
#' @param d Passed to \code{.itrgrf_dr_scores}.
#' @return A list with \code{value}, \code{se}, \code{scores}.
#' @export
.itrgrf_rule_value <- function(y, W, mu1, mu0, e, d) {
  g <- .itrgrf_dr_scores(y, W, mu1, mu0, e, d)
  n <- length(g)
  v <- sum(g) / n
  se <- if (n > 1) k.sd(g) / sqrt(n) else NaN
  list(value = v, se = se, scores = g)
}

#' .itrgrf_fit_arm
#'
#' A step of the itrgrf_native implementation. Called by \code{morie_itrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param W A vector; indexed elementwise.
#' @param arm See Usage.
#' @param rows A vector; indexed elementwise.
#' @param at_rows A vector; its length is taken and its elements indexed.
#' @param n_trees See Usage.
#' @param min_leaf Numeric; combined arithmetically in the body.
#' @param seed See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.itrgrf_fit_arm <- function(X, y, W, arm, rows, at_rows, n_trees,
                            min_leaf, seed) {
  idx <- rows[W[rows] == arm]
  if (length(idx) < 4 * min_leaf) {
    stop(sprintf("itrgrf: too few rows in treatment arm %g", arm))
  }
  Xa <- X[idx, , drop = FALSE]
  ya <- y[idx]
  forest <- grow_forest(Xa, ya, n_trees = n_trees,
                        min_leaf = min_leaf, seed = seed)
  trees <- forest$trees
  out <- numeric(length(at_rows))
  for (j in seq_along(at_rows)) {
    i <- at_rows[j]
    w <- forest_weights(trees, Xa, X[i, , drop = FALSE])
    out[j] <- sum(w * ya)
  }
  out
}

#' morie_itrgrf
#'
#' A step of the itrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param W Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param cost Passed to \code{.itrgrf_policy_from_tau}. Defaults to \code{0}.
#' @param n_trees Passed to \code{.itrgrf_fit_arm}. Defaults to \code{150}.
#' @param min_leaf Passed to \code{.itrgrf_fit_arm}. Defaults to \code{5}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param evaluate One of \code{"in-sample"}, \code{"split"}. Defaults to \code{"split"}.
#' @param propensity Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param level Numeric; combined arithmetically in the body. Defaults to \code{0.95}.
#' @return A list with \code{estimate}, \code{value}, \code{se}, \code{ci}, \code{rule}, \code{tau}, \code{mu1}, \code{mu0}, \code{treated_fraction}, \code{value_treat_all}, \code{value_treat_none}, \code{gain_over_treat_all}, \code{gain_over_treat_none}, \code{scores}, \code{cost}, \code{evaluate}, \code{n}, \code{n_scored}, \code{level}, \code{method}.
#' @export
morie_itrgrf <- function(y, W, X, cost = 0.0, n_trees = 150,
                         min_leaf = 5, seed = 0, evaluate = "split",
                         propensity = NULL, level = 0.95) {
  if (!(evaluate %in% c("split", "in-sample"))) {
    stop(sprintf("itrgrf: evaluate must be split or in-sample, got %s",
                 deparse(evaluate)))
  }
  yv <- as.numeric(y)
  Wv <- as.numeric(W)
  n <- length(yv)
  if (length(Wv) != n) {
    stop(sprintf("itrgrf: %d outcomes but %d treatments",
                 n, length(Wv)))
  }
  if (any(!(Wv %in% c(0.0, 1.0)))) {
    stop("itrgrf: the treatment must be binary 0/1")
  }
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) {
    stop(sprintf("itrgrf: %d covariate rows for %d outcomes",
                 nrow(Xm), n))
  }
  if (n < 60) {
    stop(sprintf("itrgrf: need at least 60 observations, got %d", n))
  }
  if (is.null(propensity)) {
    e <- rep(sum(Wv) / n, n)
  } else {
    e <- pmin(pmax(as.numeric(propensity), 1e-3), 1.0 - 1e-3)
    if (length(e) != n) {
      stop(sprintf("itrgrf: %d propensities for %d rows",
                   length(e), n))
    }
  }

  rng <- .ghc_rng(seed)
  if (evaluate == "split") {
    half <- n %/% 2
    perm <- order(.ghc_unif(rng, n))
    learn <- perm[seq_len(half)]
    score <- perm[(half + 1L):n]
  } else {
    learn <- seq_len(n)
    score <- seq_len(n)
  }

  all_rows <- seq_len(n)
  mu1 <- .itrgrf_fit_arm(Xm, yv, Wv, 1.0, learn, all_rows, n_trees,
                         min_leaf, seed)
  mu0 <- .itrgrf_fit_arm(Xm, yv, Wv, 0.0, learn, all_rows, n_trees,
                         min_leaf, seed + 1)
  tau <- mu1 - mu0
  d <- .itrgrf_policy_from_tau(tau, cost)

  ys <- yv[score]
  Ws <- Wv[score]
  mu1s <- mu1[score]
  mu0s <- mu0[score]
  es <- e[score]
  n_score <- length(score)

  rv <- .itrgrf_rule_value(ys, Ws, mu1s, mu0s, es, d[score])
  v <- rv$value
  se <- rv$se
  g <- rv$scores

  rv_all <- .itrgrf_rule_value(ys, Ws, mu1s, mu0s, es,
                               rep(1.0, n_score))
  rv_none <- .itrgrf_rule_value(ys, Ws, mu1s, mu0s, es,
                                rep(0.0, n_score))
  v_all <- rv_all$value
  v_none <- rv_none$value

  z <- k.qnorm(0.5 + 0.5 * level)

  list(
    estimate = v,
    value = v,
    se = se,
    ci = c(v - z * se, v + z * se),
    rule = d,
    tau = tau,
    mu1 = mu1,
    mu0 = mu0,
    treated_fraction = sum(d) / n,
    value_treat_all = v_all,
    value_treat_none = v_none,
    gain_over_treat_all = v - v_all,
    gain_over_treat_none = v - v_none,
    scores = g,
    cost = as.numeric(cost),
    evaluate = evaluate,
    n = n,
    n_scored = n_score,
    level = as.numeric(level),
    method = "individualized treatment rule from a causal forest, valued by doubly robust scores, Athey & Wager (2021)"
  )
}

#' .itrgrf_cheatsheet
#'
#' A step of the itrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.itrgrf_cheatsheet <- function() {
  "itrgrf: d(x) = 1{tau(x) > cost}; value it with the doubly robust score mu_d(X) + 1{W=d}/e_W (Y - mu_W). Learn the rule and score it on DIFFERENT halves -- the rule is an argmax, so scoring it in sample inherits the winner's curse and a rule fitted to noise looks profitable."
}

# compact alias per ledger/NAMING.md
itrforest <- morie_itrgrf
