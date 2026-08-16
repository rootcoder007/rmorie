# plrgrf.R -- function file (rootcoder007/morie)
# Partial-linear GRF: heterogeneous effects with high-dimensional controls.
#
# The partially linear model
#   Y_i = tau(X_i)*(W_i - e(X_i)) + m(X_i) + epsilon_i,
#   e(x) = E[W | X=x], m(x) = E[Y | X=x]
# separates what is being estimated, tau(.), from the nuisance surfaces
# m and e that merely have to be controlled for.
#
# Local centering is what makes it work, and skipping it is the usual
# mistake. Residualise first -- Ytilde = Y - mhat(X) and Wtilde = W -
# ehat(X) -- and only then fit the forest. The GRF estimating equation
# (2) with weights (3) then solves
#   tauhat(x) = sum_i alpha_i(x) Wtilde_i Ytilde_i /
#               sum_i alpha_i(x) Wtilde_i^2
# which is a weighted Robinson regression run in the forest's own
# neighbourhood. Without the centering, the forest spends its splits
# chasing variation in m(X) -- the confounding surface -- instead of
# variation in the treatment effect, and the estimate absorbs the
# confounder. The anchor builds a design where m is strong and tau is
# weak, which is exactly the case that separates them.
#
# Orthogonality is the reason the nuisances may be estimated roughly.
# The score psi_tau = (Ytilde - tau*Wtilde)*Wtilde has zero derivative
# in both nuisances at the truth, so first-order errors in mhat and
# ehat cancel rather than propagate -- the R-learner property.
#
# References:
# Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
# Forests", The Annals of Statistics 47(2), 1148-1178,
# doi:10.1214/18-AOS1709, arXiv:1610.01271. Eq. (2), (3), (8); Sec.
# 6.1.1 on local centering.
#
# Nie, X. & Wager, S. (2021) "Quasi-oracle estimation of heterogeneous
# treatment effects", Biometrika 108(2), 299-319,
# doi:10.1093/biomet/asaa076. The R-learner objective this solves
# locally.
#
# Robinson, P. M. (1988) "Root-N-Consistent Semiparametric Regression",
# Econometrica 56(4), 931-954, doi:10.2307/1912705. The partially
# linear model and the residual-on-residual construction.
#
# Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
# Newey, W. & Robins, J. (2018) "Double/debiased machine learning for
# treatment and structural parameters", The Econometrics Journal 21(1),
# C1-C68, doi:10.1111/ectj.12097. Neyman orthogonality and cross-fitting.

.plrgrf_eps <- 1e-12

#' .plrgrf_folds
#'
#' A step of the plrgrf_native implementation. Called by \code{local_centering}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param V Numeric; combined arithmetically in the body.
#' @return The value of \code{lapply}.
#' @export
.plrgrf_folds <- function(n, V) {
  V <- max(2, min(as.integer(V), n))
  lapply(0:(V - 1), function(v) which((seq_len(n) - 1) %% V == v))
}

#' .plrgrf_forest_predict
#'
#' A step of the plrgrf_native implementation. Called by \code{local_centering}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; indexed elementwise.
#' @param train See Usage.
#' @param at_rows A vector; its length is taken and its elements indexed.
#' @param n_trees See Usage.
#' @param min_leaf See Usage.
#' @param seed See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.plrgrf_forest_predict <- function(X, y, train, at_rows, n_trees, min_leaf, seed) {
  forest <- grow_forest(X[train, , drop = FALSE], y[train],
                        n_trees = n_trees, min_leaf = min_leaf, seed = seed)
  trees <- forest$trees
  Xt <- X[train, , drop = FALSE]
  out <- numeric(length(at_rows))
  for (i in seq_along(at_rows)) {
    w <- forest_weights(trees, Xt, X[at_rows[i], , drop = FALSE])
    out[i] <- sum(w * y[train])
  }
  out
}

#' local_centering
#'
#' A step of the plrgrf_native implementation. Called by \code{morie_plrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; its length is taken.
#' @param W Passed to \code{.plrgrf_forest_predict}.
#' @param X Passed to \code{.plrgrf_forest_predict}.
#' @param n_folds Passed to \code{.plrgrf_folds}. Defaults to \code{5}.
#' @param n_trees Passed to \code{.plrgrf_forest_predict}. Defaults to \code{100}.
#' @param min_leaf Passed to \code{.plrgrf_forest_predict}. Defaults to \code{5}.
#' @param seed Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return A list with \code{mh}, \code{eh}.
#' @export
local_centering <- function(y, W, X, n_folds = 5, n_trees = 100,
                            min_leaf = 5, seed = 0) {
  n <- length(y)
  mh <- numeric(n)
  eh <- numeric(n)
  folds <- .plrgrf_folds(n, n_folds)
  for (val in folds) {
    tr <- setdiff(seq_len(n), val)
    if (length(tr) == 0) next
    fm <- .plrgrf_forest_predict(X, y, tr, val, n_trees, min_leaf, seed)
    fe <- .plrgrf_forest_predict(X, W, tr, val, n_trees, min_leaf, seed + 1)
    mh[val] <- fm
    eh[val] <- fe
  }
  list(mh = mh, eh = eh)
}

#' residual_forest
#'
#' A step of the plrgrf_native implementation. Called by \code{morie_plrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y_res A vector; its length is taken.
#' @param w_res Numeric; combined arithmetically in the body.
#' @param X See Usage.
#' @param at Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param n_trees Defaults to \code{200}.
#' @param min_leaf Defaults to \code{5}.
#' @param seed Defaults to \code{0}.
#' @param alpha Defaults to \code{0.05}.
#' @param pi Defaults to \code{0.5}.
#' @return A list with \code{tau}, \code{info}.
#' @export
residual_forest <- function(y_res, w_res, X, at = NULL, n_trees = 200,
                            min_leaf = 5, seed = 0, alpha = 0.05, pi = 0.5) {
  n <- length(y_res)
  forest <- grow_forest(X, y_res, n_trees = n_trees, min_leaf = min_leaf,
                        seed = seed, alpha = alpha, pi = pi)
  trees <- forest$trees
  bags <- forest$bags
  s <- forest$s
  Q <- if (is.null(at)) X else as.matrix(at)
  Qn <- nrow(Q)
  tau <- numeric(Qn)
  num <- numeric(Qn)
  den <- numeric(Qn)
  for (q in seq_len(Qn)) {
    a <- forest_weights(trees, X, Q[q, , drop = FALSE])
    wbar <- sum(a * w_res)
    ybar <- sum(a * y_res)
    nu <- sum(a * (w_res - wbar) * (y_res - ybar))
    de <- sum(a * (w_res - wbar)^2)
    if (abs(de) < .plrgrf_eps) {
      stop("plrgrf: no treatment variation in the neighbourhood of a target point")
    }
    tau[q] <- nu / de
    num[q] <- nu
    den[q] <- de
  }
  list(tau = tau, info = list(trees = trees, bags = bags, s = s,
                              numerator = num, denominator = den))
}

#' morie_plrgrf
#'
#' A step of the plrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param W See Usage.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param at Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param n_trees Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @param n_folds Defaults to \code{5}.
#' @param min_leaf Defaults to \code{5}.
#' @param seed Defaults to \code{0}.
#' @param center A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param level Numeric; combined arithmetically in the body. Defaults to \code{0.95}.
#' @return A list with \code{estimate}, \code{tau}, \code{se}, \code{ci}, \code{m_hat}, \code{e_hat}, \code{y_residual}, \code{w_residual}, \code{centered}, \code{n}, \code{n_trees}, \code{ate}, \code{level}, \code{method}.
#' @export
morie_plrgrf <- function(y, W, X, at = NULL, n_trees = 200,
                         n_folds = 5, min_leaf = 5, seed = 0,
                         center = TRUE, level = 0.95) {
  yv <- as.numeric(y)
  Wv <- as.numeric(W)
  n <- length(yv)
  if (length(Wv) != n) {
    stop(sprintf("plrgrf: %d outcomes but %d treatments", n, length(Wv)))
  }
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) {
    stop(sprintf("plrgrf: %d covariate rows for %d outcomes", nrow(Xm), n))
  }
  if (n < 40) {
    stop(sprintf("plrgrf: need at least 40 observations, got %d", n))
  }
  if (center) {
    lc <- local_centering(yv, Wv, Xm, n_folds = n_folds,
                          n_trees = max(50, n_trees %/% 2),
                          min_leaf = min_leaf, seed = seed)
    mh <- lc$mh
    eh <- lc$eh
  } else {
    mh <- rep(0.0, n)
    eh <- rep(0.0, n)
  }
  yr <- yv - mh
  wr <- Wv - eh
  rf <- residual_forest(yr, wr, Xm, at = at, n_trees = n_trees,
                        min_leaf = min_leaf, seed = seed)
  tau <- rf$tau
  info <- rf$info
  Q <- if (is.null(at)) Xm else as.matrix(at)
  Qn <- nrow(Q)
  ses <- numeric(Qn)
  for (q in seq_len(Qn)) {
    a <- forest_weights(info$trees, Xm, Q[q, , drop = FALSE])
    wbar <- sum(a * wr)
    de <- info$denominator[q]
    psi <- a * (wr - wbar) * (yr - tau[q] * wr)
    v <- if (de != 0) sum(psi^2) / (de * de) else NaN
    ses[q] <- sqrt(max(v, 0))
  }
  z <- qnorm(0.5 + 0.5 * level)
  ci <- cbind(tau - z * ses, tau + z * ses)
  list(estimate = tau, tau = tau, se = ses, ci = ci,
       m_hat = mh, e_hat = eh, y_residual = yr, w_residual = wr,
       centered = as.logical(center), n = n, n_trees = as.integer(n_trees),
       ate = sum(tau) / length(tau), level = as.numeric(level),
       method = "partial-linear generalized random forest, Athey, Tibshirani & Wager (2019) eq. (2)-(3) with local centering")
}

#' .plrgrf_cheatsheet
#'
#' A step of the plrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.plrgrf_cheatsheet <- function() {
  "plrgrf: residualise FIRST -- Ytilde = Y - m(X), Wtilde = W - e(X), both cross-fitted -- then solve eq. (2) in the forest neighbourhood: tau(x) = sum a_i Wtilde Ytilde / sum a_i Wtilde^2. Skip the centering and the forest splits on m(X), the confounding surface, not on tau."
}

partial_linear_grf <- morie_plrgrf
partiallineargrf <- morie_plrgrf
