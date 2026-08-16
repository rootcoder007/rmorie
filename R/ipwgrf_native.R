# morie.fn -- function file (rootcoder007/morie)
# IPW-augmented forest: doubly robust average effects.
#
# A causal forest gives tau(x) pointwise. Averaging it gives an estimate
# of the average effect that inherits every bias in the outcome model,
# because nothing in the average corrects for how treatment was assigned.
#
# The augmented score fixes that, and only needs one of two models to be
# right.  For each observation,
#
#   Gamma_i = mu1(X_i) - mu0(X_i)
#             + W_i*(Y_i - mu1(X_i))/e(X_i)
#             - (1-W_i)*(Y_i - mu0(X_i))/(1 - e(X_i))
#
# whose mean estimates the ATE.  Cross-fit the nuisances, trim e, and
# report the largest weight -- a DR estimator with one weight of 500 is
# a one-observation estimator wearing a robustness argument.
#
# References
# ----------
# Robins, J. M., Rotnitzky, A. & Zhao, L. P. (1994).  The augmented IPW
#   score.  JASA 89(427), 846-866, doi:10.1080/01621459.1994.10476818.
# Athey, S., Tibshirani, J. & Wager, S. (2019).  Generalized Random
#   Forests.  Ann. Statist. 47(2), 1148-1178, doi:10.1214/18-AOS1709.
# Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
#   C., Newey, W. & Robins, J. (2018).  Double/debiased machine learning
#   for treatment and structural parameters.  Econometrics J. 21(1),
#   C1-C68, doi:10.1111/ectj.12097.
# Crump, R. K., Hotz, V. J., Imbens, G. W. & Mitnik, O. A. (2009).
#   Dealing with limited overlap in estimation of average treatment
#   effects.  Biometrika 96(1), 187-199, doi:10.1093/biomet/asn055.

.ipwgrf_EPS <- 1e-12

#' .ipwgrf_folds
#'
#' Part of the ipwgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @param V See Usage.
#' @return The value of \code{lapply}.
#' @export
.ipwgrf_folds <- function(n, V) {
  V <- as.integer(max(2, min(as.integer(V), n)))
  lapply(0:(V - 1), function(v) which(((seq_len(n) - 1) %% V) == v))
}

#' .ipwgrf_forest_nuisances
#'
#' Part of the ipwgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param W See Usage.
#' @param X See Usage.
#' @param n_folds Defaults to \code{5}.
#' @param n_trees Defaults to \code{120}.
#' @param min_leaf Defaults to \code{5}.
#' @param seed Defaults to \code{0}.
#' @return A list with \code{mu1}, \code{mu0}, \code{e}.
#' @export
.ipwgrf_forest_nuisances <- function(y, W, X, n_folds = 5, n_trees = 120,
                                    min_leaf = 5, seed = 0) {
  n <- length(y)
  mu1 <- rep(0.0, n)
  mu0 <- rep(0.0, n)
  e <- rep(0.0, n)

  for (val in .ipwgrf_folds(n, n_folds)) {
    tr <- setdiff(seq_len(n), val)
    if (length(tr) == 0L) next

    for (arm in c(1.0, 0.0)) {
      idx <- tr[W[tr] == arm]
      if (length(idx) < 4 * min_leaf) {
        stop(sprintf("ipwgrf: too few training rows in treatment arm %g", arm))
      }
      Xa <- X[idx, , drop = FALSE]
      ya <- y[idx]

      forest_result <- grow_forest(Xa, ya, n_trees = n_trees,
                                   min_leaf = min_leaf,
                                   seed = seed + as.integer(arm))
      trees <- forest_result[[1]]

      for (i in val) {
        w <- forest_weights(trees, Xa, X[i, , drop = FALSE])
        score <- sum(w * ya)
        if (arm == 1.0) {
          mu1[i] <- score
        } else {
          mu0[i] <- score
        }
      }
    }

    Xt <- X[tr, , drop = FALSE]
    Wt <- W[tr]
    forest_result <- grow_forest(Xt, Wt, n_trees = n_trees,
                                 min_leaf = min_leaf,
                                 seed = seed + 7)
    trees <- forest_result[[1]]

    for (i in val) {
      w <- forest_weights(trees, Xt, X[i, , drop = FALSE])
      e[i] <- sum(w * Wt)
    }
  }

  list(mu1 = mu1, mu0 = mu0, e = e)
}

#' .ipwgrf_aipw_scores
#'
#' Part of the ipwgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param W See Usage.
#' @param mu1 See Usage.
#' @param mu0 See Usage.
#' @param e See Usage.
#' @param trim Defaults to \code{0.02}.
#' @return A list with \code{g}, \code{weights}.
#' @export
.ipwgrf_aipw_scores <- function(y, W, mu1, mu0, e, trim = 0.02) {
  n <- length(y)
  t <- as.numeric(trim)
  if (!(0.0 <= t && t < 0.5)) {
    stop(sprintf("ipwgrf: trim must be in [0, 0.5), got %g", t))
  }

  g <- numeric(n)
  weights <- numeric(n)
  bound <- max(t, .ipwgrf_EPS)

  for (i in seq_len(n)) {
    ei <- min(max(as.numeric(e[i]), bound), 1.0 - bound)
    wt <- if (W[i] == 1) 1.0 / ei else 1.0 / (1.0 - ei)
    weights[i] <- wt
    g[i] <- mu1[i] - mu0[i] +
            W[i] * (y[i] - mu1[i]) / ei -
            (1.0 - W[i]) * (y[i] - mu0[i]) / (1.0 - ei)
  }

  list(g = g, weights = weights)
}

#' morie_ipwgrf
#'
#' Part of the ipwgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param W See Usage.
#' @param X See Usage.
#' @param n_folds Defaults to \code{5}.
#' @param n_trees Defaults to \code{120}.
#' @param min_leaf Defaults to \code{5}.
#' @param trim Defaults to \code{0.02}.
#' @param seed Defaults to \code{0}.
#' @param level Defaults to \code{0.95}.
#' @param break_outcome Defaults to \code{FALSE}.
#' @param break_propensity Defaults to \code{FALSE}.
#' @return A list with \code{estimate}, \code{ate}, \code{se}, \code{ci}, \code{scores}, \code{mu1}, \code{mu0}, \code{propensity}, \code{plug_in}, \code{max_weight}, \code{min_propensity}, \code{max_propensity}, \code{trim}, \code{n}, \code{level}, \code{broken_outcome}, \code{broken_propensity}, \code{method}.
#' @export
morie_ipwgrf <- function(y, W, X, n_folds = 5, n_trees = 120, min_leaf = 5,
                         trim = 0.02, seed = 0, level = 0.95,
                         break_outcome = FALSE, break_propensity = FALSE) {
  yv <- as.numeric(y)
  Wv <- as.numeric(W)
  n <- length(yv)

  if (length(Wv) != n) {
    stop(sprintf("ipwgrf: %d outcomes but %d treatments", n, length(Wv)))
  }
  if (!all(Wv == 0 | Wv == 1)) {
    stop("ipwgrf: the treatment must be binary 0/1")
  }
  if (!(sum(Wv) > 0 && sum(Wv) < n)) {
    stop("ipwgrf: both arms must be non-empty")
  }
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) {
    stop(sprintf("ipwgrf: %d covariate rows for %d outcomes",
                 nrow(Xm), n))
  }
  if (n < 60) {
    stop(sprintf("ipwgrf: need at least 60 observations, got %d", n))
  }

  nuisances <- .ipwgrf_forest_nuisances(yv, Wv, Xm,
                                        n_folds = n_folds,
                                        n_trees = n_trees,
                                        min_leaf = min_leaf,
                                        seed = seed)
  mu1 <- nuisances$mu1
  mu0 <- nuisances$mu0
  e   <- nuisances$e

  if (isTRUE(break_outcome)) {
    ybar <- sum(yv) / n
    mu1 <- rep(ybar, n)
    mu0 <- rep(ybar, n)
  }
  if (isTRUE(break_propensity)) {
    e <- rep(sum(Wv) / n, n)
  }

  scores <- .ipwgrf_aipw_scores(yv, Wv, mu1, mu0, e, trim = trim)
  g   <- scores$g
  wts <- scores$weights

  psi <- sum(g) / n
  se  <- if (n > 1) sd(g) / sqrt(n) else NaN
  z   <- qnorm(0.5 + 0.5 * as.numeric(level))
  plug <- sum(mu1 - mu0) / n

  list(
    estimate = psi,
    ate = psi,
    se = se,
    ci = c(psi - z * se, psi + z * se),
    scores = g,
    mu1 = mu1,
    mu0 = mu0,
    propensity = e,
    plug_in = plug,
    max_weight = max(wts),
    min_propensity = min(e),
    max_propensity = max(e),
    trim = as.numeric(trim),
    n = as.integer(n),
    level = as.numeric(level),
    broken_outcome = isTRUE(break_outcome),
    broken_propensity = isTRUE(break_propensity),
    method = paste("augmented IPW with forest-fitted nuisances,",
                   "Robins, Rotnitzky & Zhao (1994) score,",
                   "Athey, Tibshirani & Wager (2019) forests")
  )
}

#' .ipwgrf_cheatsheet
#'
#' Part of the ipwgrf_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.ipwgrf_cheatsheet <- function() {
  paste("ipwgrf: Gamma = mu1 - mu0 + W(Y-mu1)/e - (1-W)(Y-mu0)/(1-e),",
        "mean is the ATE. Right outcome model OR right propensity",
        "suffices, not neither. Cross-fit the nuisances, trim e, and",
        "report the largest weight -- a DR estimator with one weight",
        "of 500 is a one-observation estimator.")
}

# compact alias per ledger/NAMING.md
ipwforest <- morie_ipwgrf

# public names resolved by fn/_lazy_map.json
ipw_grf <- morie_ipwgrf
ipwgrf  <- morie_ipwgrf
