# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of qsfrgr -- quantile survival forest. Mirrors
# src/morie/fn/qsfrgr.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R and the matched random stream in
# R/aaa_helpers_ghc_rng.R.
#
# A random forest for survival does not average predictions. It averages
# NEIGHBOURHOODS. Each tree puts the query point in a leaf, and the
# training observations sharing that leaf are the ones the tree
# considers comparable to it; averaging that membership over the forest
# gives a weight for every training observation,
#
#     alpha_i(x) = (1/B) sum_b 1{i in leaf_b(x)} / |leaf_b(x)|
#
# and those weights, which sum to one, define a conditional
# distribution. Feed them to a Kaplan-Meier estimator and you get a
# conditional survival curve; invert the curve and you get a conditional
# quantile -- the median survival time for a patient like this one. That
# is the whole method, and it is why a forest can estimate a quantile at
# all: a quantile is not an average of anything, so a forest that
# averaged predictions could not produce one.
#
# Censoring is handled where it belongs, inside the Kaplan-Meier product
# rather than by dropping the censored rows. An observation censored at
# time t contributes to every risk set up to t and to no death. Dropping
# it instead would bias the curve upward, badly, and is the single most
# common way this goes wrong.
#
# Splitting uses the LOG-RANK statistic between the two children, the
# standard survival criterion: it asks whether the two groups' hazards
# differ rather than whether their mean times do, and is therefore
# undisturbed by censoring in a way a variance-reduction split is not.
#
# Honesty is a route, not a default assumption. An honest tree uses one
# half of the sample to choose the splits and the OTHER half to populate
# the leaves, so the values in a leaf were not used to decide that the
# leaf should exist.
#
# References
#   Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
#     "Estimating heterogeneous treatment effects with right-censored
#     data via causal survival forests." Journal of the Royal
#     Statistical Society Series B 85(2), 179-211.
#     doi:10.1093/jrsssb/qkac001.
#   Wager, S. and Athey, S. (2018) "Estimation and inference of
#     heterogeneous treatment effects using random forests." Journal of
#     the American Statistical Association 113(523), 1228-1242.
#   Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
#     forests." The Annals of Statistics 47(2), 1148-1178.
#   Ishwaran, H., Kogalur, U.B., Blackstone, E.H. and Lauer, M.S. (2008)
#     "Random survival forests." The Annals of Applied Statistics 2(3),
#     841-860.
#   Kaplan, E.L. and Meier, P. (1958) "Nonparametric estimation from
#     incomplete observations." Journal of the American Statistical
#     Association 53(282), 457-481.
#   Meinshausen, N. (2006) "Quantile regression forests." Journal of
#     Machine Learning Research 7, 983-999.

.QSFRGR_SPLITS <- c("logrank", "events")

#' The log-rank statistic between two groups of row indices
#'
#' The squared standardised difference between observed and expected
#' deaths in the left group, which is the quantity a survival tree
#' maximises. Zero when the groups are indistinguishable and when
#' neither can contribute a comparison.
#'
#' @param time Observed times.
#' @param event Event indicators.
#' @param left Row indices of the left group.
#' @param right Row indices of the right group.
#' @return The statistic.
#' @export
morie_qsfrgr_logrank <- function(time, event, left, right) {
  if (!length(left) || !length(right)) return(0)
  rows <- sort(c(left, right))
  times <- sort(unique(time[rows[event[rows] == 1L]]))
  ome <- numeric(0); vv <- numeric(0)
  for (t in times) {
    n1 <- 0L; n2 <- 0L; d1 <- 0L; d2 <- 0L
    for (i in left) if (time[i] >= t) {
      n1 <- n1 + 1L
      if (event[i] == 1L && time[i] == t) d1 <- d1 + 1L
    }
    for (i in right) if (time[i] >= t) {
      n2 <- n2 + 1L
      if (event[i] == 1L && time[i] == t) d2 <- d2 + 1L
    }
    n <- n1 + n2; d <- d1 + d2
    if (n < 2L || d == 0L) next
    e1 <- d * n1 / n
    ome <- c(ome, d1 - e1)
    vv <- c(vv, d * (n1 / n) * (n2 / n) * (n - d) / (n - 1))
  }
  if (!length(vv)) return(0)
  v <- .w3_csum(vv)
  if (v <= 0) return(0)
  s <- .w3_csum(ome)
  s * s / v
}

#' .qsfrgr_events_in
#'
#' A step of the qsfrgr_native implementation. Called by \code{.qsfrgr_best_split}, \code{.qsfrgr_grow}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param event A vector; indexed elementwise.
#' @param rows See Usage.
#' @return A numeric value.
#' @export
.qsfrgr_events_in <- function(event, rows) sum(event[rows] == 1L)

#' .qsfrgr_best_split
#'
#' A step of the qsfrgr_native implementation. Called by \code{.qsfrgr_grow}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param time Passed to \code{morie_qsfrgr_logrank}.
#' @param event Passed to \code{morie_qsfrgr_logrank}.
#' @param rows A vector; indexed elementwise.
#' @param feats See Usage.
#' @param min_leaf See Usage.
#' @param rule Compared against \code{"logrank"}.
#' @return The value of \code{best}, as built in the body.
#' @export
.qsfrgr_best_split <- function(X, time, event, rows, feats, min_leaf,
                               rule) {
  best <- NULL
  for (f in feats) {
    vals <- sort(unique(X[rows, f]))
    if (length(vals) < 2L) next
    for (k in seq_len(length(vals) - 1L)) {
      thr <- 0.5 * (vals[k] + vals[k + 1L])
      left <- rows[X[rows, f] <= thr]
      right <- rows[X[rows, f] > thr]
      if (length(left) < min_leaf || length(right) < min_leaf) next
      score <- if (rule == "logrank")
        morie_qsfrgr_logrank(time, event, left, right)
      else {
        # A split that isolates events is better than one that isolates
        # censoring; this is the cheap rule and it is here to be visibly
        # worse than the log-rank one.
        a <- .qsfrgr_events_in(event, left)
        b <- .qsfrgr_events_in(event, right)
        abs(a / length(left) - b / length(right))
      }
      if (is.null(best) || score > best$score ||
          (score == best$score && (f < best$f ||
                                   (f == best$f && thr < best$thr))))
        best <- list(f = f, thr = thr, score = score)
    }
  }
  best
}

#' .qsfrgr_grow
#'
#' A step of the qsfrgr_native implementation. Called by \code{morie_qsfrgr_forest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param time Passed to \code{.qsfrgr_best_split}.
#' @param event Passed to \code{.qsfrgr_events_in}.
#' @param struct_rows A vector; its length is taken and its elements indexed.
#' @param leaf_rows A vector; indexed elementwise.
#' @param feats_n Numeric; passed to \code{min}.
#' @param min_leaf Numeric; combined arithmetically in the body.
#' @param max_depth Passed to \code{.qsfrgr_grow}.
#' @param depth Numeric; combined arithmetically in the body.
#' @param e Passed to \code{.ghc_unif}.
#' @param rule Passed to \code{.qsfrgr_best_split}.
#' @return A list with \code{leaf}, \code{f}, \code{thr}, \code{l}, \code{r}.
#' @export
.qsfrgr_grow <- function(X, time, event, struct_rows, leaf_rows, feats_n,
                         min_leaf, max_depth, depth, e, rule) {
  node <- list(leaf = TRUE, rows = leaf_rows)
  if (depth >= max_depth || length(struct_rows) < 2L * min_leaf ||
      .qsfrgr_events_in(event, struct_rows) < 2L)
    return(node)
  p <- ncol(X)
  feats <- integer(0)
  pool <- seq_len(p)
  for (q in seq_len(min(feats_n, p))) {
    # floor(u * len) clamped, not an integer draw: it is the idiom the
    # rest of the forest code in this package already uses, and two
    # different ways of turning a uniform into an index consume the
    # stream identically but land on different features.
    j <- floor(.ghc_unif(e, 1L) * length(pool))
    if (j >= length(pool)) j <- length(pool) - 1
    feats <- c(feats, pool[j + 1L])
    pool <- pool[-(j + 1L)]
  }
  feats <- sort(feats)
  sp <- .qsfrgr_best_split(X, time, event, struct_rows, feats, min_leaf,
                           rule)
  if (is.null(sp) || sp$score <= 0) return(node)
  sl <- struct_rows[X[struct_rows, sp$f] <= sp$thr]
  sr <- struct_rows[X[struct_rows, sp$f] > sp$thr]
  ll <- leaf_rows[X[leaf_rows, sp$f] <= sp$thr]
  lr <- leaf_rows[X[leaf_rows, sp$f] > sp$thr]
  if (!length(ll) || !length(lr)) return(node)
  list(leaf = FALSE, f = sp$f, thr = sp$thr,
       l = .qsfrgr_grow(X, time, event, sl, ll, feats_n, min_leaf,
                        max_depth, depth + 1L, e, rule),
       r = .qsfrgr_grow(X, time, event, sr, lr, feats_n, min_leaf,
                        max_depth, depth + 1L, e, rule))
}

#' .qsfrgr_leaf_of
#'
#' A step of the qsfrgr_native implementation. Called by \code{morie_qsfrgr_weights}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$f}, \code{$l}, \code{$leaf}, \code{$r}, \code{$rows}, \code{$thr} from it.
#' @param x A vector; indexed elementwise.
#' @return The value of \code{$}.
#' @export
.qsfrgr_leaf_of <- function(node, x) {
  while (!node$leaf)
    node <- if (x[node$f] <= node$thr) node$l else node$r
  node$rows
}

#' Grow a survival forest and return its trees
#'
#' Each tree draws a subsample, splits it in half when honest, grows on
#' the first half and fills the leaves from the second.
#'
#' @param X Covariate matrix.
#' @param time Observed times.
#' @param event Event indicators.
#' @param n_trees Number of trees.
#' @param mtry Features tried per split, or NULL for all.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Maximum depth.
#' @param honest Whether to split the subsample.
#' @param seed The random stream.
#' @param rule A member of the split list.
#' @return A list of trees.
#' @export
morie_qsfrgr_forest <- function(X, time, event, n_trees = 20L,
                                mtry = NULL, min_leaf = 3L,
                                max_depth = 6L, honest = TRUE, seed = 0,
                                rule = "logrank") {
  if (!(rule %in% .QSFRGR_SPLITS))
    stop("rule must be one of ", paste(.QSFRGR_SPLITS, collapse = ", "))
  n <- length(time)
  p <- ncol(X)
  m <- if (is.null(mtry)) p else as.integer(mtry)
  e <- .ghc_rng(seed)
  trees <- list()
  for (b in seq_len(as.integer(n_trees))) {
    idx <- seq_len(n)
    if (n > 1L) for (i in seq(n, 2L)) {
      j <- floor(.ghc_unif(e, 1L) * i)
      if (j > i - 1L) j <- i - 1L
      tmp <- idx[i]; idx[i] <- idx[j + 1L]; idx[j + 1L] <- tmp
    }
    take <- idx[seq_len(max(2L * min_leaf, n %/% 2L))]
    if (honest) {
      h <- length(take) %/% 2L
      struct <- sort(take[seq_len(h)])
      leaf <- sort(take[(h + 1L):length(take)])
    } else {
      struct <- sort(take); leaf <- sort(take)
    }
    if (!length(struct) || !length(leaf)) next
    trees[[length(trees) + 1L]] <-
      .qsfrgr_grow(X, time, event, struct, leaf, m, min_leaf,
                   as.integer(max_depth), 0L, e, rule)
  }
  trees
}

#' The forest's weight on each training observation, summing to one
#'
#' Each tree contributes one over its leaf size to every observation in
#' that leaf; the forest averages over trees. A tree whose leaf is empty
#' contributes nothing rather than dividing by zero.
#'
#' @param trees A forest.
#' @param x A covariate row.
#' @param n The training sample size.
#' @return A list with the weights and the number of trees used.
#' @export
morie_qsfrgr_weights <- function(trees, x, n) {
  w <- numeric(n)
  used <- 0L
  for (t in trees) {
    rows <- .qsfrgr_leaf_of(t, x)
    if (!length(rows)) next
    used <- used + 1L
    cc <- 1 / length(rows)
    for (i in rows) w[i] <- w[i] + cc
  }
  if (used == 0L) return(list(w = w, used = 0L))
  list(w = w / used, used = used)
}

#' Weighted Kaplan-Meier survival curve
#'
#' The product over event times of one minus the weighted deaths over
#' the weighted risk set. A censored observation stays in the risk set
#' until its censoring time and never enters a numerator, which is the
#' entire content of handling censoring.
#'
#' @param time Observed times.
#' @param event Event indicators.
#' @param weights Observation weights.
#' @param grid Times at which to report the curve, or NULL.
#' @return A list with the step times, the survival after each step, and
#'   the curve on the grid when one was given.
#' @export
morie_qsfrgr_km <- function(time, event, weights, grid = NULL) {
  n <- length(time)
  ts <- sort(unique(time[event == 1L & weights > 0]))
  s <- 1
  ct <- numeric(0); cs <- numeric(0)
  for (t in ts) {
    d <- .w3_csum(weights[event == 1L & time == t])
    r <- .w3_csum(weights[time >= t])
    if (r <= 0) next
    s <- s * (1 - d / r)
    ct <- c(ct, t); cs <- c(cs, s)
  }
  out <- list(t = ct, s = cs)
  if (is.null(grid)) return(out)
  g <- numeric(length(grid))
  for (i in seq_along(grid)) {
    v <- 1
    for (k in seq_along(ct)) if (ct[k] <= grid[i]) v <- cs[k]
    g[i] <- v
  }
  out$grid <- g
  out
}

#' The smallest time whose survival has fallen to or below 1 - q
#'
#' Returns NA when the curve never gets there, which is the honest
#' answer for a median that is not reached rather than the largest
#' observed time dressed up as an estimate.
#'
#' @param curve A curve from the Kaplan-Meier function.
#' @param q The quantile.
#' @return The quantile time, or NA.
#' @export
morie_qsfrgr_quantile <- function(curve, q) {
  q <- as.numeric(q)
  if (!(q > 0 && q < 1))
    stop("the quantile must lie strictly inside (0, 1)")
  target <- 1 - q
  for (k in seq_along(curve$t)) if (curve$s[k] <= target) return(curve$t[k])
  NA_real_
}

#' Conditional survival quantiles from a forest
#'
#' @param time Observed time.
#' @param event Event indicator, one is a death and zero a censoring.
#' @param X Covariates, one row per observation.
#' @param quantile Which conditional quantile of the survival
#'   distribution.
#' @param n_trees Number of trees.
#' @param mtry Features tried per split, or NULL.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Maximum depth.
#' @param honest Whether to split the subsample.
#' @param seed The random stream.
#' @param rule A member of the split list.
#' @param newX Points to predict at, or NULL for the training rows.
#' @param grid Times at which the conditional survival curve is
#'   reported, or NULL.
#' @return A list with the conditional quantile at each query point, the
#'   survival curve on the grid, the weights' effective sample size, and
#'   how many observations were censored.
#' @export
morie_qsfrgr <- function(time, event, X, quantile = 0.5, n_trees = 20L,
                         mtry = NULL, min_leaf = 3L, max_depth = 6L,
                         honest = TRUE, seed = 0, rule = "logrank",
                         newX = NULL, grid = NULL) {
  t <- as.numeric(time)
  e <- as.integer(ifelse(as.numeric(event) != 0, 1L, 0L))
  xs <- as.matrix(X); storage.mode(xs) <- "double"
  n <- length(t)
  if (n < 4L) stop("need at least four observations")
  if (length(e) != n || nrow(xs) != n)
    stop("time, event and X must agree in length")
  trees <- morie_qsfrgr_forest(xs, t, e, n_trees, mtry, min_leaf,
                               max_depth, honest, seed, rule)
  if (!length(trees))
    stop("no tree could be grown; the sample is too small for the leaf size")
  qx <- if (is.null(newX)) xs else {
    m <- as.matrix(newX); storage.mode(m) <- "double"; m
  }
  if (is.null(grid)) grid <- sort(unique(t))
  grid <- as.numeric(grid)

  quants <- numeric(nrow(qx))
  curves <- list()
  ess <- numeric(nrow(qx))
  for (i in seq_len(nrow(qx))) {
    ww <- morie_qsfrgr_weights(trees, qx[i, ], n)
    km <- morie_qsfrgr_km(t, e, ww$w, grid)
    quants[i] <- morie_qsfrgr_quantile(km, quantile)
    curves[[i]] <- km$grid
    # Effective sample size of the weights: one over the sum of their
    # squares. It says how many observations the estimate is really
    # resting on, which a weight vector of length n hides.
    ss <- .w3_csum(ww$w * ww$w)
    ess[i] <- if (ss > 0) 1 / ss else 0
  }

  got <- quants[!is.na(quants)]
  list(quantile_estimate = ifelse(is.na(quants), NaN, quants),
       n_unreached = sum(is.na(quants)), curve = curves, grid = grid,
       ess = ess,
       mean_ess = if (length(ess)) .w3_csum(ess) / length(ess) else NaN,
       estimate = if (length(got)) .w3_csum(got) / length(got) else NaN,
       se = NaN, n_trees = length(trees), n = n, n_events = sum(e),
       n_censored = n - sum(e), n_query = nrow(qx),
       quantile = as.numeric(quantile), honest = isTRUE(honest),
       rule = rule, method = "quantile survival forest")
}

#' One-line summary of the qsfrgr module
#'
#' @return A character scalar.
#' @export
morie_qsfrgr_cheatsheet <- function()
  paste0("qsfrgr: quantile survival forest. splits ",
         paste(.QSFRGR_SPLITS, collapse = ", "),
         "; forest weights into a weighted Kaplan-Meier, inverted")
