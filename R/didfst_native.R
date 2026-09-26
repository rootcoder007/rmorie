# Difference-in-differences with a forest: heterogeneous ATT.
# Sources: Wager, S. (2025) Causal Inference: A Statistical Learning
# Approach, Stanford University, draft of 26 November 2025, Chapter 13
# "Event-Study Designs" (Definitions 13.1-13.2, Assumption 13.1 on
# non-anticipation, eq. (13.5) for the post-event estimand, eq. (13.7)
# for the DiD estimator implemented here, eq. (13.8) for the SATT,
# Assumption 13.2 on parallel trends and Theorem 13.2 on
# unbiasedness); Callaway, B. & Sant'Anna, P. H. C. (2021)
# "Difference-in-Differences with multiple time periods", Journal of
# Econometrics 225(2), 200-230, doi:10.1016/j.jeconom.2020.12.001,
# arXiv:1803.09015 (the group-time ATT(g,t) decomposition and the
# choice between never-treated and not-yet-treated comparison
# groups); Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized
# random forests", Annals of Statistics 47(2), 1148-1178,
# doi:10.1214/18-AOS1709 (the honest-forest weights alpha_i(x) of
# eq. (3)).
#
# Native implementation mirroring Python morie.fn.didfst exactly: the
# same Delta_i, the same eq. (13.7) contrast under arbitrary unit
# weights, the same forest-weighted CATT, the same Callaway-Sant'Anna
# ATT(g,t), and the same pre-period placebo.

#' Validate a balanced panel
#'
#' @param Y Numeric matrix (n by T).
#' @return A list with the matrix and its dimensions.
#' @keywords internal
#' @noRd
.panel <- function(Y) {
  M <- as.matrix(Y)
  if (nrow(M) == 0L) stop("didfst: the panel is empty")
  T <- ncol(M)
  if (T < 2L)
    stop(sprintf("didfst: need at least 2 periods, got %d", T))
  if (!all(apply(M, 1, length) == T))
    stop("didfst: the panel must be balanced")
  list(M = M, n = nrow(M), T = T)
}

#' Post-minus-pre differences
#'
#' Periods \code{1..H} are pre-treatment for every unit and
#' \code{H+1..T} post, exactly as eq. (13.7) of Wager (2025) writes
#' it.
#'
#' @param Y Numeric matrix (n by T).
#' @param event_time Integer H in 1-based period numbers.
#' @return Numeric vector of length n.
#' @export
#' @examples
#' set.seed(1)
#' Y <- matrix(rnorm(40, 10), 8, 5)
#' morie_didfst_panel_differences(Y, event_time = 3)
#' @keywords internal
morie_didfst_panel_differences <- function(Y, event_time) {
  p <- .panel(Y)
  H <- as.integer(event_time)
  if (!(H >= 1L && H < p$T))
    stop(sprintf("didfst: event_time must satisfy 1 <= H < T = %d, got %d",
                 p$T, H))
  pre <- rowMeans(p$M[, seq_len(H), drop = FALSE])
  post <- rowMeans(p$M[, (H + 1L):p$T, drop = FALSE])
  post - pre
}

#' The DiD contrast of eq. (13.7) under arbitrary unit weights
#'
#' With \code{weights = NULL} every unit counts equally and this is
#' the textbook estimator. Passing forest weights turns it into the
#' local estimate at a point.
#'
#' @param delta Numeric vector of differences.
#' @param D Numeric vector of 0/1 adoption indicators.
#' @param weights Optional numeric vector of unit weights.
#' @return A list with \code{estimate}, \code{treated_mean},
#'   \code{control_mean}, \code{treated_weight}, \code{control_weight}.
#' @export
#' @examples
#' set.seed(1)
#' Y <- matrix(rnorm(40, 10), 8, 5)
#' D <- rep(c(1, 0), 4)
#' Y[D == 1, 4:5] <- Y[D == 1, 4:5] + 2
#' delta <- morie_didfst_panel_differences(Y, event_time = 3)
#' morie_didfst_did_estimate(delta, D)
#' @keywords internal
morie_didfst_did_estimate <- function(delta, D, weights = NULL) {
  d <- as.numeric(delta)
  Dv <- as.numeric(D)
  n <- length(d)
  if (length(Dv) != n)
    stop(sprintf("didfst: %d differences but %d adoption indicators",
                 n, length(Dv)))
  bad <- Dv[!(Dv == 0 | Dv == 1)]
  if (length(bad) > 0L)
    stop(sprintf("didfst: D must be 0/1, got %s", format(bad[1])))
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n)
    stop(sprintf("didfst: %d weights for %d units", length(w), n))
  if (any(w < 0)) stop("didfst: weights must be non-negative")
  st <- sum(w * Dv)
  sc <- sum(w * (1 - Dv))
  if (st <= 1e-12 || sc <= 1e-12)
    stop(sprintf("didfst: the comparison needs weight on both adopters and non-adopters (treated %.3g, control %.3g)",
                 st, sc))
  mt <- sum(w * Dv * d) / st
  mc <- sum(w * (1 - Dv) * d) / sc
  list(estimate = mt - mc, treated_mean = mt, control_mean = mc,
       treated_weight = st, control_weight = sc)
}

#' Heterogeneous ATT: DiD taken locally under forest weights
#'
#' @param Y Numeric panel (n by T).
#' @param D Numeric vector of 0/1 adoption indicators.
#' @param X Numeric covariate matrix.
#' @param event_time Integer H in 1-based period numbers.
#' @param x_eval Optional evaluation points.
#' @param n_trees Integer number of trees.
#' @param min_leaf Integer minimum leaf size.
#' @param alpha Numeric honesty fraction.
#' @param max_depth Integer, maximum tree depth.
#' @param seed Integer seed.
#' @param kind One of \code{"single-sample"} or \code{"double-sample"}.
#' @param clusters Optional cluster assignment vector.
#' @return A list with \code{tau}, \code{att_uniform} and the
#'   evaluation metadata.
#' @export
#' @examples
#' set.seed(2)
#' n <- 60
#' Y <- matrix(rnorm(n * 5, 10), n, 5)
#' D <- rbinom(n, 1, 0.5)
#' X <- matrix(rnorm(n * 2), n, 2)
#' Y[D == 1, 4:5] <- Y[D == 1, 4:5] + 2 + X[D == 1, 1]
#' r <- morie_didfst_did_forest(Y, D, X, event_time = 3, n_trees = 20L)
#' str(r, max.level = 1)
#' @keywords internal
morie_didfst_did_forest <- function(Y, D, X, event_time, x_eval = NULL,
                                    n_trees = 200L, min_leaf = 5L,
                                    alpha = 0.05, max_depth = 12L,
                                    seed = 0L,
                                    kind = "double-sample",
                                    clusters = NULL) {
  delta <- morie_didfst_panel_differences(Y, event_time)
  Xm <- as.matrix(X)
  n <- length(delta)
  if (nrow(Xm) != n)
    stop(sprintf("didfst: %d covariate rows for %d panel units",
                 nrow(Xm), n))
  Dv <- as.numeric(D)
  flat <- morie_didfst_did_estimate(delta, Dv)
  gf <- grow_forest(Xm, delta, W = Dv, kind = kind,
                     n_trees = n_trees, min_leaf = min_leaf,
                     alpha = alpha, max_depth = max_depth,
                     seed = as.integer(seed), clusters = clusters)
  trees <- gf$trees
  pts <- if (is.null(x_eval)) Xm else as.matrix(x_eval)
  taus <- numeric(nrow(pts))
  wt_t <- numeric(nrow(pts))
  wt_c <- numeric(nrow(pts))
  for (i in seq_len(nrow(pts))) {
    w <- forest_weights(trees, Xm, pts[i, ])
    r <- morie_didfst_did_estimate(delta, Dv, weights = w)
    taus[i] <- r$estimate
    wt_t[i] <- r$treated_weight
    wt_c[i] <- r$control_weight
  }
  list(estimate = mean(taus), tau = taus, delta = delta,
       att_uniform = flat$estimate,
       treated_weight = wt_t, control_weight = wt_c, n = n,
       n_trees = as.integer(n_trees),
       event_time = as.integer(event_time),
       design = "block-adoption",
       method = paste("difference-in-differences under honest forest",
                      "weights; Wager (2025) eq. (13.7) localised by",
                      "Athey-Tibshirani-Wager (2019) eq. (3)"))
}

#' Pre-period placebo
#'
#' Split the pre-period in two and run the whole estimator inside it,
#' where the true effect is zero by non-anticipation (Assumption 13.1).
#'
#' @param Y Numeric panel (n by T).
#' @param D Numeric vector of 0/1 adoption indicators.
#' @param event_time Integer H.
#' @param split Optional integer, period at which the pre-period is
#'   split.
#' @return A list with \code{estimate} and the change components.
#' @export
#' @examples
#' set.seed(3)
#' Y <- matrix(rnorm(48, 10), 8, 6)
#' D <- rep(c(1, 0), 4)
#' r <- morie_didfst_placebo_did(Y, D, event_time = 4)
#' str(r, max.level = 1)
#' @keywords internal
morie_didfst_placebo_did <- function(Y, D, event_time, split = NULL) {
  p <- .panel(Y)
  H <- as.integer(event_time)
  if (H < 2L)
    stop(sprintf("didfst: a pre-period placebo needs at least 2 pre-periods, event_time is %d",
                 H))
  cut <- if (is.null(split)) H %/% 2L else as.integer(split)
  if (!(cut >= 1L && cut < H))
    stop(sprintf("didfst: the placebo split must satisfy 1 <= split < %d, got %d",
                 H, cut))
  pre <- p$M[, seq_len(H), drop = FALSE]
  d <- morie_didfst_panel_differences(pre, cut)
  r <- morie_didfst_did_estimate(d, D)
  list(estimate = r$estimate, treated_change = r$treated_mean,
       control_change = r$control_mean, split = cut, n_pre = H,
       interpretation = "zero is consistent with parallel trends but does not establish it",
       method = "pre-period placebo DiD; Wager (2025) Assumption 13.1")
}

#' Callaway-Sant'Anna ATT(g,t) under staggered adoption
#'
#' @param Y Numeric panel (n by T).
#' @param first_treated Numeric or list; the 1-based period at which
#'   each unit first becomes treated, or \code{NULL} / \code{Inf} for
#'   never-treated.
#' @param comparison One of \code{"never-treated"} or
#'   \code{"not-yet-treated"}.
#' @return A list with \code{att}, \code{cohorts} and the cohort
#'   details.
#' @export
#' @examples
#' set.seed(3)
#' n <- 12
#' Y <- matrix(rnorm(n * 5, 10), n, 5)
#' ft <- as.list(c(rep(3L, 4), rep(4L, 4), rep(NA_real_, 4)))
#' ft[9:12] <- list(NULL)
#' r <- morie_didfst_group_time_att(Y, ft)
#' str(r, max.level = 1)
#' @keywords internal
morie_didfst_group_time_att <- function(Y, first_treated,
                                        comparison = "not-yet-treated") {
  p <- .panel(Y)
  if (!(comparison %in% c("never-treated", "not-yet-treated")))
    stop(sprintf("didfst: comparison must be one of never-treated, not-yet-treated, got %s",
                 comparison))
  n <- p$n
  T <- p$T
  if (length(first_treated) != n)
    stop(sprintf("didfst: %d adoption times for %d units",
                 length(first_treated), n))
  G <- vector("list", n)
  for (i in seq_len(n)) {
    v <- first_treated[[i]]
    if (is.null(v)) { G[[i]] <- NULL
    next }
    f <- as.numeric(v)
    if (is.na(f) || is.infinite(f)) { G[[i]] <- NULL
    next }
    g <- as.integer(f)
    if (!(g >= 2L && g <= T))
      stop(sprintf("didfst: adoption time %d is outside 2..T = %d (a unit treated in period 1 has no pre-period)",
                   g, T))
    G[[i]] <- g
  }
  cohorts <- sort(unique(unlist(G)))
  if (length(cohorts) == 0L)
    stop("didfst: no unit is ever treated")
  out <- list()
  for (g in cohorts) {
    idx_g <- which(vapply(G, function(v) !is.null(v) && v == g, logical(1)))
    for (t in g:T) {
      idx_c <- if (comparison == "never-treated") {
        which(vapply(G, is.null, logical(1)))
      } else {
        which(vapply(G, function(v) is.null(v) || v > t, logical(1)))
      }
      if (length(idx_c) == 0L) next
      a <- t - 1L
      b <- g - 2L
      dg <- mean(p$M[idx_g, a + 1L]) - mean(p$M[idx_g, b + 1L])
      dc <- mean(p$M[idx_c, a + 1L]) - mean(p$M[idx_c, b + 1L])
      out[[paste(g, t, sep = "_")]] <- list(att = dg - dc,
                                              n_treated = length(idx_g),
                                              n_control = length(idx_c))
    }
  }
  if (length(out) == 0L)
    stop("didfst: no (g, t) cell had a usable comparison group")
  est <- mean(vapply(out, function(v) v$att, numeric(1)))
  list(att = out, cohorts = cohorts, T = T, n = n,
       comparison = comparison, estimate = est,
       method = "group-time ATT(g,t), Callaway & Sant'Anna (2021)")
}

#' Aggregate ATT(g,t)
#'
#' @param gt A \code{group_time_att} result.
#' @param scheme One of \code{"simple"}, \code{"event"} or
#'   \code{"cohort"}.
#' @param horizon Optional integer, restrict the event-time profile.
#' @return A list with the estimate and the scheme details.
#' @export
#' @examples
#' set.seed(3)
#' n <- 12
#' Y <- matrix(rnorm(n * 5, 10), n, 5)
#' ft <- as.list(c(rep(3L, 4), rep(4L, 4), rep(NA_real_, 4)))
#' ft[9:12] <- list(NULL)
#' gt <- morie_didfst_group_time_att(Y, ft)
#' r <- morie_didfst_aggregate_att(gt, scheme = "simple")
#' str(r, max.level = 1)
#' @keywords internal
morie_didfst_aggregate_att <- function(gt, scheme = "simple",
                                       horizon = NULL) {
  if (!(scheme %in% c("simple", "event", "cohort")))
    stop(sprintf("didfst: scheme must be simple, event or cohort, got %s",
                 scheme))
  cells <- if (is.list(gt)) {
    if (!is.null(gt$att)) gt$att else gt
  } else stop("didfst: nothing to aggregate")
  if (length(cells) == 0L) stop("didfst: nothing to aggregate")
  if (scheme == "simple") {
    num <- sum(vapply(cells, function(v) v$att * v$n_treated, numeric(1)))
    den <- sum(vapply(cells, function(v) v$n_treated, numeric(1)))
    return(list(estimate = num / den, scheme = "simple",
                 n_cells = length(cells)))
  }
  keyed <- list()
  for (k in names(cells)) {
    v <- cells[[k]]
    parts <- strsplit(k, "_")[[1]]
    g <- as.integer(parts[1])
    t <- as.integer(parts[2])
    key <- if (scheme == "event") t - g else g
    if (scheme == "event" && !is.null(horizon) && key > horizon) next
    keyed[[as.character(key)]] <- c(keyed[[as.character(key)]], list(v))
  }
  if (length(keyed) == 0L)
    stop("didfst: the horizon excluded every cell")
  prof <- list()
  for (k in names(keyed)) {
    vs <- keyed[[k]]
    num <- sum(vapply(vs, function(v) v$att * v$n_treated, numeric(1)))
    den <- sum(vapply(vs, function(v) v$n_treated, numeric(1)))
    prof[[k]] <- num / den
  }
  list(profile = prof, scheme = scheme,
       estimate = mean(unlist(prof)))
}

# house entry point: the package exports one morie_<module>
morie_didfst <- morie_didfst_panel_differences

# -- restored: morie-only definition kept through the rmorie sync --
#' Aggregate ATT(g,t)
#'
#' @param gt Result of \code{group_time_att}.
#' @param scheme \code{"simple"}, \code{"event"} or
#'   \code{"cohort"}.
#' @param horizon Optional event-time cap for \code{"event"}.
#' @return A list with \code{estimate}, \code{scheme} (and
#'   \code{profile} for the by-event-time and by-cohort schemes).
#' @export
aggregate_att <- function(gt, scheme = "simple", horizon = NULL) {
  if (!(scheme %in% c("simple", "event", "cohort")))
    stop(sprintf("didfst: scheme must be simple, event or cohort, got %s",
                 scheme))
  cells <- gt$att
  if (length(cells) == 0L) stop("didfst: nothing to aggregate")
  if (scheme == "simple") {
    num <- sum(vapply(cells, function(v) v$att * v$n_treated, numeric(1)))
    den <- sum(vapply(cells, function(v) v$n_treated, numeric(1)))
    return(list(estimate = num / den, scheme = "simple",
                n_cells = length(cells)))
  }
  prof <- list()
  for (key in names(cells)) {
    v <- cells[[key]]
    k <- if (scheme == "event") v$t - v$g else v$g
    if (scheme == "event" && !is.null(horizon) && k > horizon) next
    if (is.null(prof[[as.character(k)]])) prof[[as.character(k)]] <- list(num = 0, den = 0)
    prof[[as.character(k)]]$num <- prof[[as.character(k)]]$num + v$att * v$n_treated
    prof[[as.character(k)]]$den <- prof[[as.character(k)]]$den + v$n_treated
  }
  if (length(prof) == 0L) stop("didfst: the horizon excluded every cell")
  flat <- vapply(prof, function(x) x$num / x$den, numeric(1))
  list(profile = flat, scheme = scheme, estimate = mean(flat))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' DiD contrast of eq. (13.7) under arbitrary unit weights
#'
#' With no weights the estimator is the textbook difference of group
#' means; passing forest weights turns it into a local estimate.
#'
#' @param delta Numeric vector of \code{Delta_i}.
#' @param D Numeric 0/1 vector.
#' @param weights Optional non-negative weight vector.
#' @return A list with \code{estimate}, \code{treated_change},
#'   \code{control_change}, \code{treated_weight},
#'   \code{control_weight}.
#' @export
did_estimate <- function(delta, D, weights = NULL) {
  d <- as.numeric(delta)
  Dv <- as.numeric(D)
  n <- length(d)
  if (length(Dv) != n)
    stop(sprintf("didfst: %d differences but %d adoption indicators",
                 n, length(Dv)))
  if (any(!(Dv %in% c(0, 1))))
    stop("didfst: D must be 0/1")
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  if (length(w) != n) stop(sprintf("didfst: %d weights for %d units", length(w), n))
  if (any(w < 0)) stop("didfst: weights must be non-negative")
  st <- sum(w * Dv)
  sc <- sum(w * (1 - Dv))
  if (st <= .ghc_DIDFST_EPS || sc <= .ghc_DIDFST_EPS)
    stop(sprintf("didfst: the comparison needs weight on both adopters and non-adopters (treated %.3g, control %.3g)",
                 st, sc))
  mt <- sum(w * Dv * d) / st
  mc <- sum(w * (1 - Dv) * d) / sc
  list(estimate = mt - mc, treated_change = mt, control_change = mc,
       treated_weight = st, control_weight = sc)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Heterogeneous ATT: DiD taken locally under forest weights
#'
#' @param Y Balanced n-by-T panel.
#' @param D 0/1 adoption vector.
#' @param X Covariate matrix.
#' @param event_time \code{H}.
#' @param x_eval Optional points at which to evaluate; default
#'   training rows.
#' @param n_trees Number of trees.
#' @param min_leaf Minimum leaf size.
#' @param alpha Honest subsample fraction.
#' @param max_depth Maximum tree depth.
#' @param seed Seed for the shared generator.
#' @param kind Forest subsampling scheme passed to \code{hntfst}.
#' @param clusters Optional cluster IDs.
#' @return A list mirroring the Python \code{RichResult} payload.
#' @export
#' @aliases didforest
did_forest <- function(Y, D, X, event_time, x_eval = NULL,
                       n_trees = 200L, min_leaf = 5L, alpha = 0.05,
                       max_depth = 12L, seed = 0L,
                       kind = "double-sample", clusters = NULL) {
  delta <- panel_differences(Y, event_time)
  Xm <- as.matrix(X)
  n <- length(delta)
  if (nrow(Xm) != n)
    stop(sprintf("didfst: %d covariate rows for %d panel units", nrow(Xm), n))
  Dv <- as.numeric(D)
  flat <- did_estimate(delta, Dv)
  grow <- grow_forest(Xm, delta, W = Dv, kind = kind,
                               n_trees = as.integer(n_trees),
                               min_leaf = as.integer(min_leaf),
                               alpha = alpha, max_depth = as.integer(max_depth),
                               seed = as.integer(seed),
                               clusters = clusters)
  trees <- grow$trees
  pts <- if (is.null(x_eval)) Xm else as.matrix(x_eval)
  taus <- numeric(nrow(pts))
  wt_t <- numeric(nrow(pts))
  wt_c <- numeric(nrow(pts))
  for (k in seq_len(nrow(pts))) {
    w <- forest_weights(trees, Xm, pts[k, ])
    e <- did_estimate(delta, Dv, weights = w)
    taus[k] <- e$estimate
    wt_t[k] <- e$treated_weight
    wt_c[k] <- e$control_weight
  }
  list(estimate = mean(taus), tau = taus, delta = delta,
       att_uniform = flat$estimate,
       treated_weight = wt_t, control_weight = wt_c,
       n = n, n_trees = as.integer(n_trees),
       event_time = as.integer(event_time),
       design = "block-adoption",
       method = paste("difference-in-differences under honest forest",
                      "weights; Wager (2025) eq. (13.7) localised by",
                      "Athey-Tibshirani-Wager (2019) eq. (3)"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .ghc_didfst_panel
#'
#' A step of the didfst_native implementation. Called by \code{group_time_att},
#' \code{panel_differences}, \code{placebo_did}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Y A matrix; passed to \code{as.matrix}.
#' @return A list with \code{M}, \code{n}, \code{T}.
#' @export
.ghc_didfst_panel <- function(Y) {
  M <- as.matrix(Y)
  if (nrow(M) == 0L) stop("didfst: the panel is empty")
  T <- ncol(M)
  if (T < 2L) stop(sprintf("didfst: need at least 2 periods, got %d", T))
  for (r in seq_len(nrow(M)))
    if (length(M[r, ]) != T)
      stop(sprintf("didfst: row %d has %d periods, expected %d -- the panel must be balanced",
                   r, length(M[r, ]), T))
  list(M = M, n = nrow(M), T = T)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Group-time ATT(g,t) under staggered adoption
#'
#' @param Y Balanced panel.
#' @param first_treated Numeric vector of adoption periods (1-based),
#'   with \code{NA} or \code{Inf} for never-treated.
#' @param comparison \code{"never-treated"} or
#'   \code{"not-yet-treated"}.
#' @return A list with \code{att} (named list keyed by \code{(g,t)}),
#'   \code{cohorts}, \code{T}, \code{n}, \code{comparison},
#'   \code{estimate}, \code{method}.
#' @export
group_time_att <- function(Y, first_treated, comparison = "not-yet-treated") {
  pp <- .ghc_didfst_panel(Y)
  if (!(comparison %in% .ghc_DIDFST_COMPARISON))
    stop(sprintf("didfst: comparison must be one of %s, got %s",
                 paste(.ghc_DIDFST_COMPARISON, collapse = ", "),
                 comparison))
  if (length(first_treated) != pp$n)
    stop(sprintf("didfst: %d adoption times for %d units",
                 length(first_treated), pp$n))
  G <- vector("list", pp$n)
  for (i in seq_along(first_treated)) {
    v <- first_treated[i]
    if (is.null(v) || is.na(v) || is.infinite(v)) {
      G[[i]] <- NA_integer_
      next
    }
    g <- as.integer(v)
    if (!(2L <= g && g <= pp$T))
      stop(sprintf("didfst: adoption time %d is outside 2..T = %d (a unit treated in period 1 has no pre-period)",
                   g, pp$T))
    G[[i]] <- g
  }
  cohorts <- sort(unique(unlist(G[!vapply(G, is.na, logical(1))])))
  if (length(cohorts) == 0L) stop("didfst: no unit is ever treated")
  out <- list()
  for (g in cohorts) {
    idx_g <- which(!vapply(G, is.na, logical(1)) & vapply(G, function(x) x == g, logical(1)))
    for (t in g:pp$T) {
      if (comparison == "never-treated")
        idx_c <- which(vapply(G, is.na, logical(1)))
      else
        idx_c <- which(vapply(G, function(x) is.na(x) || x > t, logical(1)))
      if (length(idx_c) == 0L) next
      a <- t
      b <- g - 1L
      dg <- mean(pp$M[idx_g, a]) - mean(pp$M[idx_g, b])
      dc <- mean(pp$M[idx_c, a]) - mean(pp$M[idx_c, b])
      key <- paste0(g, ",", t)
      out[[key]] <- list(g = g, t = t, att = dg - dc,
                         n_treated = length(idx_g),
                         n_control = length(idx_c))
    }
  }
  if (length(out) == 0L)
    stop("didfst: no (g, t) cell had a usable comparison group")
  est <- mean(vapply(out, function(v) v$att, numeric(1)))
  list(att = out, cohorts = cohorts, T = pp$T, n = pp$n,
       comparison = comparison, estimate = est,
       method = "group-time ATT(g,t), Callaway & Sant'Anna (2021)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Post-minus-pre panel differences
#'
#' The differenced outcome \code{Delta_i} of eq. (13.7) under the
#' block-adoption design.
#'
#' @param Y Balanced n-by-T panel.
#' @param event_time \code{H}, in 1-based period numbers.
#' @return Numeric vector of length n.
#' @export
panel_differences <- function(Y, event_time) {
  pp <- .ghc_didfst_panel(Y)
  H <- as.integer(event_time)
  if (!(1L <= H && H < pp$T))
    stop(sprintf("didfst: event_time must satisfy 1 <= H < T = %d, got %d",
                 pp$T, H))
  vapply(seq_len(pp$n), function(i) {
    row <- pp$M[i, ]
    pre <- sum(row[seq_len(H)]) / H
    post <- sum(row[(H + 1L):pp$T]) / (pp$T - H)
    post - pre
  }, numeric(1))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Pre-period placebo DiD
#'
#' Runs the estimator entirely inside the pre-period. Zero is
#' consistent with parallel trends but does not establish it.
#'
#' @param Y Balanced panel.
#' @param D 0/1 vector.
#' @param event_time \code{H}.
#' @param split Pre-period split point.
#' @return A list mirroring the Python \code{RichResult} payload.
#' @export
placebo_did <- function(Y, D, event_time, split = NULL) {
  pp <- .ghc_didfst_panel(Y)
  H <- as.integer(event_time)
  if (H < 2L)
    stop(sprintf("didfst: a pre-period placebo needs at least 2 pre-periods, event_time is %d", H))
  cut <- if (is.null(split)) H %/% 2L else as.integer(split)
  if (!(1L <= cut && cut < H))
    stop(sprintf("didfst: the placebo split must satisfy 1 <= split < %d, got %d",
                 H, cut))
  pre <- pp$M[, seq_len(H), drop = FALSE]
  d <- panel_differences(pre, cut)
  e <- did_estimate(d, as.numeric(D))
  list(estimate = e$estimate, treated_change = e$treated_change,
       control_change = e$control_change, split = cut, n_pre = H,
       interpretation = paste("zero is consistent with parallel",
                              "trends but does not establish it"),
       method = "pre-period placebo DiD; Wager (2025) Assumption 13.1")
}

# -- restored: morie-only objects kept through the rmorie sync --
.ghc_DIDFST_COMPARISON <- c("never-treated", "not-yet-treated")

.ghc_DIDFST_EPS <- 1e-12

didforest <- did_forest
