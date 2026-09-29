# Coverage for difference-in-differences with a forest (Wager 2025 ch. 13;
# Callaway & Sant'Anna 2021). Panel differences and the weighted contrast
# are recomputed from eq. (13.7), group-time ATTs against did::att_gt, the
# aggregations from their weighted means, and the forest CATT on a panel
# whose effect is homogeneous, where every weighting must return it.

.panel_dat <- function() {
  set.seed(5)
  n <- 40
  T <- 6
  fe <- stats::rnorm(n)
  G <- rep(c(3, 4, 5, Inf), each = 10)
  Y <- matrix(0, n, T)
  for (i in 1:n) for (t in 1:T) {
    Y[i, t] <- fe[i] + 0.5 * t + (if (t >= G[i]) 1 + 0.3 * (t - G[i]) else 0) + 0.2 * stats::rnorm(1)
  }
  list(Y = Y, G = G, n = n, T = T)
}

test_that("panel differences and the eq. (13.7) contrast", {
  Y <- matrix(c(1, 2, 4, 3, 2, 5, 6, 1, 0, 2, 2, 3), 3, byrow = TRUE)
  d <- morie_didfst_panel_differences(Y, 2)
  expect_equal(d, rowMeans(Y[, 3:4]) - rowMeans(Y[, 1:2]), tolerance = 1e-12)
  expect_equal(panel_differences(Y, 2), d, tolerance = 1e-12)
  expect_identical(morie_didfst, morie_didfst_panel_differences)
  expect_error(morie_didfst_panel_differences(Y, 4), "1 <= H < T")
  D <- c(1, 0, 0)
  w <- c(2, 1, 3)
  r <- morie_didfst_did_estimate(d, D, weights = w)
  expect_equal(r$estimate, d[1] - (1 * d[2] + 3 * d[3]) / 4, tolerance = 1e-12)
  expect_equal(c(r$treated_weight, r$control_weight), c(2, 4))
  expect_equal(did_estimate(d, D, w)$estimate, r$estimate, tolerance = 1e-12)
  expect_error(morie_didfst_did_estimate(d, c(1, 2, 0)), "0/1")
  expect_error(morie_didfst_did_estimate(d, c(1, 1, 1)), "both adopters and non-adopters")
  expect_error(did_estimate(d, c(1, 0, 0), c(-1, 1, 1)), "non-negative")
})

test_that("the pre-period placebo contrasts pre-period halves", {
  p <- .panel_dat()
  D <- as.numeric(is.finite(p$G) & p$G == 5)
  pl <- morie_didfst_placebo_did(p$Y, D, event_time = 4)
  dd <- rowMeans(p$Y[, 3:4]) - rowMeans(p$Y[, 1:2])
  expect_equal(pl$estimate, mean(dd[D == 1]) - mean(dd[D == 0]), tolerance = 1e-12)
  expect_identical(pl$split, 2L)
  expect_equal(placebo_did(p$Y, D, 4, split = 1)$estimate, {
    d1 <- rowMeans(p$Y[, 2:4, drop = FALSE]) - p$Y[, 1]
    mean(d1[D == 1]) - mean(d1[D == 0])
  }, tolerance = 1e-12)
  expect_error(morie_didfst_placebo_did(p$Y, D, 1), "at least 2 pre-periods")
  expect_error(morie_didfst_placebo_did(p$Y, D, 4, split = 4), "1 <= split < 4")
})

test_that("group-time ATTs match did::att_gt for every post-treatment cell", {
  skip_if_not_installed("did")
  p <- .panel_dat()
  long <- data.frame(id = rep(1:p$n, each = p$T), period = rep(1:p$T, p$n), y = as.numeric(t(p$Y)),
                     g = rep(ifelse(is.finite(p$G), p$G, 0), each = p$T))
  for (cmp in c("never-treated", "not-yet-treated")) {
    gt <- morie_didfst_group_time_att(p$Y, as.list(p$G), comparison = cmp)
    ref <- suppressWarnings(did::att_gt(yname = "y", tname = "period", idname = "id", gname = "g", data = long,
                                        control_group = if (cmp == "never-treated") "nevertreated" else "notyettreated",
                                        bstrap = FALSE, cband = FALSE, est_method = "reg"))
    keep <- ref$t >= ref$group
    for (k in which(keep)) {
      key <- paste(ref$group[k], ref$t[k], sep = "_")
      expect_equal(gt$att[[key]]$att, ref$att[k], tolerance = 1e-9, info = paste(cmp, key))
    }
    expect_length(gt$att, sum(keep))
    rs <- group_time_att(p$Y, p$G, comparison = cmp)
    expect_equal(unname(vapply(rs$att, function(v) v$att, 1)), unname(vapply(gt$att, function(v) v$att, 1)), tolerance = 1e-12)
  }
  expect_error(morie_didfst_group_time_att(p$Y, as.list(rep(Inf, 40))), "no unit is ever treated")
  expect_error(morie_didfst_group_time_att(p$Y, as.list(c(1, p$G[-1]))), "outside 2..T")
  expect_error(morie_didfst_group_time_att(p$Y, as.list(p$G), comparison = "all"), "comparison must be one of")
})

test_that("aggregations are n-weighted means of the (g, t) cells", {
  p <- .panel_dat()
  gt <- morie_didfst_group_time_att(p$Y, as.list(p$G))
  att <- vapply(gt$att, function(v) v$att, 1)
  nt <- vapply(gt$att, function(v) v$n_treated, 1)
  gg <- as.integer(sub("_.*", "", names(att)))
  tt <- as.integer(sub(".*_", "", names(att)))
  expect_equal(morie_didfst_aggregate_att(gt)$estimate, sum(att * nt) / sum(nt), tolerance = 1e-12)
  ev <- morie_didfst_aggregate_att(gt, "event")
  e <- tt - gg
  prof <- vapply(sort(unique(e)), function(k) sum((att * nt)[e == k]) / sum(nt[e == k]), 1)
  expect_equal(unname(unlist(ev$profile))[order(as.integer(names(ev$profile)))], prof, tolerance = 1e-12)
  expect_equal(ev$estimate, mean(prof), tolerance = 1e-12)
  ch <- morie_didfst_aggregate_att(gt, "cohort")
  cp <- vapply(c(3, 4, 5), function(k) sum((att * nt)[gg == k]) / sum(nt[gg == k]), 1)
  expect_equal(unname(unlist(ch$profile)), cp, tolerance = 1e-12)
  eh <- morie_didfst_aggregate_att(gt, "event", horizon = 1)
  expect_setequal(names(eh$profile), c("0", "1"))
  rgt <- group_time_att(p$Y, p$G)
  expect_equal(aggregate_att(rgt, "event")$estimate, ev$estimate, tolerance = 1e-12)
  expect_equal(aggregate_att(rgt)$estimate, morie_didfst_aggregate_att(gt)$estimate, tolerance = 1e-12)
  expect_error(morie_didfst_aggregate_att(gt, "calendar"), "scheme must be")
  expect_error(morie_didfst_aggregate_att(gt, "event", horizon = -1), "excluded every cell")
})

test_that("the forest CATT recovers a homogeneous effect under any weighting", {
  set.seed(9)
  n <- 60
  X <- cbind(stats::runif(n), stats::runif(n))
  D <- as.numeric(stats::runif(n) < 0.5)
  fe <- stats::rnorm(n)
  Y <- outer(fe, rep(1, 4)) + outer(rep(1, n), 0.4 * (1:4))
  Y[, 3:4] <- Y[, 3:4] + 1.7 * D
  f <- morie_didfst_did_forest(Y, D, X, event_time = 2, n_trees = 20, min_leaf = 12, seed = 1)
  expect_equal(f$tau, rep(1.7, n), tolerance = 1e-10)
  expect_equal(f$att_uniform, 1.7, tolerance = 1e-12)
  expect_equal(f$delta, rowMeans(Y[, 3:4]) - rowMeans(Y[, 1:2]), tolerance = 1e-12)
  fe2 <- did_forest(Y, D, X, 2, x_eval = X[1:3, ], n_trees = 20, min_leaf = 12, seed = 1)
  expect_equal(fe2$tau, rep(1.7, 3), tolerance = 1e-10)
  expect_error(morie_didfst_did_forest(Y, D, X[-1, ], 2), "covariate rows")
})
