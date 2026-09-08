# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the staggered-DiD estimators, against
# morie.fn.{didtwfe,gbacon,cssant,boryis}.
#
# The fixture is deterministic and its truth is known from the design
# rather than from any estimator: three cohorts of three units,
# adopting in periods 3 and 5 of 8 (and never), with an effect that
# GROWS with exposure -- 1 at adoption, rising by 0.5 each period.
# Averaged over the ten treated cohort-period cells the true ATT is 2.05.
#
# That design is the whole point of the shelf: two-way fixed effects
# returns 1.55 on it, because a fifth of its weight sits on a
# comparison that uses the already-treated early cohort as a control.

did_fixture <- function() {
  u <- rep(1:9, each = 8L)
  tt <- rep(1:8, times = 9L)
  g <- rep(c(3, 3, 3, 5, 5, 5, Inf, Inf, Inf), each = 8L)
  rel <- ifelse(is.finite(g), pmax(0, tt - g), 0)
  y <- u * 0.3 + tt * 0.2 + ifelse(tt >= g, 1 + 0.5 * rel, 0)
  data.frame(y = y, d = as.numeric(tt >= g), unit = u, time = tt, gt = g)
}

test_that("the fixture's true ATT is what the design says it is", {
  d <- did_fixture()
  rel <- with(d, ifelse(is.finite(gt), time - gt, NA_real_))
  eff <- 1 + 0.5 * rel[d$d == 1]
  expect_equal(mean(eff), 2.05, tolerance = 1e-12)
  # 10 treated cohort-period cells, 3 units in each
  expect_equal(length(eff), 30L)
})

test_that("TWFE misses the truth and the decomposition says why", {
  d <- did_fixture()
  fe <- morie_did_panel_fe(d, "y", "d", "unit", "time")
  expect_equal(as.numeric(fe$estimate), 1.55, tolerance = 1e-10)
  bac <- morie_did_bacon_decomposition(d, "y", "d", "unit", "time")
  # matches morie.fn.gbacon component for component
  expect_equal(bac$overall_estimate, 1.55, tolerance = 1e-10)
  expect_equal(sum(bac$components$weight), 1, tolerance = 1e-10)
  cmp <- bac$components
  key <- paste(cmp$treated, cmp$untreated)
  expect_equal(cmp$weight[key == "3 Inf"], 0.3, tolerance = 1e-10)
  expect_equal(cmp$weight[key == "5 Inf"], 0.4, tolerance = 1e-10)
  expect_equal(cmp$weight[key == "3 5"], 0.1, tolerance = 1e-10)
  expect_equal(cmp$weight[key == "5 3"], 0.2, tolerance = 1e-10)
  expect_equal(cmp$estimate[key == "3 Inf"], 2.25, tolerance = 1e-10)
  expect_equal(cmp$estimate[key == "5 Inf"], 1.75, tolerance = 1e-10)
  expect_equal(cmp$estimate[key == "3 5"], 1.25, tolerance = 1e-10)
  # the forbidden comparison: the late cohort against the already-treated
  # early one. It differences out the early cohort's own growing effect,
  # which is how a coefficient lands below every clean comparison.
  expect_equal(cmp$estimate[key == "5 3"], 0.25, tolerance = 1e-10)
  expect_lt(cmp$estimate[key == "5 3"], min(cmp$estimate[key != "5 3"]))
})

test_that("group-time ATTs match morie.fn cell by cell", {
  d <- did_fixture()
  gt <- morie_did_group_time_att(d, "y", "unit", "time", "gt",
                                 n_bootstrap = 0L)
  post <- gt[gt$post, ]
  expect_equal(nrow(gt), 14L)
  expect_equal(nrow(post), 10L)
  for (i in seq_len(nrow(post))) {
    expect_equal(post$att[i], 1 + 0.5 * (post$time[i] - post$cohort[i]),
                 tolerance = 1e-9)
  }
  # pre-treatment cells are the parallel-trends check and are zero here
  expect_lt(max(abs(gt$att[!gt$post])), 1e-9)
})

test_that("the aggregation uses post cells only and weights by cohort", {
  d <- did_fixture()
  gt <- morie_did_group_time_att(d, "y", "unit", "time", "gt",
                                 n_bootstrap = 0L)
  ov <- morie_did_aggregate_gt_att(gt, aggregation = "overall")
  expect_equal(ov$estimate, 2.05, tolerance = 1e-9)
  # averaging the pre-treatment cells in would give 20.5/14 = 1.4643,
  # which is not an average treatment effect of anything
  expect_false(isTRUE(all.equal(ov$estimate, mean(gt$att))))
  coh <- morie_did_aggregate_gt_att(gt, aggregation = "cohort")
  expect_equal(coh$estimate[coh$group == 3], 2.25, tolerance = 1e-9)
  expect_equal(coh$estimate[coh$group == 5], 1.75, tolerance = 1e-9)
  ev <- morie_did_aggregate_gt_att(gt, aggregation = "event_time")
  # the event study keeps the pre-periods, since showing them is its job
  expect_true(any(ev$group < 0))
  for (i in which(ev$group >= 0)) {
    expect_equal(ev$estimate[i], 1 + 0.5 * ev$group[i], tolerance = 1e-9)
  }
})

test_that("a cohort-size-weighted aggregate is not an unweighted one", {
  # one big early cohort, one small late cohort: the two disagree, so
  # the weighting choice is visible in the answer
  gt <- data.frame(cohort = c(3, 3, 5, 5), time = c(3, 4, 5, 6),
                   att = c(1, 2, 10, 20), std_error = rep(0.1, 4),
                   n_treated = c(90, 90, 10, 10), post = rep(TRUE, 4))
  ov <- morie_did_aggregate_gt_att(gt, aggregation = "overall")
  expect_equal(ov$estimate, 0.45 * 1 + 0.45 * 2 + 0.05 * 10 + 0.05 * 20,
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(ov$estimate, mean(gt$att))))
})

test_that("the imputation estimator matches morie.fn exactly", {
  d <- did_fixture()
  imp <- morie_did_borusyak(d, "y", "unit", "time", "gt", n_bootstrap = 0L)
  expect_equal(as.numeric(imp$estimate), 2.05, tolerance = 1e-9)
})

test_that("the event study reproduces the design's dynamic path", {
  d <- did_fixture()
  sa <- as.data.frame(morie_did_sun_abraham(d, "y", "unit", "time", "gt"))
  post <- sa[sa$rel_time >= 0, ]
  for (i in seq_len(nrow(post))) {
    expect_equal(post$estimate[i], 1 + 0.5 * post$rel_time[i],
                 tolerance = 1e-9)
  }
  pre <- sa[sa$rel_time < 0, ]
  expect_lt(max(abs(pre$estimate)), 1e-9)
})

test_that("every heterogeneity-robust estimator agrees, and TWFE does not", {
  d <- did_fixture()
  gt <- morie_did_group_time_att(d, "y", "unit", "time", "gt",
                                 n_bootstrap = 0L)
  cs <- morie_did_aggregate_gt_att(gt, aggregation = "overall")$estimate
  imp <- as.numeric(morie_did_borusyak(d, "y", "unit", "time", "gt",
                                       n_bootstrap = 0L)$estimate)
  expect_equal(cs, 2.05, tolerance = 1e-9)
  expect_equal(imp, 2.05, tolerance = 1e-9)
  expect_lt(as.numeric(morie_did_panel_fe(d, "y", "d", "unit",
                                          "time")$estimate), 2.05 - 0.3)
})
