test_that("nested judges show no significant reversal; a crossing pair is refuted", {
  # nested by construction: every judge incarcerates the defendants below its threshold, the same
  # defendants (the same uniform u) seen by every judge, in every group
  set.seed(7)
  n <- 6000
  judge <- rep(sprintf("J%d", 1:6), length.out = n)
  thr <- c(J1 = 0.2, J2 = 0.3, J3 = 0.4, J4 = 0.5, J5 = 0.6, J6 = 0.7)
  group <- rep(c("a", "b", "c"), each = n / 3)
  u <- runif(n)
  d <- as.integer(u < thr[judge])
  r <- morie_judge_monotonicity(judge, d, group)
  expect_equal(r$summary$significant, 0L)
  expect_equal(nrow(r$comparisons), 3 * choose(6, 2))
  expect_true(all(r$by_group$slope > 0))
  expect_equal(r$judges$judge[1], "J6")
  # the comparison statistics are the textbook pooled two-proportion test
  row <- r$comparisons[1, ]
  pp <- (row$group_rate_stricter * row$n_stricter + row$group_rate_lenient * row$n_lenient) /
    (row$n_stricter + row$n_lenient)
  z <- (row$group_rate_lenient - row$group_rate_stricter) /
    sqrt(pp * (1 - pp) * (1 / row$n_stricter + 1 / row$n_lenient))
  expect_equal(row$z, z)
  expect_equal(row$p_value, stats::pnorm(-z))
  expect_equal(r$comparisons$p_adjusted, stats::p.adjust(r$comparisons$p_value, "holm"))
  # judge J6 becomes the most lenient in group c: it is stricter overall and more lenient there
  d2 <- d
  c6 <- judge == "J6" & group == "c"
  d2[c6] <- as.integer(u[c6] < 0.05)
  r2 <- morie_judge_monotonicity(judge, d2, group)
  sig <- r2$comparisons[r2$comparisons$significant, ]
  # J6's overall rate falls with it, so its order flips against the judges just below it in the
  # other groups too: every significant reversal involves J6, and group c's slope is the lowest
  expect_gt(nrow(sig), 0L)
  expect_true(all(sig$stricter == "J6" | sig$lenient == "J6"))
  expect_true(all(sig$group_rate_stricter < sig$group_rate_lenient))
  sl <- stats::setNames(r2$by_group$slope, r2$by_group$group)
  expect_lt(sl[["c"]], min(sl[["a"]], sl[["b"]]))
})

test_that("cells below min_n are not compared, and the check refuses bad input", {
  judge <- c(rep("A", 40), rep("B", 40), rep("C", 5))
  d <- c(rep(0:1, 20), rep(c(1, 1, 0, 1), 10), rep(1, 5))
  group <- rep("g", 85)
  r <- morie_judge_monotonicity(judge, d, group, min_n = 30)
  expect_equal(nrow(r$comparisons), 1L)
  expect_identical(sort(c(r$comparisons$stricter, r$comparisons$lenient)), c("A", "B"))
  # one judge per group: nothing to compare
  r0 <- morie_judge_monotonicity(c("A", "A", "B", "B"), c(0, 1, 1, 1), c("x", "x", "y", "y"), min_n = 1)
  expect_equal(r0$summary$comparisons, 0L)
  # tied overall rates are not ordered, so not compared
  rt <- morie_judge_monotonicity(rep(c("A", "B"), each = 4), c(0, 1, 0, 1, 1, 0, 1, 0), rep("g", 8), min_n = 1)
  expect_equal(nrow(rt$comparisons), 0L)
  expect_error(morie_judge_monotonicity(1:3, 1:2, 1:3), "equal length")
  expect_error(morie_judge_monotonicity(c(1, NA), 0:1, 1:2), "missing")
  expect_error(morie_judge_monotonicity(1:2, c(0, 2), 1:2), "0/1")
  expect_error(morie_judge_monotonicity(1:2, 0:1, 1:2, min_n = 0), "min_n")
  expect_error(morie_judge_monotonicity(1:2, 0:1, 1:2, alpha = 1), "alpha")
})
