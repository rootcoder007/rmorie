# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native jackknife / CV folds / bootstrap resample set, cross-validated
# against bootstrap::jackknife and the rsample::bootstraps design.

test_that("jackknife reproduces bootstrap::jackknife", {
  skip_if_not_installed("bootstrap")
  set.seed(42)
  x <- stats::rgamma(60, shape = 2, rate = 0.5)
  stats_list <- list(
    mean = mean,
    median = stats::median,
    sd = stats::sd,
    cv = function(v) stats::sd(v) / mean(v)
  )
  for (nm in names(stats_list)) {
    f <- stats_list[[nm]]
    ref <- bootstrap::jackknife(x, f)
    nat <- jackknife(x, f)
    # identical leave-one-out arithmetic -> rounding-level agreement
    expect_equal(nat$jackknife_estimates, ref$jack.values, tolerance = 1e-12)
    expect_equal(nat$se, ref$jack.se, tolerance = 1e-12)
    expect_equal(nat$bias, ref$jack.bias, tolerance = 1e-12)
  }
})

test_that("jackknife deletes rows of a matrix", {
  set.seed(1)
  X <- cbind(stats::rnorm(30), stats::rnorm(30))
  f <- function(d) stats::cor(d[, 1], d[, 2])
  res <- jackknife(X, f)
  expect_length(res$jackknife_estimates, 30L)
  expect_equal(res$jackknife_estimates[3], f(X[-3, ]))
})

test_that(".build_folds partitions the rows natively", {
  set.seed(7)
  folds <- rmorie:::.build_folds(53, 5, NULL, NULL)
  expect_length(folds, 5L)
  expect_setequal(unlist(folds), 1:53)
  expect_false(anyDuplicated(unlist(folds)) > 0)
  expect_true(all(lengths(folds) %in% c(10L, 11L)))
  loo <- rmorie:::.build_folds(12, 12, NULL, NULL)
  expect_true(all(lengths(loo) == 1L))
  expect_false(any(grepl("rsample", deparse(rmorie:::.build_folds))))
})

test_that("morie_rsample_bootstraps returns a native resample set", {
  set.seed(3)
  n <- 200
  df <- data.frame(x = stats::rnorm(n), g = rep(c("a", "b"), c(150, 50)))
  rs <- morie_rsample_bootstraps(df, times = 25L)
  expect_s3_class(rs, "morie_bootstraps")
  expect_equal(nrow(rs), 25L)
  expect_equal(rs$id[c(1, 25)], c("Bootstrap01", "Bootstrap25"))
  for (sp in rs$splits) {
    expect_length(sp$analysis, n)
    expect_true(all(sp$analysis %in% seq_len(n)))
    expect_setequal(sp$assessment, setdiff(seq_len(n), sp$analysis))
  }
  # Out-of-bag fraction ~ (1 - 1/n)^n ~ e^-1 = 0.368; with 25 x 200 rows
  # the Monte-Carlo sd of the mean OOB fraction is about 0.0075, so a
  # 0.04 band is > 5 sd.
  oob <- mean(vapply(rs$splits, function(s) length(s$assessment) / n, 0))
  expect_lt(abs(oob - exp(-1)), 0.04)

  # strata: per-stratum counts are preserved in every analysis set
  rs2 <- morie_rsample_bootstraps(df, times = 10L, strata = "g", apparent = TRUE)
  expect_equal(nrow(rs2), 11L)
  expect_equal(rs2$id[11], "Apparent")
  for (sp in rs2$splits[1:10]) {
    expect_equal(as.vector(table(df$g[sp$analysis])), c(150L, 50L))
  }
  expect_equal(rs2$splits[[11]]$analysis, seq_len(n))
  expect_error(morie_rsample_bootstraps(df, times = 2L, pool = 0.1), "unsupported")
})

test_that("morie_rsample_bootstraps matches rsample::bootstraps sizes", {
  skip_if_not_installed("rsample")
  set.seed(11)
  df <- data.frame(x = stats::rnorm(120))
  ref <- rsample::bootstraps(df, times = 10L)
  nat <- morie_rsample_bootstraps(df, times = 10L)
  expect_equal(nrow(nat), nrow(ref))
  expect_equal(nat$id, ref$id)
  ref_in <- vapply(ref$splits, function(s) length(s$in_id), 0L)
  nat_in <- vapply(nat$splits, function(s) length(s$analysis), 0L)
  expect_equal(nat_in, ref_in)
})
