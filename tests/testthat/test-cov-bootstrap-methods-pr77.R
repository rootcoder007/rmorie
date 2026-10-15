# R/bootstrap_methods.R, the lines PR #77 added that no other test reached: stratified CV folds and
# the bootstrap resampler's refusals and numeric strata (quantile bins, a constant stratifier).

.bm <- function(name) get(name, envir = environment(morie_rsample_bootstraps))

test_that("stratified folds partition the rows and split each stratum as cut() does", {
  bf <- .bm(".build_folds")
  s <- rep(c("a", "b", "c"), c(13, 7, 10))
  set.seed(3)
  folds <- bf(30L, 4L, s, NULL)
  expect_length(folds, 4L)
  expect_identical(sort(unlist(folds)), 1:30)
  for (lev in unique(s)) {
    ns <- sum(s == lev)
    want <- as.vector(table(cut(seq_len(ns), 4L, labels = FALSE)))
    got <- vapply(folds, function(f) sum(s[f] == lev), integer(1))
    expect_identical(got, want)
  }
  # the CV driver uses them: every row is scored once, each fold's MSE is the hand-fitted one
  cv <- .bm(".boot_cross_validate")
  set.seed(1)
  x <- stats::runif(30)
  y <- x + stats::rnorm(30)
  fit <- function(X, y) stats::smooth.spline(X[, 1], y, df = 3)
  mse <- function(t, p) mean((t - p$y)^2)
  res <- cv(matrix(x), y, fit, mse, n_folds = 4L, stratify = s, seed = 9L)
  expect_identical(sum(res$fold_sizes), 30L)
  set.seed(9)
  f9 <- bf(30L, 4L, s, NULL)
  hand <- vapply(f9, function(f) {
    m <- stats::smooth.spline(x[-f], y[-f], df = 3)
    mean((y[f] - stats::predict(m, x[f])$y)^2)
  }, numeric(1))
  expect_equal(res$scores, hand)
})

test_that("the bootstrap resampler refuses bad times, empty data and misfit strata", {
  d <- data.frame(x = 1:10)
  expect_error(morie_rsample_bootstraps(d, times = 0L), "`times` must be a positive integer.", fixed = TRUE)
  expect_error(morie_rsample_bootstraps(d[0, , drop = FALSE]), "`data` has no rows.", fixed = TRUE)
  expect_error(morie_rsample_bootstraps(d, strata = 1:3),
               "`strata` must be a column name of `data` or a vector of length nrow(data).", fixed = TRUE)
})

test_that("a numeric stratifier is resampled within its quantile bins", {
  set.seed(4)
  d <- data.frame(z = stats::rexp(41), x = stats::rnorm(41))
  for (br in list(NULL, 2L)) {
    k <- if (is.null(br)) 4L else br
    bins <- cut(d$z, unique(stats::quantile(d$z, seq(0, 1, length.out = k + 1L), names = FALSE)),
                include.lowest = TRUE)
    rs <- if (is.null(br)) morie_rsample_bootstraps(d, 6L, strata = "z") else
      morie_rsample_bootstraps(d, 6L, strata = "z", breaks = br)
    expect_identical(attr(rs, "strata"), "z")
    for (sp in rs$splits) {
      expect_identical(as.vector(table(bins[sp$analysis])), as.vector(table(bins)))
      expect_identical(sp$assessment, setdiff(seq_len(41), sp$analysis))
    }
  }
  # a constant stratifier has one bin, so the draw is the unstratified one under the same seed
  set.seed(7)
  a <- morie_rsample_bootstraps(d, 5L, strata = rep(2, 41))
  set.seed(7)
  b <- morie_rsample_bootstraps(d, 5L)
  expect_identical(a$splits, b$splits)
  expect_true(attr(a, "strata"))
})
