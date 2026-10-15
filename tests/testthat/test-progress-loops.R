# The long loops show rmoriebricklayer's progress bar: each one names its work and counts its
# steps. Off a terminal the bar is a milestone line on stderr, so the tests read the messages.

st <- asNamespace("rmoriebricklayer")$.bl_progress_state   # an environment: its fields can be set

bars <- function(expr) {
  old <- st$delay
  st$delay <- 0
  on.exit(st$delay <- old, add = TRUE)
  withr::local_options(morie.progress = TRUE, morie.quiet = NULL)
  paste(utils::capture.output(type = "message", invisible(force(expr))), collapse = "\n")
}

test_that("the bootstraps count their replicates", {
  out <- bars(morie_boot(1:30, function(d, i) mean(d[i]), R = 20L))
  expect_match(out, "bootstrap: 20 of 20", fixed = TRUE)
  out <- bars(morie_tsboot(as.numeric(1:40), mean, R = 10L, l = 5L))
  expect_match(out, "block bootstrap: 9 of 10", fixed = TRUE)
  out <- bars(morie_bootstrap_sample(data.frame(x = 1:25), function(d) mean(d$x),
                                     n_bootstrap = 10L))
  expect_match(out, "bootstrap: 10 of 10", fixed = TRUE)
})

test_that("forests count trees, the causal-forest bootstrap its replicates", {
  set.seed(1)
  n <- 120L
  x <- matrix(stats::rnorm(n * 2L), n)
  d <- stats::rbinom(n, 1L, 0.5)
  y <- x[, 1] + d + stats::rnorm(n)
  out <- bars(morie_causal_forest(y, d, x, n_trees = 10L, min_leaf = 5L))
  expect_match(out, "causal forest: 10 of 11", fixed = TRUE)
  expect_match(out, "effects for every row", fixed = TRUE)
  out <- bars(morie_causal_forest_bootstrap(y, d, x, B = 3L, n_trees = 5L, min_leaf = 5L))
  expect_match(out, "causal forest bootstrap: 2 of 3", fixed = TRUE)
  expect_false(grepl("causal forest: ", gsub("causal forest bootstrap", "", out), fixed = TRUE))
  out <- bars(morie_esl_random_forest(x, y, B = 10L))
  expect_match(out, "random forest: 9 of 10", fixed = TRUE)
  out <- bars(morie_random_forest_ensemble(x, y, n_estimators = 10L))
  expect_match(out, "random forest: growing 10 trees", fixed = TRUE)
})

test_that("DML and the super learners count their folds", {
  set.seed(2)
  n <- 150L
  df <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  df$t <- stats::rbinom(n, 1L, stats::plogis(df$x1))
  df$y <- df$x1 + df$t + stats::rnorm(n)
  out <- bars(morie_dml_clustered(df, "t", "y", c("x1", "x2"), n_folds = 3L))
  expect_match(out, "clustered DML: 2 of 3", fixed = TRUE)
  out <- bars(morie_caus_dml_partial_lin(df$y, df$t, as.matrix(df[c("x1", "x2")]), n_folds = 3))
  expect_match(out, "partially linear DML: 2 of 3", fixed = TRUE)
  out <- bars(super_learner(df$y, as.matrix(df[c("x1", "x2")]), n_folds = 3))
  expect_match(out, "super learner: 3 of 4", fixed = TRUE)
  out <- bars(morie_otis_irm_dml(df, "t", "y", c("x1", "x2"), n_folds = 3L))
  expect_match(out, "IRM-DML: 2 of 3", fixed = TRUE)
  out <- bars(morie_otis_aipw_superlearner(df, "t", "y", c("x1", "x2"), n_folds = 2L))
  expect_match(out, "SuperLearner AIPW: 5 of 6", fixed = TRUE)
})

test_that("a bar stays silent unless someone is watching, and when quiet", {
  old <- st$delay
  st$delay <- 0
  on.exit(st$delay <- old, add = TRUE)
  withr::local_options(morie.progress = TRUE, morie.quiet = TRUE)
  expect_identical(utils::capture.output(type = "message",
    invisible(morie_boot(1:30, function(d, i) mean(d[i]), R = 5L))), character())
})
