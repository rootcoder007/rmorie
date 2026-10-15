# R/dtrsp.R, the lines PR #77 added that no other test reached: the depth and all-missing refusals,
# a node with more than five qualifying surrogates (rpart keeps the best five), and rpart's
# quicksort finishing a long run without its insertion-sort exit, against rpart.

.rpart_tree <- function(x, y) {
  df <- as.data.frame(x)
  names(df) <- paste0("x", seq_len(ncol(x)) - 1L)
  df$.y <- factor(y)
  fit <- rpart::rpart(.y ~ ., data = df, method = "class",
                      control = rpart::rpart.control(maxdepth = 30L, cp = 0, minsplit = 2L,
                                                     minbucket = 1L, xval = 0L))
  fi <- stats::setNames(rep(0, ncol(x)), names(df)[seq_len(ncol(x))])
  fi[names(fit$variable.importance)] <- fit$variable.importance
  list(fit = fit, acc = mean(stats::predict(fit, df, type = "class") == df$.y),
       leaves = sum(fit$frame$var == "<leaf>"), fi = as.numeric(fi / sum(fi)))
}

test_that("a depth below 1 and rows with every predictor missing are refused", {
  x <- matrix(stats::rnorm(20), 10, 2)
  y <- rep(c("a", "b"), 5)
  expect_error(morie_decision_tree_split(x, y, max_depth = 0L), "Maximum depth must be at least 1", fixed = TRUE)
  expect_error(morie_decision_tree_split(matrix(NA_real_, 10, 2), y),
               "No observations left after removing missing values", fixed = TRUE)
})

test_that("seven correlated predictors leave six surrogates; the best five are kept as in rpart", {
  skip_if_not_installed("rpart")
  skip_if(grepl("^(aarch64|arm64)", R.version$arch), "rpart breaks ties by fused rounding on arm64")
  set.seed(21)
  n <- 150
  z <- stats::rnorm(n)
  x <- cbind(z, z + matrix(stats::rnorm(n * 6, sd = 0.3), n, 6))
  y <- cut(z + stats::rnorm(n, sd = 0.5), c(-Inf, -0.4, 0.5, Inf), labels = c("lo", "mid", "hi"))
  fit <- get(".morie_cart_class_fit", envir = environment(morie_decision_tree_split))(unname(x), y)
  expect_length(fit$nodes[["1"]]$surr, 5L)
  ref <- .rpart_tree(x, y)
  # rpart's root rows: the primary split, its competitors, then its surrogates in order
  fr <- ref$fit$frame
  sur <- rownames(ref$fit$splits)[1L + fr$ncompete[1L] + seq_len(fr$nsurrogate[1L])]
  expect_identical(paste0("x", vapply(fit$nodes[["1"]]$surr, function(s) s$var, integer(1)) - 1L), sur)
  nat <- morie_decision_tree_split(x, y)
  expect_identical(nat$n_leaves, as.integer(ref$leaves))
  expect_equal(nat$train_accuracy, ref$acc, tolerance = 1e-12)
  expect_equal(nat$feature_importances, ref$fi, tolerance = 1e-12)
})

test_that("unbalanced classes go through rpart's quicksort, which returns a sorting permutation", {
  skip_if_not_installed("rpart")
  skip_if(grepl("^(aarch64|arm64)", R.version$arch), "rpart breaks ties by fused rounding on arm64")
  ms <- get(".morie_rpart_mysort", envir = environment(morie_decision_tree_split))
  set.seed(121)
  for (v in list(round(stats::rnorm(80), 1), rep(c(3, 1, 2), 9), as.numeric(40:1), rep(1, 12), c(rep(0, 15), 1))) {
    o <- ms(v)
    expect_identical(sort(o), seq_along(v))
    expect_identical(v[o], sort(v))
  }
  x <- matrix(round(stats::rnorm(80 * 4), 1), 80, 4)
  y <- factor(sample(1:4, 80, TRUE, prob = c(0.55, 0.25, 0.15, 0.05)))
  nat <- morie_decision_tree_split(x, y)
  ref <- .rpart_tree(x, y)
  expect_identical(nat$n_leaves, as.integer(ref$leaves))
  expect_equal(nat$train_accuracy, ref$acc, tolerance = 1e-12)
  expect_equal(nat$feature_importances, ref$fi, tolerance = 1e-12)
})
