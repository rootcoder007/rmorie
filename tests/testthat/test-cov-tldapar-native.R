# Data-adaptive target parameters with CV-TMLE (van der Laan & Rose 2018,
# Chap. 9; Hubbard et al. 2016): seeded splits, define-on-training /
# estimate-on-holdout, pooled influence curves and CV variable importance.

test_that("split_sample is a seeded Fisher-Yates shuffle dealt into V folds", {
  sp <- split_sample(11, 3, seed = 4)
  e <- .ghc_rng(4)
  idx <- 1:11
  for (i in 11:2) {
    j <- as.integer(.ghc_unif(e, 1L) * i) %% i + 1L
    tmp <- idx[i]
    idx[i] <- idx[j]
    idx[j] <- tmp
  }
  expect_identical(sp$estimation, lapply(1:3, function(v) sort(idx[seq(v, 11, by = 3)])))
  expect_identical(sp$training[[2]], sort(setdiff(1:11, sp$estimation[[2]])))
  expect_identical(sort(unlist(sp$estimation)), 1:11)
  # the shuffle is uniform: with n = V = 4 each fold holds one element,
  # and over 400 seeds element 1 lands in each fold about equally often
  pos <- vapply(0:399, function(s) {
    which(vapply(split_sample(4, 4, s)$estimation, function(f) 1L %in% f, TRUE))
  }, 1L)
  cnt <- tabulate(pos, 4)
  expect_gt(stats::chisq.test(cnt)$p.value, 0.001)
  expect_error(split_sample(5, 1), "V must lie in 2..5")
  expect_identical(morie_tldapar(11, 3, 4)$estimation, sp$estimation)
})

test_that("the data-adaptive parameter is defined on training and estimated on holdout", {
  set.seed(2)
  y <- stats::rnorm(30, mean = 1)
  def <- function(tr) stats::median(y[tr])
  est <- function(p, ho) list(estimate = mean(y[ho] > p), ic = as.numeric(y[ho] > p) - mean(y[ho] > p))
  r <- data_adaptive_parameter(def, est, 30, V = 5, seed = 1)
  sp <- split_sample(30, 5, 1)
  fe <- vapply(1:5, function(v) mean(y[sp$estimation[[v]]] > def(sp$training[[v]])), 1)
  expect_equal(r$fold_estimates, fe, tolerance = 1e-15)
  expect_equal(r$psi, mean(fe), tolerance = 1e-15)
  ic <- numeric(30)
  for (v in 1:5) {
    ho <- sp$estimation[[v]]
    ic[ho] <- est(def(sp$training[[v]]), ho)$ic
  }
  expect_equal(r$se, stats::sd(ic) / sqrt(30), tolerance = 1e-14)
  expect_equal(r$ci, r$psi + c(-1.96, 1.96) * r$se, tolerance = 1e-15)
})

test_that("cv_tmle averages fold estimates and pools the influence curve", {
  ics <- list(c(0.1, -0.2), c(0.3, 0), c(-0.1, 0.2, 0.05))
  r <- cv_tmle(c(0.4, 0.5, 0.45), ics, 7)
  ic <- unlist(ics)
  expect_equal(r$psi, 0.45, tolerance = 1e-15)
  expect_equal(r$se, stats::sd(ic) / sqrt(7), tolerance = 1e-15)
  expect_equal(morie_tldapar(7, fold_estimates = c(0.4, 0.5, 0.45), fold_ics = ics,
                             mode = "combine")$se, r$se)
  expect_error(cv_tmle(numeric(0), list(), 0), "no fold estimates")
  expect_error(cv_tmle(0.1, ics, 5), "7 influence-curve values for 5")
})

test_that("variable importance estimates only the variables screened in each fold", {
  set.seed(5)
  X <- matrix(stats::rnorm(40 * 3), 40)
  screen <- function(tr) if (length(tr) %% 2 == 0) c(1, 3) else 1
  effect <- function(j, ho) list(estimate = mean(X[ho, j]), ic = X[ho, j] - mean(X[ho, j]))
  r <- variable_importance(X, NULL, screen, effect, V = 4, seed = 2)
  sp <- split_sample(40, 4, 2)
  est1 <- vapply(sp$estimation, function(h) mean(X[h, 1]), 1)
  expect_equal(r$importance[["1"]]$psi, mean(est1), tolerance = 1e-15)
  expect_identical(r$importance[["1"]]$folds_selected, 4L)
  expect_null(r$importance[["2"]])
  v <- morie_tldapar(40, V = 4, seed = 2, screen = screen, effect = effect, mode = "vimp")
  expect_equal(v$importance, r$importance)
})

test_that("naive reuse returns the estimate with a validity warning", {
  r <- naive_reuse(function(idx) list(estimate = length(idx) / 2), 10)
  expect_identical(r$estimate, 5)
  expect_match(r$warning, "not valid")
  expect_identical(morie_tldapar(10, reuse_fn = function(idx) list(estimate = 1), mode = "reuse")$estimate, 1)
})
