# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The fairness GAN callables run on the native base-R backend on every
# install: no torch / reticulate / JAX gate.

test_that("the fairness backend is always native", {
  bk <- rmorie:::.fairness_backend()
  expect_identical(bk$kind, "native")
  expect_false(any(grepl("requireNamespace|torch::|reticulate::",
                         deparse(rmorie:::.fairness_backend))))
})

test_that("morie_fairness_spatial_gan fits and samples without torch", {
  set.seed(1)
  pts <- cbind(stats::rnorm(80, 5, 2), stats::rnorm(80, -3, 0.5))
  r <- morie_fairness_spatial_gan(pts, steps = 20L, batch_size = 16L,
                                  latent_dim = 4L, hidden = 8L, seed = 3L)
  expect_true(r$fitted)
  expect_identical(r$backend, "native")
  expect_length(r$history, 20L)
  s1 <- r$sample(10L, seed = 7L)
  s2 <- r$sample(10L, seed = 7L)
  expect_equal(dim(s1), c(10L, 2L))
  expect_identical(s1, s2)  # seeded sampling is reproducible
})

test_that("morie_fairness_ctgan_debiaser rebalances favourable rates natively", {
  set.seed(4)
  n <- 400L
  g <- sample(c("A", "B"), n, TRUE)
  y <- stats::rbinom(n, 1, ifelse(g == "A", 0.7, 0.3))
  df <- data.frame(group = g, outcome = y, x1 = stats::rnorm(n),
                   stringsAsFactors = FALSE)
  r <- morie_fairness_ctgan_debiaser(df, "outcome", "x1", privileged = "A",
                                     n = 4000L, seed = 1L)
  expect_true(r$fitted)
  expect_identical(r$backend, "native")
  rates <- tapply(r$debiased$outcome, r$debiased$group, mean)
  # outcomes are Bernoulli(target_rate) for every group; with ~2000 rows
  # per group the sd of each rate is ~0.01, so 0.05 is a 5-sd band
  expect_lt(max(abs(rates - r$target_rate)), 0.05)
})
