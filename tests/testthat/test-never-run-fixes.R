# SPDX-License-Identifier: AGPL-3.0-or-later
# Regression pins for four never-run paths fixed on 2026-09-01. Each one
# was a real bug that only executing the code exposed; every assertion
# below fails on the pre-fix code.

test_that("survnnr fits from a list of rows (backprop no longer sees a nested list)", {
  set.seed(1); n <- 40
  X <- lapply(seq_len(n), function(i) rnorm(3)); tm <- rexp(n); ev <- rbinom(n, 1, .7)
  fit <- morie_survnnr_fit(X, tm, ev, hidden = c(4L), n_epochs = 20)
  expect_length(fit$risk, n)
  expect_true(all(is.finite(fit$loss_history)))
  expect_true(fit$epochs >= 2)
})

test_that("rjmcmc: the uniform stream advances and births are accepted from k = 0", {
  u <- .unif_stream(1L)
  expect_length(unique(c(u(), u(), u(), u())), 4L)
  set.seed(2); y <- sort(c(runif(30, 0, .4), runif(60, .4, 1)))
  r <- changepoint_rjmcmc(y = y, L = 1, n_iter = 4000L, burn_in = 1000L, seed = 1L)
  # a clear step in intensity must pull mass off k = 0
  expect_gt(r$k_mean, 0.5)
  expect_lt(r$k_posterior[1], 0.5)
  # deterministic for a seed
  r2 <- changepoint_rjmcmc(y = y, L = 1, n_iter = 4000L, burn_in = 1000L, seed = 1L)
  expect_identical(r$k_posterior, r2$k_posterior)
})

test_that("mqtmpl EM scan matches the Python arm on a fixed backcross (cM positions)", {
  y  <- c(1.9, 0.3, 2.4, 0.1, 2.2, 0.6, 1.7, 0.4, 2.0, 0.2, 2.6, 0.5)
  mk <- list(c(1,0,1,0,1,0,1,0,1,0,1,0), c(1,0,1,0,1,1,1,0,0,0,1,0), c(0,0,1,0,1,1,0,0,0,1,1,0))
  f <- if (exists("mqtmpl_scanone")) mqtmpl_scanone else morie_mqtmpl_scanone
  r <- f(y, mk, c(0, 10, 20), method = "em", step = 5)
  expect_equal(r$peak_position, 0)
  expect_equal(r$peak_lod, 6.89523009508, tolerance = 1e-9)
  expect_equal(r$lod, c(6.89523009508, 6.28234432026, 1.84002019604, 1.84002019604, 1.1367051393, 0.295401178129), tolerance = 1e-9)
})

test_that("native Parquet reader handles an OPTIONAL string column (no arrow needed)", {
  df <- data.frame(i = 1:50L, v = seq(0.5, 25, by = 0.5), s = paste0("row", 1:50), stringsAsFactors = FALSE)
  fp <- tempfile(fileext = ".parquet")
  morie_write_parquet(df, fp, compression = NULL)
  back <- morie_fetch_parquet(fp)
  expect_equal(back$i, df$i)
  expect_equal(back$v, df$v)
  expect_identical(back$s, df$s)     # was "d\001", row1 ... row49 before the fix
  expect_equal(nrow(back), 50L)
})
