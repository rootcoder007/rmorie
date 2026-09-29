# Coverage tests for R/blinkg_native.R (BLINK GWAS): the marker scan
# against lm, LD and bin filters, BIC/AIC model selection against logLik,
# and the iterative driver.

bk_data <- function() {
  n <- 30
  i <- 1:n
  g <- list(
    (i * 7) %% 3, (i * 5 + 1) %% 3, (i * 11 + 2) %% 3, (i %% 2) * 2,
    ((i * 7) %% 3 + (i %% 5 == 0)) %% 3, floor(i / 11)
  )
  y <- 2 * g[[1]] - 1.5 * g[[4]] + sin(i)
  list(y = y, g = g, cv = list(cos(i)))
}

test_that("the marker scan equals lm with QTN pseudo-covariates", {
  d <- bk_data()
  s <- morie_blinkg_scan(d$y, d$g, d$cv, qtn = 1L)
  # marker 3 is an exact linear function of the QTN, so its fit is singular
  expect_true(is.nan(s$p[3]) && is.nan(s$se[3]))
  for (j in c(2, 4:6)) {
    ct <- summary(lm(d$y ~ d$cv[[1]] + d$g[[1]] + d$g[[j]]))$coefficients
    expect_equal(s$beta[j], ct[4, 1], tolerance = 1e-9)
    expect_equal(s$se[j], ct[4, 2], tolerance = 1e-9)
    expect_equal(s$p[j], ct[4, 4], tolerance = 1e-9)
  }
  ct1 <- summary(lm(d$y ~ d$cv[[1]] + d$g[[1]]))$coefficients
  expect_equal(s$t[1], ct1[3, 3], tolerance = 1e-9)
  small <- morie_blinkg_scan(1:3, list(c(0, 1, 2)), list(c(1, 0, 1), c(2, 2, 1)))
  expect_true(is.nan(small$p) && is.nan(small$se))
})

test_that("LD and bin filters keep the strongest marker per block", {
  d <- bk_data()
  o <- c(1L, 5L, 2L, 4L, 6L, 3L)
  keep <- integer(0)
  for (j in o) if (all(abs(vapply(keep, function(q) cor(d$g[[j]], d$g[[q]]), 0)) <= 0.6)) keep <- c(keep, j)
  expect_equal(morie_blinkg_ld_filter(d$g, o, threshold = 0.6), keep)
  expect_equal(morie_blinkg_ld_filter(d$g, o, threshold = 1), o)
  pos <- c(10, 250, 120, 900, 180, 610)
  expect_equal(morie_blinkg_bin_filter(o, pos, 200), o[!duplicated(floor(pos[o] / 200))])
  expect_error(morie_blinkg_bin_filter(o, pos, 0), "bin size must be positive")
})

test_that("BIC and AIC model selection over nested candidate sets", {
  d <- bk_data()
  cand <- c(1L, 4L, 2L)
  sc <- vapply(1:3, function(k) {
    X <- do.call(cbind, c(d$cv, d$g[cand[1:k]]))
    -2 * as.numeric(logLik(lm(d$y ~ X))) + k * log(30)
  }, 0)
  r <- morie_blinkg_select(d$y, d$g, cand, d$cv)
  expect_equal(r$scores, sc, tolerance = 1e-9)
  expect_equal(r$k, which.min(sc))
  expect_equal(r$qtn, cand[seq_len(which.min(sc))])
  a <- morie_blinkg_select(d$y, d$g, cand, d$cv, "aic")
  expect_equal(a$scores, sc - (1:3) * log(30) + (1:3) * 2, tolerance = 1e-9)
  expect_equal(morie_blinkg_select(d$y, d$g, cand, criterion = "none"), list(qtn = cand, scores = numeric(0), k = 3L))
  expect_equal(morie_blinkg_select(d$y, d$g, integer(0))$k, 0L)
  expect_error(morie_blinkg_select(d$y, d$g, cand, criterion = "dic"), "criterion must be one of")
})

test_that("the BLINK driver iterates to a fixed QTN set", {
  d <- bk_data()
  r <- morie_blinkg(d$y, d$g, covars = d$cv)
  expect_true(r$converged)
  expect_equal(r$threshold, 0.01 / 6)
  s <- morie_blinkg_scan(d$y, d$g, d$cv, r$qtn + 1L)
  expect_equal(r$p, s$p)
  expect_true(all(c(0L, 3L) %in% r$qtn))
  expect_equal(r$significant, which(!is.nan(s$p) & s$p < 0.01 / 6) - 1L)
  expect_equal(r$lambda_gc, median(s$t^2, na.rm = TRUE) / qchisq(0.5, 1), tolerance = 1e-12)
  expect_equal(r$estimate, min(s$p, na.rm = TRUE))
  b <- morie_blinkg(d$y, d$g, positions = c(10, 250, 120, 900, 180, 610), covars = d$cv, selection = "bin", bin_size = 200)
  expect_equal(b$selection, "bin")
  expect_true(0L %in% b$qtn)
  one <- morie_blinkg(d$y, d$g, max_iter = 1)
  expect_equal(one$iterations, 1L)
  expect_match(morie_blinkg_cheatsheet(), "ld, bin")
  expect_error(morie_blinkg(d$y, d$g, selection = "clump"), "selection must be one of")
  expect_error(morie_blinkg(d$y, d$g, criterion = "dic"), "criterion must be one of")
  expect_error(morie_blinkg(1:2, list(1:2)), "at least three")
  expect_error(morie_blinkg(d$y, list()), "at least one marker")
  expect_error(morie_blinkg(d$y, list(1:3)), "one value per individual")
  expect_error(morie_blinkg(d$y, d$g, selection = "bin"), "needs marker positions")
  expect_error(morie_blinkg(d$y, d$g, positions = 1:2, selection = "bin"), "one entry per marker")
  expect_error(morie_blinkg(d$y, d$g, positions = 1:6, selection = "bin"), "needs a bin size")
})
