.dp_items <- function(seed, n = 3000, p = 12, dif = 0, di = 1) {
  set.seed(seed)
  grp <- rbinom(n, 1, 0.5)
  th <- rnorm(n) - 0.5 * grp
  X <- matrix(0, n, p)
  for (j in seq_len(p)) {
    eta <- th - (j - p / 2) * 0.3
    if (j == di) eta <- eta - dif * grp
    X[, j] <- as.numeric(runif(n) < 1 / (1 + exp(-eta)))
  }
  list(X = X, grp = grp)
}

test_that("SIBTEST finds a planted DIF item", {
  d <- .dp_items(2, dif = 1.2, di = 3)
  r <- morie_sibtest(d$X, d$grp)
  expect_lt(r$p_value[3], 0.01)
  expect_equal(which.max(abs(r$statistic)), 3L)
})

test_that("the correction is what makes SIBTEST's size usable", {
  raw <- corr <- 0
  for (s in 1:4) {
    d <- .dp_items(s, dif = 0)
    raw <- raw + sum(morie_sibtest(d$X, d$grp, correct = FALSE)$p_value < 0.05)
    corr <- corr + sum(morie_sibtest(d$X, d$grp, correct = TRUE)$p_value < 0.05)
  }
  expect_lt(corr, raw / 2)
})

test_that("B is beta over its standard error", {
  d <- .dp_items(4, dif = 0.8)
  r <- morie_sibtest(d$X, d$grp)
  ok <- r$se > 0
  expect_equal(r$statistic[ok], (r$beta / r$se)[ok], tolerance = 1e-12)
})

.dp_panel <- function(seed, N = 12, Tn = 100, coint = TRUE, m = 2) {
  set.seed(seed)
  rows <- list()
  grp <- c()
  for (i in seq_len(N)) {
    xs <- lapply(seq_len(m), function(k) cumsum(rnorm(Tn)))
    y <- if (coint) Reduce(`+`, xs) + rnorm(Tn) else cumsum(rnorm(Tn))
    rows[[i]] <- cbind(y, do.call(cbind, xs))
    grp <- c(grp, rep(i, Tn))
  }
  list(X = do.call(rbind, rows), g = grp)
}

test_that("all five Pedroni statistics reject a cointegrated panel", {
  d <- .dp_panel(3, coint = TRUE)
  r <- morie_panel_cointegration(d$X, d$g)
  for (nm in c("panel_v", "panel_rho", "panel_t", "group_rho", "group_t")) {
    expect_lt(r$p_values[[nm]], 0.01)
  }
})

test_that("no Pedroni statistic rejects a spurious panel", {
  d <- .dp_panel(3, coint = FALSE)
  r <- morie_panel_cointegration(d$X, d$g)
  for (nm in c("panel_v", "panel_rho", "panel_t", "group_rho", "group_t")) {
    expect_gt(r$p_values[[nm]], 0.05)
  }
})

test_that("panel v is the right-tail statistic", {
  d <- .dp_panel(3, coint = TRUE)
  r <- morie_panel_cointegration(d$X, d$g)
  expect_gt(r$z[["panel_v"]], 0)
  for (nm in c("panel_rho", "panel_t", "group_rho", "group_t")) {
    expect_lt(r$z[[nm]], 0)
  }
})

test_that("standardisation is Pedroni equation 2 and the table matches print", {
  expect_equal(.PEDRONI_T2$standard[["2"]]$panel_v, c(6.982, 81.145))
  expect_equal(.PEDRONI_T2$intercept[["2"]]$group_t, c(-2.453, 0.618))
  expect_equal(.PEDRONI_T2$trend[["7"]]$panel_rho, c(-32.756, 154.378))
  for (cs in names(.PEDRONI_T2)) expect_equal(names(.PEDRONI_T2[[cs]]), as.character(2:7))
  d <- .dp_panel(7, N = 10, coint = TRUE)
  r <- morie_panel_cointegration(d$X, d$g)
  for (nm in names(r$statistics)) {
    mv <- .PEDRONI_T2$intercept[["2"]][[nm]]
    expect_equal(r$z[[nm]], (r$statistics[[nm]] - mv[1] * sqrt(r$n_units)) / sqrt(mv[2]),
                 tolerance = 1e-12)
  }
})

test_that("a single-regressor panel refuses to standardise", {
  d <- .dp_panel(9, N = 8, m = 1)
  r <- morie_panel_cointegration(d$X, d$g)
  expect_equal(r$n_regressors, 1L)
  expect_length(r$z, 0L)
  expect_true(any(grepl("m = 2..7", r$warnings, fixed = TRUE)))
})
