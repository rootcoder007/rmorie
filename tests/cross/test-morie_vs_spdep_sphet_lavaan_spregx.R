# Cross tests: spdep::moran.mc, spatialreg::spautolm, sphet::spreg and kpjtest, lavaan MLM scaling.

setup_ <- function() {
  g <- expand.grid(c = 0:7, r = 0:4)
  n <- nrow(g)
  A <- outer(seq_len(n), seq_len(n), function(i, j) as.numeric(abs(g$c[i] - g$c[j]) + abs(g$r[i] - g$r[j]) == 1))
  B <- outer(seq_len(n), seq_len(n), function(i, j) {
    dc <- abs(g$c[i] - g$c[j])
    dr <- abs(g$r[i] - g$r[j])
    as.numeric((dc == 1 & dr == 1) | (dc == 2 & dr == 0))
  })
  i <- 0:(n - 1)
  X <- cbind(x1 = sin(i * 0.7) + 0.1 * (i %% 5), x2 = cos(i * 1.3) * (1 + 0.02 * i))
  y <- 1 + 0.8 * X[, 1] - 0.5 * X[, 2] + 0.4 * sin(i * 2.1) + 0.3 * cos(i * i * 0.1)
  list(A = A, W = A / rowSums(A), W1 = B / rowSums(B), X = X, y = y,
       df = data.frame(y = y, X, pop = 50 + (i * 37) %% 90))
}

test_that("Moran's I and spautolm fits equal spdep and spatialreg", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("spatialreg")
  s <- setup_()
  lb <- spdep::mat2listw(s$A, style = "B")
  lw <- spdep::mat2listw(s$W, style = "W")
  expect_equal(MoranPermutationTest(s$y, s$A, nsim = 9)$statistic, as.numeric(spdep::moran.mc(s$y, lb, nsim = 9)$statistic),
               tolerance = 1e-12)
  for (args in list(list(W = s$W, lw = lw, fam = "SAR", w = FALSE), list(W = s$A, lw = lb, fam = "CAR", w = FALSE),
                    list(W = s$W, lw = lw, fam = "SAR", w = TRUE))) {
    ref <- if (args$w) spatialreg::spautolm(y ~ x1 + x2, s$df, args$lw, weights = pop) else
      spatialreg::spautolm(y ~ x1 + x2, s$df, args$lw, family = args$fam)
    got <- SpautolmFit(s$y, s$X, args$W, weights = if (args$w) s$df$pop else NULL, family = args$fam,
                       bounds = if (args$fam == "CAR") c(-0.3, 0.26) else c(-0.99, 0.99))
    # coef() of spautolm appends lambda; optimize() stops at tol = eps^(2/3)
    expect_equal(c(got$lambda_, got$beta), as.numeric(c(ref$lambda, ref$fit$coefficients)), tolerance = 1e-7)
    expect_equal(got$loglik, as.numeric(ref$LL), tolerance = 1e-9)
  }
})

test_that("S2SLS, KP-HET and the J-test equal sphet", {
  skip_if_not_installed("sphet")
  skip_if_not_installed("spdep")
  s <- setup_()
  lw <- spdep::mat2listw(s$W, style = "W")
  lw1 <- spdep::mat2listw(s$W1, style = "W")
  for (dur in c(FALSE, TRUE)) for (het in c(FALSE, TRUE)) {
    ref <- sphet::spreg(y ~ x1 + x2, s$df, listw = lw, model = "lag", Durbin = dur, het = het)
    got <- S2slsLag(s$y, s$X, s$W, durbin = dur, het = het)
    expect_equal(got$coefficients, as.numeric(coef(ref)), tolerance = 1e-10)
    expect_equal(got$se, unname(sqrt(diag(as.matrix(ref$var)))), tolerance = 1e-10)
  }
  ref <- sphet::spreg(y ~ x1 + x2, s$df, listw = lw, model = "error", het = TRUE)
  got <- GmErrorHet(s$y, s$X, s$W)
  # sphet minimises with nlminb; the GM objective is a quartic in rho
  expect_equal(c(got$beta, got$rho), as.numeric(coef(ref)), tolerance = 1e-7)
  expect_equal(got$se, unname(sqrt(diag(as.matrix(ref$var)))), tolerance = 1e-6)
  j <- sphet::kpjtest(y ~ x1 + x2, y ~ x1 + x2, s$df, listw0 = lw, listw1 = lw1)
  expect_equal(SpatialJTest(s$y, s$X, s$W, s$X, s$W1, method = "sphet")$statistic,
               as.numeric(tail(summary(j)$CoefTable[, 3], 1)), tolerance = 1e-10)
})

test_that("SatorraBentler scaling equals lavaan MLM", {
  skip_if_not_installed("lavaan")
  n <- 300
  i <- seq_len(n)
  f <- sin(i * 0.37) + cos(i * 0.11)
  X <- sapply(1:4, function(j) 0.8 * f + (sin(i * j * 1.3)^3) + 0.2 * j * cos(i * (j + 2) * 0.7))
  colnames(X) <- paste0("x", 1:4)
  fit <- lavaan::cfa("F =~ x1 + x2 + x3 + x4", data = as.data.frame(X), estimator = "MLM")
  fm <- lavaan::fitMeasures(fit, c("chisq", "df", "chisq.scaling.factor"))
  r <- SatorraBentler(X, lavaan::lavInspect(fit, "implied")$cov, lavaan::lavInspect(fit, "delta"), fm[["chisq"]], fm[["df"]])
  expect_equal(r$scaling, fm[["chisq.scaling.factor"]], tolerance = 1e-8)
})
