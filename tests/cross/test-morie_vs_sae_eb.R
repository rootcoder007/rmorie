fy <- c(10.2, 12.8, 11.3, 15.0, 13.2, 9.7, 16.5, 8.9, 14.1, 12.0)
fx <- c(3.1, 2.2, 4.0, 2.9, 4.4, 1.9, 3.6, 2.7, 4.8, 3.3)
fd <- c(1.2, 0.8, 2.0, 1.5, 0.9, 1.1, 1.7, 0.6, 1.3, 1.0)
area <- rep(1:5, c(3, 4, 2, 5, 3))
ux <- c(1.2, 2.3, 1.8, 3.1, 2.2, 2.9, 3.5, 0.8, 1.1, 2.6, 3.3, 2.8, 3.9, 3.0, 1.9, 2.4, 2.1)
uy <- c(4.1, 5.9, 5.0, 7.9, 5.6, 7.5, 8.9, 2.4, 3.6, 7.0, 8.1, 7.4, 9.6, 7.7, 4.4, 5.5, 5.3)
xbar <- c(1.7, 2.9, 1.0, 3.1, 2.2)
popn <- c(50, 80, 30, 120, 60)

test_that("FayHerriot equals sae::eblupFH and mseFH, and metafor's tau^2", {
  skip_if_not_installed("sae")
  for (m in c("REML", "ML", "FH")) {
    ref <- sae::mseFH(fy ~ fx, fd, method = m, MAXITER = 1000, PRECISION = 1e-12)
    r <- FayHerriot(fy, cbind(1, fx), fd, method = m, maxiter = 1000L, tol = 1e-12)
    expect_equal(r$A, ref$est$fit$refvar, tolerance = 1e-10)
    expect_equal(r$beta, ref$est$fit$estcoef$beta, tolerance = 1e-10)
    expect_equal(r$eblup, as.vector(ref$est$eblup), tolerance = 1e-10)
    expect_equal(r$mse, ref$mse, tolerance = 1e-10)
    expect_equal(r$loglik, unname(ref$est$fit$goodness["loglike"]), tolerance = 1e-10)
  }
  skip_if_not_installed("metafor")
  tau2 <- metafor::rma(fy, fd, mods = ~fx, method = "REML", control = list(threshold = 1e-12, maxiter = 1000))$tau2
  expect_equal(FayHerriot(fy, cbind(1, fx), fd)$A, tau2, tolerance = 1e-8)
})

test_that("BhfEblup equals sae::eblupBHF (lmer) and nlme::lme", {
  skip_if_not_installed("sae")
  for (m in c("REML", "ML")) {
    ref <- sae::eblupBHF(uy ~ ux, dom = area, meanxpop = data.frame(area = 1:5, x = xbar),
                         popnsize = data.frame(area = 1:5, N = popn), method = m)
    r <- BhfEblup(uy, cbind(1, ux), area, cbind(1, xbar), areas = 1:5, popsize = popn, method = m)
    expect_equal(r$eblup, ref$eblup$eblup, tolerance = 1e-5)
    expect_equal(r$sigma2_u, ref$fit$refvar, tolerance = 1e-5)
    expect_equal(r$sigma2_e, ref$fit$errorvar, tolerance = 1e-5)
    expect_equal(r$loglik, ref$fit$loglike, tolerance = 1e-8)
  }
  skip_if_not_installed("nlme")
  fit <- nlme::lme(uy ~ ux, random = ~ 1 | area, method = "REML",
                   control = nlme::lmeControl(msTol = 1e-12, tolerance = 1e-12))
  r <- BhfEblup(uy, cbind(1, ux), area, cbind(1, xbar))
  expect_equal(r$sigma2_e, fit$sigma^2, tolerance = 1e-6)
  expect_equal(r$beta, unname(nlme::fixef(fit)), tolerance = 1e-6)
})

test_that("MarshallEb, PoissonGammaEb and PotthoffWhittinghill equal spdep, SpatialEpi and DCluster", {
  n <- c(3, 10, 4, 25, 7, 1)
  x <- c(800, 1500, 1000, 3000, 1200, 400)
  skip_if_not_installed("spdep")
  for (fam in c("poisson", "binomial")) {
    ref <- spdep::EBest(n, x, family = fam)
    expect_equal(MarshallEb(n, x, fam)$estimate, ref$estmm, tolerance = 1e-14)
  }
  skip_if_not_installed("SpatialEpi")
  Y <- c(25, 2, 40, 8, 1, 30, 4, 60, 3, 18)
  E <- c(10.2, 6.1, 14.5, 9.0, 4.8, 11.9, 10.5, 18.2, 7.7, 9.4)
  cv <- c(0.3, -0.2, 0.8, 0.1, -0.5, 0.4, 0.0, 1.1, -0.3, 0.2)
  ref <- SpatialEpi::eBayes(Y, E, cv)
  r <- PoissonGammaEb(Y, E, cv)
  expect_equal(r$alpha, ref$alpha, tolerance = 1e-6)
  expect_equal(r$beta, unname(ref$beta), tolerance = 1e-6)
  expect_equal(r$RR, ref$RR, tolerance = 1e-7)
  expect_equal(r$RRmed, ref$RRmed, tolerance = 1e-7)
  skip_if_not_installed("DCluster")
  ex <- sum(n) * x / sum(x)
  ref <- DCluster::pottwhitt.stat(data.frame(Observed = n, Expected = ex))
  r <- PotthoffWhittinghill(n, ex)
  expect_equal(c(r$T, r$mean, r$variance, r$p_value), c(ref$T, ref$asintmean, ref$asintvat, ref$pvalue),
               tolerance = 1e-14)
})
