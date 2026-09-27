test_that("Ripk, RipG and the Strauss MPLE agree with spatstat", {
  skip_if_not_installed("spatstat.explore")
  skip_if_not_installed("spatstat.model")
  U <- .morie_random_uniform(160, seed = 31, stream = 0)
  P <- cbind(2 * U[seq(1, 159, 2)], U[seq(2, 160, 2)])
  W <- spatstat.geom::owin(c(0, 2), c(0, 1))
  X <- spatstat.geom::ppp(P[, 1], P[, 2], window = W)
  r <- seq(0, 0.2, by = 1e-4)
  at <- c(501, 1001, 2001)
  K <- as.data.frame(spatstat.explore::Kest(X, r = r, correction = c("iso", "border", "trans")))[at, ]
  k <- Ripk(P, c(0, 2, 0, 1), r[at])
  expect_lt(max(abs(k$k * 80 / 79 - K$iso)), 1e-9)
  expect_lt(max(abs(k$k_trans * 80 / 79 - K$trans)), 1e-9)
  expect_lt(max(abs(k$k_border - K$border)), 1e-9)
  G <- as.data.frame(spatstat.explore::Gest(X, r = r, correction = "rs"))[at, ]
  expect_lt(max(abs(RipG(P, c(0, 2, 0, 1), r[at])$g_border - G$rs)), 1e-9)
  g <- expand.grid(a = 0:11, b = 0:11)
  D <- spatstat.geom::ppp((g$a + 0.5) * 2 / 12, (g$b + 0.5) / 12, window = W)
  Q <- spatstat.geom::quadscheme(X, D, method = "grid", ntile = c(12, 12))
  fit <- spatstat.model::ppm(Q ~ 1, spatstat.model::Strauss(0.1), correction = "none")
  s <- morie_strmkr_strauss_process(P, 0.1, window = c(0, 2, 0, 1), nx = 12, ny = 12)
  expect_lt(max(abs(c(s$beta, s$gamma) - exp(stats::coef(fit)))), 1e-7)
})

test_that("KernelIntensity equals density.ppp at the points", {
  skip_if_not_installed("spatstat.explore")
  U <- .morie_random_uniform(100, seed = 33, stream = 0)
  P <- cbind(3 * U[1:50], U[51:100])
  X <- spatstat.geom::ppp(P[, 1], P[, 2], window = spatstat.geom::owin(c(0, 3), c(0, 1)))
  for (sg in c(0.1, 0.3)) {
    expect_lt(max(abs(KernelIntensity(P, c(0, 3, 0, 1), sg)$intensity -
                        spatstat.explore::density.ppp(X, sigma = sg, at = "points", edge = TRUE))), 1e-9)
    expect_lt(max(abs(KernelIntensity(P, c(0, 3, 0, 1), sg, correction = "diggle")$intensity -
                        spatstat.explore::density.ppp(X, sigma = sg, at = "points", edge = TRUE, diggle = TRUE))), 1e-9)
  }
})

test_that("PoissonProcessFit equals ppm on the same quadrature", {
  skip_if_not_installed("spatstat.model")
  U <- .morie_random_uniform(160, seed = 34, stream = 0)
  P <- cbind(2 * U[1:80], U[81:160])
  W <- spatstat.geom::owin(c(0, 2), c(0, 1))
  X <- spatstat.geom::ppp(P[, 1], P[, 2], window = W)
  g <- expand.grid(a = 0:11, b = 0:11)
  D <- spatstat.geom::ppp((g$a + 0.5) * 2 / 12, (g$b + 0.5) / 12, window = W)
  Q <- spatstat.geom::quadscheme(X, D, method = "grid", ntile = c(12, 12))
  fit <- spatstat.model::ppm(Q ~ x + y)
  r <- PoissonProcessFit(P, c(0, 2, 0, 1), degree = 1)
  expect_lt(max(abs(r$theta - stats::coef(fit))), 1e-8)
  expect_lt(max(abs(r$se - sqrt(diag(stats::vcov(fit))))), 1e-8)
})

test_that("NeymanScottProcess K and pcf equal spatstat cluster models", {
  skip_if_not_installed("spatstat.random")
  rr <- seq(0.01, 0.3, length.out = 12)
  for (kk in list(c("thomas", "Thomas"), c("cauchy", "Cauchy"), c("matern", "MatClust"))) {
    info <- spatstat.random::spatstatClusterModelInfo(kk[2])
    par <- switch(kk[1], thomas = c(7, 0.03^2), cauchy = c(7, 4 * 0.03^2), matern = c(7, 0.03))
    r <- NeymanScottProcess(7, 3, 0.03, kernel = kk[1], r = rr)
    expect_lt(max(abs(r$K - info$K(par, rr))), 1e-12)
    expect_lt(max(abs(r$pcf - info$pcf(par, rr))), 1e-10)
  }
})

test_that("GibbsPseudolikelihood equals ppm on the same quadrature", {
  skip_if_not_installed("spatstat.model")
  U <- .morie_random_uniform(140, seed = 37, stream = 0)
  P <- cbind(2 * U[1:70], U[71:140])
  W <- spatstat.geom::owin(c(0, 2), c(0, 1))
  X <- spatstat.geom::ppp(P[, 1], P[, 2], window = W)
  g <- expand.grid(a = 0:11, b = 0:11)
  D <- spatstat.geom::ppp((g$a + 0.5) * 2 / 12, (g$b + 0.5) / 12, window = W)
  Q <- spatstat.geom::quadscheme(X, D, method = "grid", ntile = c(12, 12))
  fits <- list(
    list(spatstat.model::Geyer(0.1, 2), list("geyer", r = 0.1, sat = 2)),
    list(spatstat.model::DiggleGratton(0.002, 0.1), list("diggle_gratton", delta = 0.002, rho = 0.1)),
    list(spatstat.model::Softcore(0.5), list("softcore", kappa = 0.5))
  )
  for (f in fits) {
    ref <- spatstat.model::ppm(Q ~ 1, f[[1]], correction = "none")
    m <- do.call(GibbsPseudolikelihood, c(list(P, c(0, 2, 0, 1)), f[[2]]))
    expect_lt(max(abs(m$theta - stats::coef(ref))), 1e-7)
  }
})
