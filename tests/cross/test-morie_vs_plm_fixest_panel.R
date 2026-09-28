test_that("panel diagnostics equal plm, and the fixest Conley covariance", {
  skip_if_not_installed("plm")
  skip_if_not_installed("fixest")
  skip_if_not_installed("nlme")
  suppressPackageStartupMessages(library(plm)) # pcdtest and pbltest evaluate plm() unqualified
  data("Grunfeld", package = "plm", envir = environment())
  g <- Grunfeld[order(Grunfeld$firm, Grunfeld$year), ]
  X <- as.matrix(g[, c("value", "capital")])
  f <- inv ~ value + capital
  rh <- PanelResiduals(g$inv, X, g$firm, "heterogeneous")$residuals
  rw <- PanelResiduals(g$inv, X, g$firm, "within")$residuals
  rp <- PanelResiduals(g$inv, X, g$firm, "pooling")$residuals
  for (tst in c("cd", "lm", "sclm", "rho", "absrho")) {
    expect_equal(CrossSectionDependence(rh, g$firm, g$year, tst)$statistic,
                 unname(plm::pcdtest(f, data = g, test = tst)$statistic), tolerance = 1e-10)
  }
  expect_equal(CrossSectionDependence(rw, g$firm, g$year)$statistic,
               unname(plm::pcdtest(f, data = g, model = "within")$statistic), tolerance = 1e-10)
  expect_equal(CrossSectionDependence(rw, g$firm, g$year, "bcsclm")$statistic,
               unname(plm::pcdtest(f, data = g, model = "within", test = "bcsclm")$statistic), tolerance = 1e-10)
  expect_equal(UnobservedEffectsTest(rp, g$firm)$statistic, unname(plm::pwtest(f, data = g)$statistic),
               tolerance = 1e-10)
  w <- plm::plm(f, data = g, model = "within")
  expect_equal(PanelSerialTest(g$inv, X, g$firm, 2)$statistic, unname(plm::pbgtest(w, order = 2)$statistic),
               tolerance = 1e-10)
  expect_equal(PanelSerialTest(g$inv, X, g$firm, 3, "F")$statistic,
               unname(plm::pbgtest(w, order = 3, type = "F")$statistic), tolerance = 1e-10)
  expect_equal(unlist(PanelVarianceComponents(rp, g$firm)[c(1, 3)]),
               unname(plm::ercomp(f, g, method = "walhus")$sigma2), tolerance = 1e-10, ignore_attr = TRUE)
  xm <- colMeans(X)
  am <- g$inv - mean(g$inv) - as.vector(sweep(X, 2, xm) %*% stats::coef(w))
  expect_equal(unlist(PanelVarianceComponents(am, g$firm)[c(1, 3)]),
               unname(plm::ercomp(f, g, method = "amemiya")$sigma2), tolerance = 1e-10, ignore_attr = TRUE)
  data("Produc", package = "plm", envir = environment())
  p <- Produc[order(Produc$state, Produc$year), ]
  Xp <- cbind(log(p$pcap), log(p$pc), log(p$emp), p$unemp)
  fp <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
  for (alt in c("onesided", "twosided")) {
    expect_equal(BaltagiLiTest(log(p$gsp), Xp, p$state, alt)$statistic,
                 unname(plm::pbltest(fp, data = p, alternative = alt)$statistic), tolerance = 1e-6)
  }
  set.seed(3)
  n <- 80
  dd <- data.frame(lat = 40 + runif(n) * 3, lon = -80 + runif(n) * 4, x = rnorm(n), z = runif(n))
  dd$y <- 1 + 2 * dd$x - dd$z + rnorm(n)
  m <- fixest::feols(y ~ x + z, dd)
  for (dist in c("triangular", "spherical")) {
    for (cut in c(40, 90)) {
      v <- suppressWarnings(stats::vcov(m, vcov = fixest::vcov_conley(lat = "lat", lon = "lon", cutoff = cut,
                                                                     distance = dist, vcov_fix = FALSE)))
      ours <- ConleyVcov(cbind(dd$x, dd$z), stats::resid(m), dd$lat, dd$lon, cut, distance = paste0("fixest_", dist))
      expect_equal(unclass(v), ours$vcov, tolerance = 1e-10, ignore_attr = TRUE)
    }
  }
})
