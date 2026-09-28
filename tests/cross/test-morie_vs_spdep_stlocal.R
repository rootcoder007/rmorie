test_that("StGetisOrd equals spdep::localG and BivariateMoran equals moran_bv", {
  skip_if_not_installed("spdep")
  u <- .morie_random_uniform(200, seed = 21, stream = 0)
  P <- cbind(5 * u[1:20], 5 * u[21:40])
  Z <- matrix(u[41:120], 4, byrow = TRUE)
  Z[3:4, 1:5] <- Z[3:4, 1:5] + 1.5
  D <- as.matrix(dist(P))
  key <- expand.grid(i = 1:20, t = 1:4)
  nb <- lapply(seq_len(nrow(key)), function(r) {
    which(D[key$i[r], key$i] <= 1.5 & abs(key$t - key$t[r]) <= 1)
  })
  class(nb) <- "nb"
  attr(nb, "self.included") <- TRUE
  lw <- spdep::nb2listw(nb, style = "B")
  x <- as.vector(t(Z))
  lg <- spdep::localG(x, lw)
  g <- StGetisOrd(Z, P, 1.5, 1)
  expect_equal(as.vector(t(g$z)), as.vector(lg), tolerance = 1e-12)
  lw2 <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(P, 4)), style = "W")
  set.seed(1)
  mb <- spdep::moran_bv(u[121:140], u[141:160], lw2, nsim = 9)
  expect_equal(BivariateMoran(u[121:140], u[141:160], spdep::listw2mat(lw2), nsim = 9)$statistic, unname(mb$t0),
               tolerance = 1e-12)
  tt <- (0:19) %% 3
  zz <- u[161:180]
  r <- StTrendSurface(zz, P, tt, degree = 2, time_degree = 1)
  xc <- P[, 1] - mean(P[, 1])
  yc <- P[, 2] - mean(P[, 2])
  tc <- tt - mean(tt)
  f <- lm(zz ~ (xc + yc + I(xc^2) + I(xc * yc) + I(yc^2)) * tc)
  expect_equal(sum(r$residuals^2), sum(residuals(f)^2), tolerance = 1e-10)
})
