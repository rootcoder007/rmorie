skip_if_not_installed("splm")
skip_if_not_installed("spdep")
skip_if_not_installed("plm")

set.seed(8)
N <- 12
T <- 5
xy <- cbind(stats::runif(N), stats::runif(N))
lw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(xy, k = 3), sym = TRUE), style = "W")
W <- spdep::listw2mat(lw)
dat <- data.frame(id = rep(1:N, T), time = rep(1:T, each = N), x1 = stats::rnorm(N * T), x2 = stats::runif(N * T))
mu <- stats::rnorm(N)
dat$y <- as.vector(kronecker(diag(T), solve(diag(N) - 0.4 * W)) %*% (1 + dat$x1 - 0.5 * dat$x2 + rep(mu, T) +
                                                                        stats::rnorm(N * T)))
X <- as.matrix(dat[, c("x1", "x2")])

test_that("within lag, error and SAC panels match splm::spml", {
  a <- splm::spml(y ~ x1 + x2, data = dat, index = c("id", "time"), listw = lw, model = "within", lag = TRUE,
                  spatial.error = "none")
  expect_equal(sppsar(dat$y, X, W, dat$time, dat$id)$rho, unname(a$coefficients[1]), tolerance = 1e-5)
  b <- splm::spml(y ~ x1 + x2, data = dat, index = c("id", "time"), listw = lw, model = "within", lag = FALSE,
                  spatial.error = "b")
  expect_equal(sppsem(dat$y, X, W, dat$time, dat$id)$rho, unname(b$spat.coef), tolerance = 1e-5)
  s <- splm::spml(y ~ x1 + x2, data = dat, index = c("id", "time"), listw = lw, model = "within", lag = TRUE,
                  spatial.error = "b")
  r <- sppsac(dat$y, X, W, dat$time, dat$id)
  expect_equal(c(r$rho, r$lambda), unname(s$spat.coef), tolerance = 1e-6)
})

test_that("pooled LM tests match splm::slmtest", {
  for (tt in c("lme", "lml", "rlme", "rlml")) {
    ref <- splm::slmtest(y ~ x1 + x2, data = dat, listw = lw, test = tt, model = "pooling")
    got <- sppdiag(dat$y, X, W, dat$time, dat$id)
    key <- c(lme = "RSerr", lml = "RSlag", rlme = "adjRSerr", rlml = "adjRSlag")[[tt]]
    expect_equal(got[[key]], as.numeric(ref$statistic), tolerance = 1e-8)
  }
})
