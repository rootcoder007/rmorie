# Cross tests: GTWR and multiscale GWR against GWmodel; Gaussian HMM against HiddenMarkov.

make_st <- function() {
  i <- 0:39
  xy <- cbind((i * 7) %% 10 + 0.3 * sin(i), (i * 3) %% 8 + 0.2 * cos(i))
  tt <- i %% 5
  X <- cbind(x1 = sin(i * 0.7) + 0.1 * xy[, 1], x2 = cos(i * 1.1) * (1 + 0.05 * xy[, 2]))
  y <- 1 + (0.5 + 0.1 * xy[, 1]) * X[, 1] - (0.3 + 0.05 * xy[, 2] + 0.05 * tt) * X[, 2] + 0.2 * sin(i * 2.3)
  list(xy = xy, tt = tt, X = X, y = y)
}

test_that("GtwrFit equals GWmodel::gtwr and MgwrBackfit equals gwr.multiscale", {
  skip_if_not_installed("GWmodel")
  skip_if_not_installed("sp")
  s <- make_st()
  spdf <- sp::SpatialPointsDataFrame(s$xy, data.frame(y = s$y, s$X))
  ref <- suppressWarnings(GWmodel::gtwr(y ~ x1 + x2, spdf, obs.tv = s$tt, st.bw = 12, kernel = "bisquare", lamda = 0.4))
  got <- GtwrFit(s$y, s$X, s$xy, s$tt, 12, lam = 0.4, distance = "fotheringham", past_only = TRUE)
  expect_equal(got$beta, unname(as.matrix(as.data.frame(ref$SDF)[, 1:3])), tolerance = 1e-10)
  ref <- utils::capture.output(m <- GWmodel::gwr.multiscale(y ~ x1 + x2, spdf, kernel = "bisquare", bws0 = c(8, 15, 6),
                                                              bw.seled = rep(TRUE, 3), threshold = 1e-12,
                                                              max.iterations = 5000, hatmatrix = FALSE,
                                                              force.armadillo = TRUE))
  got <- MgwrBackfit(s$y, s$X, s$xy, c(8, 15, 6), threshold = 1e-12)
  expect_equal(got$beta, unname(as.matrix(as.data.frame(m$SDF)[, 1:3])), tolerance = 1e-8)
})

test_that("GaussianHmm equals HiddenMarkov::BaumWelch", {
  skip_if_not_installed("HiddenMarkov")
  i <- 0:59
  x <- sin(i * 0.9) + ifelse((i %/% 7) %% 2 == 1, 4, 0) + 0.3 * cos(i * i)
  got <- GaussianHmm(x, 2, max_iter = 5000, tol = 1e-13)
  q <- sort(x)
  q7 <- function(p) {
    h <- (length(q) - 1) * p
    lo <- floor(h)
    (1 - (h - lo)) * q[lo + 1] + (h - lo) * q[min(lo + 2, length(q))]
  }
  s0 <- sqrt(mean((x - mean(x))^2))
  mod <- HiddenMarkov::dthmm(x, Pi = matrix(c(0.9, 0.1, 0.1, 0.9), 2), delta = c(0.5, 0.5), distn = "norm",
                             pm = list(mean = c(q7(0.25), q7(0.75)), sd = c(s0, s0)))
  fit <- HiddenMarkov::BaumWelch(mod, HiddenMarkov::bwcontrol(maxiter = 5000, tol = 1e-13, prt = FALSE))
  expect_equal(got$means, fit$pm$mean, tolerance = 1e-8)
  expect_equal(got$sds, fit$pm$sd, tolerance = 1e-8)
  expect_equal(got$trans, unname(fit$Pi), tolerance = 1e-8)
  expect_equal(got$loglik, fit$LL, tolerance = 1e-10)
})
