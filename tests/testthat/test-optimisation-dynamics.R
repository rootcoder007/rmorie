test_that("Cuttip reaches the integer optimum", {
  r <- Cuttip(c(3, 2), rbind(c(2, 3), c(4, 1), c(1, 1)), c(12, 10, 10))
  g <- expand.grid(0:10, 0:10)
  ok <- g[, 1] * 2 + g[, 2] * 3 <= 12 & g[, 1] * 4 + g[, 2] <= 10 & g[, 1] + g[, 2] <= 10
  expect_equal(r$objective, max(3 * g[ok, 1] + 2 * g[ok, 2]), tolerance = 1e-9)
  expect_equal(r$status, "optimal")
})

test_that("Socpts solves cone programs", {
  r <- Socpts(c(3, -4), list(diag(2)), list(c(0, 0)), list(list(c(0, 0), 1)))
  expect_equal(r$x, c(-0.6, 0.8), tolerance = 1e-8)
  w <- Socpts(c(1, 1), list(diag(2)), list(c(-5, -5)), list(list(c(0, 0), 1)))
  expect_equal(w$x, rep(5 - 1 / sqrt(2), 2), tolerance = 1e-8)
})

test_that("Sirepi RK4 and final size", {
  r <- Sirepi(990, 10, 0, 0.3, 0.1, 400)
  expect_equal(r$final_S, 990 * exp(-(3 / 1000) * (1000 - r$final_S)), tolerance = 1e-12)
  expect_equal(r$S[length(r$S)], r$final_S, tolerance = 1e-8)
  expect_equal(r$S + r$I + r$R, rep(1000, length(r$S)), tolerance = 1e-12)
})

test_that("Stsmod log-likelihood is the joint Gaussian density", {
  y <- c(1.1, 0.9, 1.4, 1.2, 1.6, 1.3)
  n <- length(y)
  S <- 10 + 0.2 * outer(0:(n - 1), 0:(n - 1), pmin) + diag(0.5, n)
  r <- Stsmod(y, 1, 1, 0.5, 0.2, 1, a1 = 0, P1 = 10)
  expect_equal(r$loglik, -0.5 * (n * log(2 * pi) + as.numeric(determinant(S)$modulus) + sum(y * solve(S, y))),
               tolerance = 1e-10)
  expect_equal(r$smoothed[[1]], sum(10 * solve(S, y)), tolerance = 1e-10)
})

test_that("front ends forward to the Geron implementations", {
  m <- function(src, prefix) if (length(prefix) %% 2 == 0) log(c(0.6, 0.4)) else log(c(0.3, 0.7))
  expect_identical(Gptas(m, "hi", 2, 3), morie_geron_beam_search(m, "hi", beam_width = 2, max_len = 3))
  expect_identical(names(formals(Ddpgc))[1:4], c("env", "actor", "critic", "tau"))
})
