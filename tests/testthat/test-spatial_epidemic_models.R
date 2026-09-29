test_that("spatial_sir conserves patches and matches one RK4 step", {
  W <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
  N <- c(1000, 800, 1200, 500)
  r <- spatial_sir(c(5, 0, 0, 0), W, N, beta = 0.5, gamma = 0.2, t_max = 40, dt = 0.25)
  expect_equal(r$S[[161]] + r$I[[161]] + r$R[[161]], N, tolerance = 1e-12)
  iso <- spatial_sir(c(5, 0, 0, 0), NULL, N, beta = 0.5, gamma = 0.2, t_max = 40, dt = 0.25)
  expect_equal(iso$R[[161]][3], 0)
  C <- 0.9 * diag(4) + 0.1 * W / rowSums(W)
  f <- function(y) {
    S <- y[1:4]
    I <- y[5:8]
    l <- as.vector(C %*% (I / N))
    c(-0.5 * S * l, 0.5 * S * l - 0.2 * I, 0.2 * I)
  }
  y0 <- c(N - c(5, 0, 0, 0), c(5, 0, 0, 0), rep(0, 4))
  h <- 0.25
  k1 <- f(y0)
  k2 <- f(y0 + h / 2 * k1)
  k3 <- f(y0 + h / 2 * k2)
  k4 <- f(y0 + h * k3)
  y1 <- y0 + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  s <- spatial_sir(c(5, 0, 0, 0), W, N, beta = 0.5, gamma = 0.2, t_max = h, dt = h)
  expect_equal(c(s$S[[2]], s$I[[2]], s$R[[2]]), y1, tolerance = 1e-12)
})

test_that("penalised spatial frailty and cure models solve their score equations", {
  W <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
  i <- 0:29
  reg <- i %% 3
  x <- matrix(sin(i))
  t <- 0.5 + ((i * 7) %% 11) / 5 * exp(-0.5 * x[, 1] - 0.3 * reg)
  ev <- as.numeric(i %% 5 != 0)
  r <- spatial_frailty(t, ev, x, reg, W)
  eta <- x[, 1] * r$coefficients + r$frailties[reg + 1]
  H <- t^r$shape * exp(eta)
  expect_lt(abs(sum((ev - H) * x[, 1])), 1e-7)
  Q <- diag(rowSums(W)) - W
  gu <- vapply(0:2, function(k) sum((ev - H)[reg == k]), 0) - as.vector(Q %*% r$frailties)
  expect_lt(max(abs(gu)), 1e-7)
  j <- 0:35
  x2 <- matrix(sin(j))
  t2 <- ifelse(j %% 4 == 0, 5, 0.2 + ((j * 7) %% 11) / 8)
  ev2 <- as.numeric(!(j %% 4 == 0 | j %% 9 == 1))
  cr <- spatial_cure_rate(t2, ev2, x2, j %% 3, W)
  th <- exp(x2[, 1] * cr$coefficients + cr$regional_effects[j %% 3 + 1])
  G <- exp(cr$log_scale + cr$shape * log(t2))
  expect_lt(abs(sum((ev2 - th * (1 - exp(-G))) * x2[, 1])), 1e-7)
  expect_equal(cr$cure_fraction, exp(-th), tolerance = 1e-12)
})
