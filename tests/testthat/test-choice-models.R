panel_data <- function() {
  U <- .morie_random_uniform(2000, seed = 6, stream = 0)
  Z <- .morie_random_normal(2000, seed = 6, stream = 1)
  y <- integer(0)
  X <- matrix(0, 0, 2)
  g <- integer(0)
  for (i in 0:39) {
    a <- 0.8 * Z[i + 1]
    for (t in 0:4) {
      x1 <- Z[101 + i * 5 + t]
      x2 <- U[501 + i * 5 + t]
      eta <- -0.3 + a + 0.9 * x1 - 1.2 * x2
      y <- c(y, as.integer(U[1001 + i * 5 + t] < plogis(eta)))
      X <- rbind(X, c(x1, x2))
      g <- c(g, i + 1L)
    }
  }
  list(y = y, X = X, g = g)
}

test_that("PanelBinaryChoice matches clogit and glmer values and the Python arm", {
  D <- panel_data()
  r <- PanelBinaryChoice(D$y, D$X, D$g)
  expect_equal(r$coef, c(0.769142, -1.662757), tolerance = 1e-5)
  expect_equal(r$loglik, -60.672473, tolerance = 1e-6)
  r <- PanelBinaryChoice(D$y, D$X, D$g, model = "re_logit")
  expect_equal(r$coef, c(0.015379, 0.832846, -1.962072), tolerance = 1e-4)
  expect_lt(r$sigma, 1e-3)
})

test_that("PanelBinaryChoice matches glmer with a large random effect", {
  U <- .morie_random_uniform(2000, seed = 7, stream = 0)
  Z <- .morie_random_normal(2000, seed = 7, stream = 1)
  i <- rep(0:49, each = 6)
  k <- 201 + i * 6 + rep(0:5, 50)
  x1 <- Z[k]
  y <- as.integer(U[k + 800] < plogis(-0.2 + 1.5 * Z[i + 1] + 0.8 * x1))
  r <- PanelBinaryChoice(y, cbind(x1), i + 1L, model = "re_logit", n_quad = 40)
  expect_equal(r$coef, c(-0.1811396, 0.8595103), tolerance = 1e-4)
  expect_lt(abs(r$sigma - 0.9047192), 2e-5)
  expect_lt(abs(r$loglik + 183.276772), 1e-5)
})

test_that("BlissPoints recovers ideal points", {
  Z <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(2, 1), c(0.5, 2))
  Rt <- rbind(5 - 2 * colSums((t(Z) - c(0.7, 0.4))^2), 1 + colSums((t(Z) - c(1, 1))^2))
  b <- BlissPoints(Rt, Z)
  expect_equal(b$ideal[[1]], c(0.7, 0.4), tolerance = 1e-12)
  expect_identical(b$anti_ideal, c(FALSE, TRUE))
})
