test_that("MuZero edge values include the edge reward: Q = r + gamma V(child)", {
  rep <- function(o) 0
  dyn <- function(s, a) list(if (identical(a, "L")) 1 else 0, s + 1)
  pred <- function(s) list(p = c(0.5, 0.5), v = 0)
  g <- 0.997
  r1 <- morie_muzero(NULL, c("L", "R"), rep, dyn, pred, simulations = 1, gamma = g)
  expect_equal(r1$visits$L, 1L)
  expect_equal(r1$Q$L, 1)
  # second simulation: L then L again; V(L) = (0 + 1) / 2
  r2 <- morie_muzero(NULL, c("L", "R"), rep, dyn, pred, simulations = 2, gamma = g)
  expect_equal(r2$Q$L, 1 + g / 2, tolerance = 1e-15)
  r3 <- morie_muzero(NULL, c("L", "R"), rep, dyn, pred, simulations = 30)
  expect_equal(r3$visits$L + r3$visits$R, 30L)
  expect_equal(r3$value, (r3$visits$L * r3$Q$L + r3$visits$R * r3$Q$R) / 30,
               tolerance = 1e-14)
})
