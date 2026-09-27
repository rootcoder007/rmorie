test_that("ML factor analysis and retention criteria match factanal, MAP and the Python arm", {
  xs <- 99
  u <- function() { xs <<- (xs * 16807) %% 2147483647; xs / 2147483647 }
  z <- function() { a <- u(); b <- u(); sqrt(-2 * log(a)) * cos(2 * pi * b) }
  X <- t(vapply(1:300, function(i) {
    f1 <- z()
    f2 <- z()
    c(0.8 * f1 + 0.4 * z(), 0.7 * f1 + 0.5 * z(), 0.6 * f1 + 0.6 * z(), 0.8 * f2 + 0.4 * z(), 0.7 * f2 + 0.5 * z(), 0.5 * f1 + 0.5 * f2 + 0.5 * z())
  }, numeric(6)))
  f <- MlFac(X, 2)
  expect_equal(f$uniqueness, c(0.246269453065463, 0.260086455057108, 0.525380287526848, 0.206369465646447, 0.408501546403968, 0.343808068939422), tolerance = 1e-8)
  expect_equal(f$statistic, 3.37122551114991, tolerance = 1e-8)
  expect_equal(EfaNfactors(X, "map")$map_values, c(0.191906509332262, 0.176833764100459, 0.105901901182621, 0.233252358695946, 0.498113956813281), tolerance = 1e-12)
  expect_equal(EfaNfactors(X, "bic")$bic_values[1:2], c(263.279132684515, -19.4439043874749), tolerance = 1e-8)
  expect_equal(EfaNfactors(X, "parallel", nsim = 20, seed = 7)$threshold, c(1.2565893175643983, 1.1449133150508983, 1.085070600812775, 0.995374529325129, 0.9656585304195988, 0.8628327543397473), tolerance = 1e-12)
  for (m in c("map", "map4", "kaiser", "scree", "af", "variance", "aic", "bic")) expect_equal(EfaNfactors(X, m)$n_factors, 2)
  skip_if_not_installed("nFactors")
  ns <- nFactors::nScree(eigen(cor(X))$values)$Components
  expect_equal(c(EfaNfactors(X, "scree")$n_factors, EfaNfactors(X, "af")$n_factors), c(ns$noc, ns$naf))
})

test_that("parallel analysis retains no factors for pure noise", {
  xs <- 2024
  u <- function() { xs <<- (xs * 16807) %% 2147483647; xs / 2147483647 }
  N <- matrix(0, 120, 6)
  for (i in 1:120) for (j in 1:6) { a <- u(); b <- u(); N[i, j] <- sqrt(-2 * log(a)) * cos(2 * pi * b) }
  expect_equal(EfaNfactors(N, "parallel", nsim = 30, seed = 1)$n_factors, 0)
  expect_equal(EfaNfactors(N, "kaiser")$n_factors, 3)
})
