# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: two-step floating catchment vs SpatialAcc::ac.

town <- function() {
  U <- .morie_random_uniform(2000, seed = 17, stream = 0)
  i <- 0:24
  j <- 0:5
  dem <- cbind(10 * U[2 * i + 1], 10 * U[2 * i + 2])
  sup <- cbind(10 * U[101 + 2 * j], 10 * U[102 + 2 * j])
  D <- outer(seq_len(25), seq_len(6), Vectorize(function(a, b) sqrt(sum((dem[a, ] - sup[b, ])^2))))
  grid <- matrix(as.numeric(U[501:620] < 0.55), 10, 12, byrow = TRUE)
  list(P = round(50 + 200 * U[201 + i]), S = round(5 + 20 * U[301 + j]), D = D, grid = grid)
}

test_that("2SFCA matches SpatialAcc::ac", {
  skip_if_not_installed("SpatialAcc")
  T <- town()
  expect_equal(SpatialAccessibility(T$P, T$S, T$D, d0 = 3)$accessibility,
               SpatialAcc::ac(T$P, T$S, T$D, d0 = 3, family = "2SFCA"), tolerance = 1e-12)
})
