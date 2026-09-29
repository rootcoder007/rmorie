# Coverage for DimeNet's directional message passing (Klicpera, Gross &
# Gunnemann 2020): the k-j-i angle, triplet counting, the radial Bessel
# basis sqrt(2/c) sin(n pi d / c) / d, the angular Legendre basis (against
# the three-term recurrence and closed forms), and one message-passing
# round in which m_ji aggregates interactions with every incoming m_kj.

test_that("angles, triplets and the Bessel basis", {
  expect_equal(morie_dimNet_angle_between(c(1, 0, 0), c(0, 0, 0), c(0, 2, 0)), pi / 2, tolerance = 1e-12)
  expect_equal(morie_dimNet(c(1, 1), c(0, 0), c(1, 0)), pi / 4, tolerance = 1e-12)
  expect_error(morie_dimNet_angle_between(1:2, 1:2, 3:4), "three distinct positions")
  adj <- list("1" = c(2, 3), "2" = c(1, 3), "3" = c(1, 2, 4), "4" = 3)
  tc <- morie_dimNet_triplet_count(adj)
  expect_identical(c(tc$pairs, tc$triplets), c(8, 2 + 2 + 6 + 0))
  b <- morie_dimNet_bessel_basis(1.3, cutoff = 4, n_basis = 5)
  expect_equal(b, sqrt(2 / 4) * sin((1:5) * pi * 1.3 / 4) / 1.3, tolerance = 1e-12)
  expect_error(morie_dimNet_bessel_basis(1, cutoff = 0), "cutoff must be positive")
  expect_error(morie_dimNet_bessel_basis(0), "distance must be positive")
})

test_that("the angular basis is P_0..P_{n-1}(cos angle)", {
  a <- 0.7
  x <- cos(a)
  P <- c(1, x, (3 * x^2 - 1) / 2, (5 * x^3 - 3 * x) / 2, (35 * x^4 - 30 * x^2 + 3) / 8)
  expect_equal(morie_dimNet_spherical_harmonic_basis(a, 5), P, tolerance = 1e-12)
  expect_equal(morie_dimNet_spherical_harmonic_basis(a, 1), 1)
  expect_equal(morie_dimNet_spherical_harmonic_basis(a, 2), P[1:2], tolerance = 1e-12)
  expect_error(morie_dimNet_spherical_harmonic_basis(a, 0), "at least one")
})

test_that("a message-passing round aggregates the incoming messages m_kj", {
  R <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(2, 1, 1))
  adj <- list("1" = 2, "2" = c(1, 3), "3" = c(2, 4), "4" = 3)
  msgs <- list("1->2" = c(1, 0), "2->1" = c(0, 1), "2->3" = c(2, 1), "3->2" = c(1, 1),
               "3->4" = c(0.5, 0.5), "4->3" = c(1, -1))
  interact <- function(m, rbf, sbf) m * (rbf[1] + sbf[2])
  update <- function(m, a) m + a
  r <- morie_dimNet_directional_message_pass(msgs, adj, R, interact, update, cutoff = 5, n_rbf = 2, n_sbf = 3)
  # m_{2->3}: incoming k -> 2 with k != 3 is only 1 -> 2
  d <- sqrt(sum((R[1, ] - R[2, ])^2))
  ang <- morie_dimNet_angle_between(R[1, ], R[2, ], R[3, ])
  ref <- msgs[["2->3"]] + msgs[["1->2"]] * (morie_dimNet_bessel_basis(d, 5, 2)[1] + cos(ang))
  expect_equal(r$messages[["2->3"]], ref, tolerance = 1e-12)
  # m_{3->2}: incoming 4 -> 3
  d4 <- sqrt(sum((R[4, ] - R[3, ])^2))
  a4 <- morie_dimNet_angle_between(R[4, ], R[3, ], R[2, ])
  expect_equal(r$messages[["3->2"]], msgs[["3->2"]] + msgs[["4->3"]] * (morie_dimNet_bessel_basis(d4, 5, 2)[1] + cos(a4)), tolerance = 1e-12)
  # 1 has no other neighbour, so m_{1->2} only passes through update
  expect_equal(r$messages[["1->2"]], msgs[["1->2"]])
  expect_identical(r$n_messages, 6L)
  expect_identical(r$triplets, morie_dimNet_triplet_count(adj)$triplets)
  expect_error(morie_dimNet_directional_message_pass(msgs[-1], adj, R, interact, update), "no message 1->2")
})
