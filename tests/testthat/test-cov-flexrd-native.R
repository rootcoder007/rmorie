# Coverage for induced-fit docking: Rodrigues rotation about a bond axis
# (against the axis-angle rotation matrix), chi-angle moves of side-chain
# atoms, the 12-6 Lennard-Jones score with radius scaling and cutoff, and
# the two-stage search (soft ranking of poses, side-chain refinement by
# exhaustive grid or coordinate descent, hard rescoring).

.rotm <- function(k, t) {
  K <- rbind(c(0, -k[3], k[2]), c(k[3], 0, -k[1]), c(-k[2], k[1], 0))
  diag(3) + sin(t) * K + (1 - cos(t)) * K %*% K
}
.lj <- function(rec, lig, rr, lr, s = 1, eps = 1, cut = 8) {
  e <- 0
  for (i in seq_along(lig)) for (j in seq_along(rec)) {
    r <- sqrt(sum((lig[[i]] - rec[[j]])^2))
    if (r > cut) next
    q <- (lr[i] + rr[j]) * s / r
    e <- e + 4 * eps * (q^12 - q^6)
  }
  e
}

test_that("Rodrigues rotation equals the axis-angle rotation matrix", {
  a <- c(1, 0, 0)
  b <- c(1, 2, 2)
  p <- c(0.5, 1, -1)
  k <- (b - a) / sqrt(8)
  expect_equal(morie_flexrd_rotate(p, a, b, 75), a + as.numeric(.rotm(k, 75 * pi / 180) %*% (p - a)), tolerance = 1e-12)
  expect_equal(morie_flexrd_rotate(p, a, b, 360), p, tolerance = 1e-12)
  expect_error(morie_flexrd_rotate(p, a, a, 10), "two distinct atoms")
  co <- list(c(0, 0, 0), c(0, 0, 1), c(1, 0, 1), c(1, 1, 2))
  ch <- morie_flexrd_chi(co, list(a = 0, b = 1, moves = c(2, 3)), 90)
  expect_equal(ch[[3]], c(0, 1, 1), tolerance = 1e-12)
  expect_equal(ch[[4]], morie_flexrd_rotate(co[[4]], co[[1]], co[[2]], 90), tolerance = 1e-12)
  expect_identical(ch[1:2], co[1:2])
})

test_that("the energy is a scaled 12-6 Lennard-Jones sum within the cutoff", {
  rec <- list(c(0, 0, 0), c(3, 0, 0), c(20, 0, 0))
  lig <- list(c(0, 2.5, 0), c(3, 3, 1))
  e <- morie_flexrd_energy(rec, lig, c(1.2, 1, 1), c(1, 0.8), scale = 0.9, epsilon = 0.3, cutoff = 8)
  expect_equal(e, .lj(rec, lig, c(1.2, 1, 1), c(1, 0.8), 0.9, 0.3, 8), tolerance = 1e-12)
  expect_error(morie_flexrd_energy(rec, list(c(0, 0, 0)), 1:3, 1), "on top of each other")
})

.setup <- function() {
  rec <- list(coords = list(c(0, 0, 0), c(0, 0, 1.5), c(1.4, 0, 2), c(4, 4, 4)), radii = c(1, 1, 1, 1))
  lig <- list(poses = list(list(c(2.4, 0.2, 2.2)), list(c(0, 3, 0)), list(c(-3, 0, 1))), radii = 0.9)
  chis <- list(list(a = 0, b = 1, moves = 2))
  list(rec = rec, lig = lig, chis = chis)
}

test_that("the exhaustive search picks the best chi on the grid, then rescores hard", {
  s <- .setup()
  r <- morie_flexrd(s$rec, s$lig, s$chis, search = "exhaustive", n_keep = 3)
  soft <- vapply(s$lig$poses, function(p) .lj(s$rec$coords, p, s$rec$radii, 0.9, 0.7), 1)
  expect_equal(r$stage1, soft, tolerance = 1e-12)
  best <- Inf
  for (pi_ in 1:3) {
    ang <- c(0, -60, 60, 180)
    es <- vapply(ang, function(a) {
      rc <- morie_flexrd_chi(s$rec$coords, s$chis[[1]], a)
      .lj(rc, s$lig$poses[[pi_]], s$rec$radii, 0.9, 0.7)
    }, 1)
    rc <- morie_flexrd_chi(s$rec$coords, s$chis[[1]], ang[which.min(es)])
    hard <- .lj(rc, s$lig$poses[[pi_]], s$rec$radii, 0.9, 1)
    if (hard < best) {
      best <- hard
      arg <- c(pi_ - 1, ang[which.min(es)])
    }
  }
  expect_equal(r$energy, best, tolerance = 1e-12)
  expect_equal(c(r$pose_index, r$chi), arg)
  expect_equal(r$gain, r$rigid_energy - r$energy, tolerance = 1e-12)
  c1 <- morie_flexrd(s$rec, s$lig, s$chis, n_keep = 1)
  expect_identical(c1$kept, as.integer(order(soft)[1] - 1))
  expect_lte(c1$energy_soft, c1$rigid_energy_soft + 1e-12)
  n0 <- morie_flexrd(s$rec, list(coords = list(c(3, 3, 0)), radii = 1), list())
  expect_equal(n0$energy, .lj(s$rec$coords, list(c(3, 3, 0)), s$rec$radii, 1), tolerance = 1e-12)
  expect_identical(n0$n_chi, 0L)
  expect_error(morie_flexrd(s$rec, s$lig, s$chis, search = "anneal"), "coordinate or exhaustive")
  expect_error(morie_flexrd(list(coords = s$rec$coords, radii = 1), s$lig, s$chis), "one radius per receptor atom")
  expect_error(morie_flexrd(s$rec, s$lig, list(list(a = 0, b = 0, moves = 2))), "two distinct atoms")
  expect_error(morie_flexrd(s$rec, s$lig, list(list(a = 0))), "needs a, b and moves")
  expect_match(morie_flexrd_cheatsheet(), "Rodrigues")
})
