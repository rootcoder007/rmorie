# Coverage for GoldScore (Jones et al. 1997; Verdonk et al. 2003): the
# generalised m-n Lennard-Jones well eps (m q^n - n q^m) / (n - m) with its
# minimum -eps at r0, the split inner/outer potentials, the van der Waals
# sum with combining rules r0 = r_i + r_j and eps = sqrt(eps_i eps_j),
# H-bond and torsion terms, and the assembled fitness with the 1.375
# external weight.

.lj <- function(r, r0, e, m, n) e * (m * (r0 / r)^n - n * (r0 / r)^m) / (n - m)

test_that("m-n Lennard-Jones has its minimum -eps at r0", {
  expect_equal(morie_goldsc_lj(3.1, 3.5, 0.2), .lj(3.1, 3.5, 0.2, 6, 12), tolerance = 1e-12)
  expect_equal(morie_goldsc_lj(3.5, 3.5, 0.2, 4, 8), -0.2, tolerance = 1e-12)
  expect_equal(morie_goldsc_lj(5, 3.5, 0.2, 4, 8), .lj(5, 3.5, 0.2, 4, 8), tolerance = 1e-12)
  expect_identical(morie_goldsc_lj(0, 3.5, 0.2), Inf)
  expect_error(morie_goldsc_lj(1, 1, 1, 6, 6), "must exceed")
  # split potential: softer inner branch below r0, 4-8 above
  expect_equal(morie_goldsc_split(2.5, 3.5, 0.2), .lj(2.5, 3.5, 0.2, 2, 4), tolerance = 1e-12)
  expect_equal(morie_goldsc_split(4, 3.5, 0.2), .lj(4, 3.5, 0.2, 4, 8), tolerance = 1e-12)
  expect_equal(morie_goldsc_split(3.5, 3.5, 0.2, inner = c(1, 2)), -0.2, tolerance = 1e-12)
})

.radii <- list(list("C", 1.9), list("O", 1.6), list("N", 1.8))
.depths <- list(list("C", 0.1), list("O", 0.2), list("N", 0.15))

test_that("vdW sums pair energies with combining rules and a cutoff", {
  pairs <- list(list(3.2, "C", "O"), list(4.1, "N", "C"), list(9, "O", "O"))
  v <- morie_goldsc_vdw(pairs, .radii, .depths, potential = "6-12", cutoff = 8)
  ref <- c(.lj(3.2, 3.5, sqrt(0.02), 6, 12), .lj(4.1, 3.7, sqrt(0.015), 6, 12))
  expect_equal(v$terms, ref, tolerance = 1e-12)
  expect_equal(v$total, sum(ref), tolerance = 1e-12)
  expect_identical(v$n, 2L)
  s <- morie_goldsc_vdw(pairs[1], .radii, .depths, potential = "split_1-2")
  expect_equal(s$total, .lj(3.2, 3.5, sqrt(0.02), 1, 2), tolerance = 1e-12)
  expect_identical(morie_goldsc_vdw(list(), .radii, .depths)$total, 0)
  expect_error(morie_goldsc_vdw(list(list(3, "S", "C")), .radii, .depths), "no radius for atom type S")
  expect_error(morie_goldsc_vdw(pairs, .radii, .depths, potential = "9-6"), "potential must be one of")
})

test_that("H-bond terms inside the distance limit and cosine torsions", {
  h <- morie_goldsc_hbond(list(list(1.9, -2.5), list(2.6, -1), list(2.2, -0.7)))
  expect_equal(h$terms, c(-2.5, -0.7))
  expect_equal(h$total, -3.2, tolerance = 1e-12)
  expect_identical(morie_goldsc_hbond(list())$total, 0)
  tt <- morie_goldsc_torsion(list(c(60, 1.5, 3, 0), c(180, 0.8, 2, pi)))
  expect_equal(tt$terms, c(1.5 * (1 + cos(pi)), 0.8 * (1 + cos(2 * pi - pi))), tolerance = 1e-12)
  expect_equal(tt$total, sum(tt$terms), tolerance = 1e-12)
})

test_that("fitness is minus (hbond + 1.375 vdW_ext + vdW_int + torsion)", {
  rec <- list(list(0, 0, 0, "O"), list(3, 0, 0, "N"))
  lig <- list(list(0, 3.4, 0, "C"), list(3, 3, 1, "O"))
  internal <- list(list(3.8, "C", "O"))
  hb <- list(list(2, -1.2))
  tor <- list(c(30, 0.5, 3, 0))
  g <- morie_goldsc(rec, lig, .radii, .depths, hbonds = hb, internal = internal, torsions = tor)
  d <- function(a, b) sqrt(sum((unlist(a[1:3]) - unlist(b[1:3]))^2))
  ext <- 0
  for (a in rec) for (b in lig) {
    r0 <- c(C = 1.9, O = 1.6, N = 1.8)[[a[[4]]]] + c(C = 1.9, O = 1.6, N = 1.8)[[b[[4]]]]
    e <- sqrt(c(C = 0.1, O = 0.2, N = 0.15)[[a[[4]]]] * c(C = 0.1, O = 0.2, N = 0.15)[[b[[4]]]])
    ext <- ext + .lj(d(a, b), r0, e, 4, 8)
  }
  int <- .lj(3.8, 3.5, sqrt(0.02), 6, 12)
  tq <- 0.5 * (1 + cos(3 * 30 * pi / 180))
  expect_equal(g$vdw_external, ext, tolerance = 1e-12)
  expect_equal(g$vdw_internal, int, tolerance = 1e-12)
  expect_equal(g$energy, -1.2 + 1.375 * ext + int + tq, tolerance = 1e-12)
  expect_equal(g$fitness, -g$energy)
  expect_identical(c(g$n_external, g$n_internal, g$n_hbond), c(4L, 1L, 1L))
  expect_equal(morie_goldsc(rec, lig, .radii, .depths, vdw_weight = 1)$energy, ext, tolerance = 1e-12)
  expect_match(morie_goldsc_cheatsheet(), "1.375")
})
