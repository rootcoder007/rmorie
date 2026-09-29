# Coverage tests for rfppos_native.R: the covalent-pose geometry helpers,
# the warhead search and the filter itself. Every value is recomputed in
# the test from base R vector algebra.

cross3 <- function(a, b) {
  c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
}

test_that("distance is the Euclidean norm of the difference", {
  a <- c(1.5, -2.0, 0.25)
  b <- c(-0.5, 1.0, 4.25)
  expect_equal(morie_rfppos_distance(a, b), sqrt(sum((a - b)^2)), tolerance = 1e-12)
  expect_equal(morie_rfppos_distance(a, a), 0)
})

test_that("angle at the middle point is the arc cosine of the normalised dot product", {
  a <- c(1, 0, 0)
  b <- c(0, 0, 0)
  c <- c(0, 1, 0)
  expect_equal(morie_rfppos_angle(a, b, c), 90, tolerance = 1e-12)
  a <- c(2.0, 1.0, -0.5)
  b <- c(0.5, 0.5, 0.5)
  c <- c(-1.0, 3.0, 2.0)
  u <- a - b
  v <- c - b
  ref <- acos(sum(u * v) / sqrt(sum(u * u) * sum(v * v))) * 180 / pi
  expect_equal(morie_rfppos_angle(a, b, c), ref, tolerance = 1e-12)
  # collinear points clamp the cosine instead of returning NaN
  expect_equal(morie_rfppos_angle(c(2, 0, 0), c(0, 0, 0), c(1, 0, 0)), 0, tolerance = 1e-12)
  expect_error(morie_rfppos_angle(a, a, c), "three distinct points")
})

test_that("dihedral follows the IUPAC sign convention", {
  # a at +x, d at +y, axis b->c along +z: clockwise when viewed along
  # the axis, so the torsion is +90 (Wikipedia / IUPAC convention).
  a <- c(1, 0, 0)
  b <- c(0, 0, 0)
  c <- c(0, 0, 1)
  d <- c(0, 1, 1)
  expect_equal(morie_rfppos_dihedral(a, b, c, d), 90, tolerance = 1e-12)
  expect_equal(morie_rfppos_dihedral(d, c, b, a), 90, tolerance = 1e-12)
  expect_equal(morie_rfppos_dihedral(a, b, c, c(1, 0, 1)), 0, tolerance = 1e-12)
  expect_equal(morie_rfppos_dihedral(a, b, c, c(-1, 0, 1)), 180, tolerance = 1e-12)
  # a general quadruple against atan2(|b2| b1 . (b2 x b3), (b1 x b2) . (b2 x b3))
  a <- c(0.3, -1.2, 2.0)
  b <- c(1.1, 0.4, 0.7)
  c <- c(2.5, 0.9, -0.3)
  d <- c(3.0, -1.5, 0.2)
  b1 <- b - a
  b2 <- c - b
  b3 <- d - c
  ref <- atan2(sqrt(sum(b2 * b2)) * sum(b1 * cross3(b2, b3)),
               sum(cross3(b1, b2) * cross3(b2, b3))) * 180 / pi
  expect_equal(morie_rfppos_dihedral(a, b, c, d), ref, tolerance = 1e-12)
  expect_error(morie_rfppos_dihedral(a, b, b, d), "defined axis")
})

test_that("warhead search finds the electrophilic carbon from bond orders", {
  # acrylamide C=CC(=O)N: Michael acceptor (beta, alpha, carbonyl) and a
  # Buergi-Dunitz carbonyl carbon (C, O, third neighbour)
  bd <- morie_rfppos_warhead("C=CC(=O)N", "burgi_dunitz")
  expect_equal(bd, c(2, 3, 4))  # carbonyl C, its O, the amide N as the third neighbour
  mi <- morie_rfppos_warhead("C=CC(=O)N", "michael")
  expect_equal(mi, c(0, 1, 2))
  expect_null(morie_rfppos_warhead("CCO", "michael"))
  expect_null(morie_rfppos_warhead("CCO", "burgi_dunitz"))
  expect_error(morie_rfppos_warhead("C=O", "sn2"), "burgi_dunitz or michael")
})

test_that("the filter judges distance and attack angle against the ideal", {
  # methanal-like ligand C=O with the electrophile at the origin, the
  # carbonyl oxygen along +x; sulfur placed 2.0 A away at the
  # Buergi-Dunitz angle from the C=O bond in the xy plane
  coords <- list(c(0, 0, 0), c(1.2, 0, 0))
  th <- 107 * pi / 180
  sg <- 2.0 * c(cos(th), sin(th), 0)
  cys <- list(SG = sg, CB = sg + c(0, 0, 1.5))
  r <- morie_rfppos(list(smiles = "C=O", coords = coords), cys, warhead = c(0, 1, 1))
  expect_true(r$passes)
  expect_equal(r$distance, 2.0, tolerance = 1e-12)
  expect_equal(r$angle, 107, tolerance = 1e-9)
  expect_equal(r$angle_error, 107 - r$ideal, tolerance = 1e-9)
  expect_equal(r$dihedral, morie_rfppos_dihedral(cys$CB, sg, coords[[1]], coords[[2]]))
  expect_equal(r$warhead_torsion,
               morie_rfppos_dihedral(sg, coords[[1]], coords[[2]], coords[[2]]))
  expect_equal(r$n_atoms, 2)
  # the same pose at 4 A fails on distance, at 60 degrees on the angle
  far <- morie_rfppos(list(smiles = "C=O", coords = coords),
                      list(SG = 4.0 * c(cos(th), sin(th), 0)), warhead = c(0, 1, 1))
  expect_false(far$distance_ok)
  expect_match(far$reason, "too far")
  expect_null(far$dihedral)
  bent <- morie_rfppos(list(smiles = "C=O", coords = coords),
                       list(SG = 2.0 * c(cos(pi / 3), sin(pi / 3), 0)), warhead = c(0, 1, 1))
  expect_true(bent$distance_ok)
  expect_false(bent$angle_ok)
  expect_match(bent$reason, "wrong angle")
  # no warhead: an explicit reason and no geometry
  none <- morie_rfppos(list(smiles = "CCO", coords = list(c(0, 0, 0), c(1.5, 0, 0), c(2, 1, 0))),
                       cys, mode = "michael")
  expect_false(none$passes)
  expect_null(none$distance)
  expect_error(morie_rfppos(list(smiles = "C=O", coords = coords[1]), cys), "one coordinate triple")
  expect_error(morie_rfppos(list(smiles = "C=O", coords = coords), cys, mode = "sn2"),
               "burgi_dunitz or michael")
})

test_that("the cheatsheet names the three geometric criteria", {
  s <- morie_rfppos_cheatsheet()
  expect_type(s, "character")
  expect_match(s, "distance")
  expect_match(s, "torsion")
})
