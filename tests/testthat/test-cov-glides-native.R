# Coverage tests for R/glides_native.R (Friesner et al. 2004, Glide-style
# empirical scoring): vdW, Coulomb, lipophilic and H-bond terms and the
# assembled GScore from coordinates.

test_that("generalised Lennard-Jones and Coulomb terms", {
  pr <- list(list(3.5, 3.8, 0.2), list(4.2, 3.6, 0.15))
  v <- morie_glides_vdw(pr)
  lj <- function(r, s, e) e * ((6 / 6) * (s / r)^12 - (12 / 6) * (s / r)^6)
  expect_equal(v$terms, c(lj(3.5, 3.8, 0.2), lj(4.2, 3.6, 0.15)), tolerance = 1e-12)
  expect_equal(morie_glides_vdw(list(list(3.8, 3.8, 0.2)))$total, -0.2, tolerance = 1e-12)
  v84 <- morie_glides_vdw(pr, m = 4, n = 8)
  expect_equal(v84$terms[1], 0.2 * ((4 / 4) * (3.8 / 3.5)^8 - (8 / 4) * (3.8 / 3.5)^4), tolerance = 1e-12)
  expect_equal(morie_glides_vdw(list(list(0, 1, 1)))$terms, Inf)
  cp <- list(list(3, 0.4, -0.5), list(5, -0.2, -0.3))
  expect_equal(morie_glides_coulomb(cp)$total, 332.0637 * (0.4 * -0.5 / 3 + -0.2 * -0.3 / 5), tolerance = 1e-12)
  expect_equal(morie_glides_coulomb(cp, "distance", 4)$terms, 332.0637 * c(-0.2 / 36, 0.06 / 100), tolerance = 1e-12)
  expect_error(morie_glides_coulomb(cp, "debye"), "dielectric must be")
  expect_error(morie_glides_coulomb(cp, epsilon = 0), "positive")
})

test_that("lipophilic ramp and hydrogen-bond classes", {
  l <- morie_glides_lipo(c(3, 5.6, 8))
  expect_equal(l$terms, c(1, 0.5, 0))
  expect_error(morie_glides_lipo(1, r1 = 5, r2 = 4), "outer radius")
  h <- morie_glides_hbond(list(list("neutral_neutral", 0.8), list("charged_charged", 0.5)),
    weights = list(hbond_charged_charged = 2))
  expect_equal(h$terms, c(0.8, 1))
  expect_equal(h$by_class$charged_charged, 1)
  expect_equal(h$total, 1.8)
  expect_error(morie_glides_hbond(list(list("ionic", 1))), "class must be")
})

test_that("GScore assembly and the coordinate front end", {
  s <- morie_glides_score(vdw = -10, coulomb = -4, lipo = 3, hbond = 2, metal = 1, rotb = 0.7,
    weights = list(lipo = -0.5, hbond = -1))
  expect_equal(s$total, 0.065 * -10 + 0.130 * -4 - 1.5 - 2 + 1 + 0.7, tolerance = 1e-12)
  expect_equal(morie_glides_score(vdw = 1, coefficients = list(vdw = 1))$total, 1)
  rec <- list(list(0, 0, 0, "C"), list(4, 0, 0, "O"))
  lig <- list(list(0, 3.5, 0, "C"), list(4, 4, 0, "N"))
  radii <- list(list("C", 1.9), list("O", 1.7), list("N", 1.8))
  depths <- list(list("C", 0.1), list("O", 0.2), list("N", 0.16))
  charges <- list(list("C", 0), list("O", -0.5), list("N", 0.3))
  g <- morie_glides(rec, lig, radii, depths, charges, lipophilic = "C", n_rot = 2)
  d <- function(a, b) sqrt(sum((a - b)^2))
  R <- list(c(0, 0, 0), c(4, 0, 0))
  L <- list(c(0, 3.5, 0), c(4, 4, 0))
  rt <- c("C", "O")
  lt <- c("C", "N")
  rad <- c(C = 1.9, O = 1.7, N = 1.8)
  dep <- c(C = 0.1, O = 0.2, N = 0.16)
  chg <- c(C = 0, O = -0.5, N = 0.3)
  vp <- list()
  cpairs <- list()
  for (i in 1:2) for (j in 1:2) {
    r <- d(R[[i]], L[[j]])
    vp[[length(vp) + 1]] <- list(r, rad[[rt[i]]] + rad[[lt[j]]], sqrt(dep[[rt[i]]] * dep[[lt[j]]]))
    cpairs[[length(cpairs) + 1]] <- list(r, chg[[rt[i]]], chg[[lt[j]]])
  }
  expect_equal(g$vdw_energy, morie_glides_vdw(vp)$total, tolerance = 1e-12)
  expect_equal(g$coulomb_energy, morie_glides_coulomb(cpairs)$total, tolerance = 1e-12)
  expect_equal(g$lipophilic_count, morie_glides_lipo(3.5)$total)
  expect_equal(g$gscore, 0.065 * g$vdw_energy + 0.130 * g$coulomb_energy + g$lipophilic_count + 0.7, tolerance = 1e-12)
  expect_equal(morie_glides(rec, lig, radii, depths, charges, cutoff = 4)$n_contacts, 2L)
  expect_error(morie_glides(rec, lig, radii[1:2], depths, charges), "no radius for atom type N")
  expect_match(morie_glides_cheatsheet(), "GScore")
})
