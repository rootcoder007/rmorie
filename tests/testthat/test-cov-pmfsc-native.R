# Knowledge-based PMF scoring (Muegge & Martin 1999; Muegge 2001):
# shell volumes, radial bins, A(r) = -kT ln(f rho(r) / rho_bulk) derived
# from contact counts, capped unobserved bins, and pose scoring.

pm_obs <- list(list("N", "O", 2.9), list("N", "O", 3.1), list("N", "O", 3.2),
               list("N", "O", 5.5), list("C", "C", 4.1), list("C", "C", 4.4),
               list("N", "O", 13))

test_that("shells and bins", {
  expect_equal(morie_pmfsc_shell(1, 2), 4 / 3 * pi * 7, tolerance = 1e-15)
  expect_error(morie_pmfsc_shell(2, 1), "outer radius")
  expect_identical(morie_pmfsc_bin(3.1, 12, 24), 6L)
  expect_identical(morie_pmfsc_bin(0, 12, 24), 0L)
  expect_identical(morie_pmfsc_bin(12, 12, 24), -1L)
  expect_error(morie_pmfsc_bin(-1, 12, 24), "negative")
})

test_that("the derived potential inverts the Boltzmann relation per bin", {
  pot <- morie_pmfsc_derive(pm_obs, n_complexes = 2, r_max = 6, n_bins = 6, kT = 0.6, cap = 5)
  e <- 0:6
  vol <- 4 / 3 * pi * (e[-1]^3 - e[-7]^3)
  cnt <- c(0, 0, 1, 2, 0, 1)
  dens <- cnt / (2 * vol)
  ref <- 4 / (2 * 4 / 3 * pi * 216)
  a <- ifelse(dens > 0, -0.6 * log(dens / ref), 0.6 * 5)
  no <- pot[["N|O"]]
  expect_identical(no$counts, as.integer(cnt))
  expect_equal(no$potential, a, tolerance = 1e-14)
  expect_identical(no$capped, 3L)
  expect_identical(names(pot), c("C|C", "N|O"))
  un <- morie_pmfsc_derive(pm_obs, 2, 6, 6, reference = "uniform")
  expect_equal(un[["N|O"]]$reference, 1 / (4 / 3 * pi * 216), tolerance = 1e-15)
  occ <- rep(1, 6)
  ev <- morie_pmfsc_derive(pm_obs, 2, 6, 6, correction = "excluded_volume", occupied = occ)
  expect_equal(ev[["N|O"]]$correction, vol / (vol - 1), tolerance = 1e-15)
  expect_error(morie_pmfsc_derive(pm_obs, correction = "excluded_volume"), "occupied volumes")
  expect_error(morie_pmfsc_derive(pm_obs, 2, 6, 6, correction = "excluded_volume",
                                  occupied = c(10, rep(0, 5))), "entirely occupied")
  expect_error(morie_pmfsc_derive(pm_obs, reference = "ideal"), "reference must be")
  expect_error(morie_pmfsc_derive(pm_obs, correction = "x"), "correction must be")
  expect_error(morie_pmfsc_derive(pm_obs, n_bins = 0), "radial bin")
  expect_error(morie_pmfsc_derive(pm_obs, n_complexes = 0), "at least one complex")
})

test_that("a pose scores as the sum of binned pair potentials", {
  pot <- morie_pmfsc_derive(pm_obs, 2, 6, 6)
  pairs <- list(list("N", "O", 2.5), list("C", "C", 4.2), list("S", "O", 3), list("N", "O", 7))
  s <- morie_pmfsc_score(pairs, pot, r_max = 6, n_bins = 6, missing = 0.5)
  expect_equal(s$score, pot[["N|O"]]$potential[3] + pot[["C|C"]]$potential[5] + 0.5,
               tolerance = 1e-15)
  expect_identical(c(s$used, s$beyond, s$unknown), c(2L, 1L, 1L))
  rec <- list(list(0, 0, 0, "N"), list(0, 0, 4.2, "C"))
  lig <- list(list(0, 0, 2.5, "O"), list(0, 0, 0, "C"))
  r <- morie_pmfsc(rec, lig, potential = pot, r_max = 6, n_bins = 6)
  d <- c(2.5, 0, 1.7, 4.2)
  types <- c("N|O", "N|C", "C|O", "C|C")
  ref <- sum(vapply(1:4, function(k) {
    p <- pot[[types[k]]]
    if (is.null(p)) 0 else p$potential[morie_pmfsc_bin(d[k], 6, 6) + 1]
  }, 1))
  expect_equal(r$score, ref, tolerance = 1e-14)
  expect_identical(r$n_pairs, 4L)
  r2 <- morie_pmfsc(rec, lig, observations = pm_obs, n_complexes = 2, r_max = 6, n_bins = 6)
  expect_equal(r2$score, r$score, tolerance = 1e-15)
  expect_error(morie_pmfsc(rec, lig), "derived potential or the observations")
  expect_match(morie_pmfsc_cheatsheet(), "derived, not shipped", fixed = TRUE)
})
