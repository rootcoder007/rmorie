# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/vdcal_native.R (Oie-Tozer steady-state volume of
# distribution). Vss = Vp(1 + Re/I) + fu Vp(Ve/Vp - Re/I) + Vr fu/fut
# is recomputed from the human physiological constants, and the inverse
# direction is checked to invert it exactly.

.vd_H <- list(Vp = 0.0436, Ve = 0.151, Vr = 0.380, Re_i = 1.4)
.vd_vss <- function(fu, fut, p = .vd_H) {
  p$Vp * (1 + p$Re_i) + fu * p$Vp * (p$Ve / p$Vp - p$Re_i) + p$Vr * fu / fut
}

test_that("oie_tozer splits Vss into plasma, extracellular and tissue terms", {
  r <- morie_vdcal_oie_tozer(0.1, 0.05)
  expect_equal(r$plasma, .vd_H$Vp * 2.4, tolerance = 1e-12)
  expect_equal(r$extra, 0.1 * .vd_H$Vp * (.vd_H$Ve / .vd_H$Vp - 1.4), tolerance = 1e-12)
  expect_equal(r$tissue, .vd_H$Vr * 2, tolerance = 1e-12)
  expect_equal(r$total, .vd_vss(0.1, 0.05), tolerance = 1e-12)
  # a drug confined to plasma (fu = fut) still carries the tissue-water term
  expect_equal(morie_vdcal_oie_tozer(1, 1)$total, .vd_vss(1, 1), tolerance = 1e-12)
  # stronger tissue binding (smaller fut) raises the volume
  expect_gt(morie_vdcal_oie_tozer(0.1, 0.01)$total, r$total)
  alt <- list(Vp = 0.05, Ve = 0.16, Vr = 0.4, Re_i = 1.2)
  expect_equal(morie_vdcal_oie_tozer(0.2, 0.1, par = alt)$total, .vd_vss(0.2, 0.1, alt),
               tolerance = 1e-12)
  expect_error(morie_vdcal_oie_tozer(0, 0.1), "plasma free fraction")
  expect_error(morie_vdcal_oie_tozer(1.5, 0.1), "plasma free fraction")
  expect_error(morie_vdcal_oie_tozer(0.1, 0), "tissue free fraction")
  expect_error(morie_vdcal_oie_tozer(0.1, 0.1, par = list(Vp = 0)), "Vp must be positive")
  expect_error(morie_vdcal_oie_tozer(0.1, 0.1, par = list(Re_i = -1)), "albumin ratio")
})

test_that("fut inverts the Oie-Tozer equation", {
  for (fu in c(0.02, 0.2, 0.9)) for (fut in c(0.01, 0.3)) {
    v <- .vd_vss(fu, fut)
    expect_equal(morie_vdcal_fut(v, fu), fut, tolerance = 1e-12)
  }
  alt <- list(Vp = 0.05, Ve = 0.16, Vr = 0.4, Re_i = 1.2)
  expect_equal(morie_vdcal_fut(.vd_vss(0.3, 0.2, alt), 0.3, par = alt), 0.2, tolerance = 1e-12)
  # a volume at or below the plasma plus extracellular terms is impossible
  base <- .vd_H$Vp * 2.4 + 0.1 * .vd_H$Vp * (.vd_H$Ve / .vd_H$Vp - 1.4)
  expect_error(morie_vdcal_fut(base - 1e-9, 0.1), "below what plasma and extracellular")
  # only just above it: the implied tissue free fraction exceeds one
  expect_error(morie_vdcal_fut(base + 1e-6, 0.1), "exceeds one")
  expect_error(morie_vdcal_fut(1, 0), "plasma free fraction")
})

test_that("morie_vdcal runs both directions and the descriptor route", {
  f <- morie_vdcal("CCO", 0.1, fut = 0.05, weight = 80)
  expect_equal(f$vss, .vd_vss(0.1, 0.05), tolerance = 1e-12)
  expect_equal(f$vss_litres, f$vss * 80, tolerance = 1e-12)
  expect_equal(f$binding_ratio, 2, tolerance = 1e-12)
  expect_identical(f$route, "given")
  expect_identical(f$direction, "vss")
  expect_equal(f$plasma_term + f$extracellular_term + f$tissue_term, f$vss, tolerance = 1e-12)
  inv <- morie_vdcal("CCO", 0.1, vss = f$vss, direction = "fut")
  expect_equal(inv$fut, 0.05, tolerance = 1e-12)
  expect_identical(inv$route, "inverse")
  # descriptors: fut = exp(-(a + b elogd + c fi + d log(1/fu)))
  co <- list(a = 0.5, b = 0.3, c = -0.2, d = 0.4)
  y <- 0.5 + 0.3 * 2 - 0.2 * 0.25 + 0.4 * log(1 / 0.1)
  d <- morie_vdcal("CCO", 0.1, elogd = 2, fi = 0.25, coefficients = co)
  expect_equal(d$fut, exp(-y), tolerance = 1e-12)
  expect_identical(d$route, "descriptors")
  expect_equal(d$vss, .vd_vss(0.1, exp(-y)), tolerance = 1e-12)
  expect_error(morie_vdcal("CCO", 0.1, direction = "clearance"), "direction must be one of")
  expect_error(morie_vdcal("CCO", 0.1, direction = "fut"), "needs a measured volume")
  expect_error(morie_vdcal("CCO", 0.1), "give a tissue free fraction")
  expect_error(morie_vdcal("CCO", 0.1, elogd = 2, fi = 0.25), "regression coefficients")
  expect_error(morie_vdcal("CCO", 0.1, elogd = 2, fi = 0.25, coefficients = list(a = -9)),
               "outside \\(0, 1\\]")
  expect_error(morie_vdcal("CCO", 0.1, fut = 0.05, weight = 0), "body mass must be positive")
})

test_that("morie_vdcal_cheatsheet quotes the human constants", {
  expect_match(morie_vdcal_cheatsheet(), "Vp 0.0436, Ve 0.151, Vr 0.380", fixed = TRUE)
})
