# Rangayyan batch A in R, against the same book equations as the Python
# arm.  Expected values are closed forms of the density being integrated
# or recomputed here from the printed equation.

gauss <- function(mu = 0, sd = 1) {
  cst <- 1 / (sd * sqrt(2 * pi))
  function(v) cst * exp(-0.5 * ((v - mu) / sd)^2)
}
unif01 <- function(v) if (v >= 0 && v <= 1) 1 else 0
UGRID <- seq(0, 1, length.out = 4001)

test_that("PdfMean implements eq (3.1)", {
  r <- PdfMean(gauss(2, 1.5), lower = 2 - 18, upper = 2 + 18)
  expect_equal(r$mean, 2, tolerance = 1e-7)
  expect_equal(r$pdf_mass, 1, tolerance = 1e-7)
  expect_equal(PdfMean(unif01, x = UGRID)$mean, 0.5, tolerance = 1e-9)
})

test_that("PdfMS is the second moment, not the second central one", {
  r <- PdfMS(gauss(2, 1.5), lower = 2 - 18, upper = 2 + 18)
  expect_equal(r$ms, 6.25, tolerance = 1e-6)             # sigma^2 + mu^2
  expect_equal(r$variance_from_identity, 2.25, tolerance = 1e-6)
})

test_that("PdfVar implements eq (3.3) and the CV caveat", {
  r <- PdfVar(unif01, x = UGRID)
  expect_equal(r$variance, 1 / 12, tolerance = 1e-8)
  expect_equal(r$cv, sqrt(1 / 12) / 0.5, tolerance = 1e-7)
  g <- seq(-3, 3, by = 0.001)
  expect_null(PdfVar(gauss(0, 1), x = g)$cv)
})

test_that("PdfSkew implements eq (3.4)", {
  g <- seq(-8, 8, by = 0.001)
  expect_equal(PdfSkew(gauss(0, 1), x = g)$skewness, 0, tolerance = 1e-8)
  ge <- seq(0, 60, by = 0.002)
  expect_equal(PdfSkew(function(v) if (v >= 0) exp(-v) else 0,
                       x = ge)$skewness, 2, tolerance = 1e-4)
})

test_that("PdfKurt implements eq (3.5) and its excess", {
  g <- seq(-10, 10, by = 0.001)
  r <- PdfKurt(gauss(0, 1), x = g)
  expect_equal(r$kurtosis, 3, tolerance = 1e-6)
  expect_equal(r$excess, 0, tolerance = 1e-6)
  u <- PdfKurt(unif01, x = UGRID)
  expect_equal(u$kurtosis, 1.8, tolerance = 1e-6)
})

test_that("DiffEnt implements eq (3.6) and may be negative", {
  sdv <- 2
  want <- 0.5 * log2(2 * pi * exp(1) * sdv^2)
  g <- seq(-24, 24, by = 0.002)
  expect_equal(DiffEnt(gauss(0, sdv), x = g)$entropy, want, tolerance = 1e-6)
  g2 <- seq(0, 0.5, length.out = 4001)
  expect_equal(DiffEnt(function(v) if (v >= 0 && v <= 0.5) 2 else 0,
                       x = g2)$entropy, -1, tolerance = 1e-9)
})

test_that("Smean and Srms implement eqs (3.7)-(3.10) with divisor N", {
  expect_equal(Smean(c(1, 2, 6))$mean, 3)
  expect_error(Smean(numeric(0)), "at least one")
  r <- Srms(c(3, 4))
  expect_equal(r$ms, 12.5)
  expect_equal(r$rms, sqrt(12.5))
  expect_equal(r$sd, 0.5)          # N, not N-1
  expect_equal(r$ddof, 0L)
})

test_that("Shannon implements eq (3.11)", {
  expect_equal(Shannon(rep(0.25, 4))$entropy, 2)
  expect_equal(Shannon(rep(0.25, 4))$max_entropy, 2)
  expect_equal(Shannon(c(1, 0, 0))$entropy, 0)
  expect_equal(Shannon(c(2, 2, 2, 2))$entropy, 2)
  q <- Shannon(c(0, 0.1, 1, 1.1, 2, 2.1, 3, 3.1), levels = 4)
  expect_equal(q$entropy, 2)
  expect_equal(q$levels, 4L)
})

test_that("NoiseModel implements eqs (3.12)-(3.14) without assuming 3.14", {
  r <- NoiseModel(c(1, 2, 3, 4), c(0.5, -0.5, 0.5, -0.5))
  expect_equal(r$y, c(1.5, 1.5, 3.5, 3.5))
  expect_equal(r$mean_observed, r$mean_additive)
  o <- NoiseModel(c(1, -1, 1, -1), c(1, 1, -1, -1))
  expect_equal(o$covariance, 0)
  expect_equal(o$variance_observed, o$variance_additive)
  d <- NoiseModel(c(1, 2, 3, 4), c(1, 2, 3, 4))
  expect_equal(d$correlation, 1)
  expect_equal(d$variance_observed, 2 * d$variance_additive)
  expect_error(NoiseModel(c(1, 2), 1), "same length")
})

test_that("MeanSum implements eq (3.13)", {
  r <- MeanSum(c(1, 3), c(10, 20), 0.5)
  expect_equal(r$mean, 17.5)
  expect_equal(unname(r$component_means), c(2, 15, 0.5))
})

test_that("EnsMean implements eq (3.15) with a 1/sqrt(M) SE", {
  r <- EnsMean(list(c(1, 10), c(3, 20), c(5, 30)), index = 2)
  expect_equal(r$mean, 20)
  expect_equal(r$m, 3L)
  sd <- sqrt((100 + 0 + 100) / 3)
  expect_equal(r$se, sd / sqrt(3))
  expect_error(EnsMean(list(1, 2), index = 5), "outside")
})

test_that("EnsAvg implements eq (3.18) and rejects ragged records", {
  r <- EnsAvg(list(c(0, 2, 4), c(2, 4, 6)))
  expect_equal(r$average, c(1, 3, 5))
  expect_error(EnsAvg(list(c(1, 2), 1)), "same length")
})

test_that("CovXY implements eqs (3.21)-(3.22)", {
  r <- CovXY(c(1, 2, 3, 4), c(2, 4, 6, 8))
  expect_equal(r$covariance, (1.5 * 3 + 0.5 + 0.5 + 1.5 * 3) / 4)
  expect_equal(r$correlation, 1)
  a <- CovXY(c(1, 2, 3, 4), c(4, 1, 3, 2))
  b <- CovXY(c(1, 2, 3, 4), c(4, 1, 3, 2), ddof = 1)
  expect_equal(b$covariance, a$covariance * 4 / 3)
  expect_null(CovXY(c(1, 1, 1), c(1, 2, 3))$correlation)
})

test_that("DiracDelta is undefined at the origin (eq 3.24)", {
  r <- DiracDelta(c(-1, 0, 1))
  expect_true(is.na(r$delta[2]))
  expect_equal(r$delta[c(1, 3)], c(0, 0))
  w <- DiracDelta(c(-0.2, 0, 0.2, 1), width = 0.25)
  expect_equal(w$height, 4)
  expect_equal(w$delta[4], 0)
})

test_that("DeltaArea checks eq (3.25) at any width", {
  for (w in c(2, 0.5, 0.05)) {
    expect_equal(DeltaArea(width = w)$area, 1, tolerance = 1e-12)
  }
  g <- seq(-1, 1, by = 0.01)
  expect_true(DeltaArea(t = g, values = ifelse(abs(g) <= 1, 0.5, 0))$unit_area)
  expect_false(DeltaArea(t = g, values = ifelse(abs(g) <= 1, 1, 0))$unit_area)
})

test_that("DeltaLim matches eq (3.26) and diverges at zero", {
  a <- 0.4
  expect_equal(DeltaLim(c(0.5, 1, 2.5), a)$values,
               0.5 * a * abs(c(0.5, 1, 2.5))^(a - 1))
  expect_true(is.na(DeltaLim(0, 0.4)$values[1]))
  areas <- vapply(c(0.8, 0.4, 0.2, 0.05),
                  function(aa) DeltaLim(c(-3, 3), aa)$area_symmetric,
                  numeric(1))
  expect_equal(areas, sort(areas, decreasing = TRUE))
  expect_equal(areas[4], 3^0.05)
})

test_that("the two step definitions disagree at the origin", {
  expect_equal(Ustep(c(-1, 0, 1e-12, 1))$u, c(0, 0, 1, 1))   # eq 3.27
  expect_equal(StepSeq(-2:2)$u, c(0, 0, 1, 1, 1))            # eq 3.35
  expect_equal(Ustep(0)$u, 0)
  expect_equal(StepSeq(0L)$u, 1)
})

test_that("Sifting implements eq (3.28) with strict limits", {
  expect_equal(Sifting(function(t) t^2 + 1, 2, 0, 5)$value, 5)
  expect_equal(Sifting(function(t) 7, 9, 0, 5)$value, 0)
  expect_equal(Sifting(function(t) 7, 5, 0, 5)$value, 0)
  expect_equal(Sifting(function(t) 7, 0, 0, 5)$value, 0)
})

test_that("DeltaDecomp weights sum to the integral (eq 3.29)", {
  r <- DeltaDecomp(c(1, 2, 3, 4), c(0, 0.5, 1, 1.5))
  expect_equal(r$reconstruction_error, 0, tolerance = 1e-12)
  expect_equal(r$total_weight, r$integral, tolerance = 1e-12)
})

test_that("ContConv carries the dt of eq (3.30)", {
  plain <- c(1, 3, 2)
  expect_equal(ContConv(c(1, 2), c(1, 1), dt = 0.5)$y, 0.5 * plain)
  expect_equal(ContConv(1, c(2, -1, 0.5))$y, c(2, -1, 0.5))
})

test_that("ContConvAlt commutes with ContConv (eq 3.31)", {
  a <- ContConv(c(1, -2, 3, 0.5), c(0.25, 0.5, 0.25), dt = 0.1)
  b <- ContConvAlt(c(1, -2, 3, 0.5), c(0.25, 0.5, 0.25), dt = 0.1)
  expect_equal(b$y, a$y, tolerance = 1e-14)
  expect_true(b$commutes)
})

test_that("KDelta and StepSeq implement eqs (3.34)-(3.35)", {
  expect_equal(KDelta(5L)$delta, c(1, 0, 0, 0, 0))
  expect_equal(KDelta(5L, shift = 2, amplitude = 1.5)$delta,
               c(0, 0, 1.5, 0, 0))
  expect_equal(StepSeq(-2:3, shift = 1)$first_difference,
               c(0, 0, 0, 1, 0, 0))
})

test_that("RampFilt implements eq (3.42) with the stated normalization", {
  r <- RampFilt()
  expect_equal(r$n_taps, 501L)
  expect_equal(r$h[1], 2.5)
  expect_equal(r$h[501], 0, tolerance = 1e-12)
  expect_equal(r$gain, 626.25)
  expect_equal(sum(r$h_normalized), 1, tolerance = 1e-12)
  f <- RampFilt(rep(5, 2000))
  expect_equal(f$y[2000], 5, tolerance = 1e-9)
  expect_error(RampFilt(fs = 0), "positive")
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_ch3_sample_mean(c(1, 2, 6))$mean, 3)
  expect_equal(morie_ch3_discrete_unit_step(0L)$u, 1)
})
