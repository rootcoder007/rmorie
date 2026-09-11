# HBV rainfall-runoff (Seibert & Vis 2012, Eqs. 1-6).
# Anchors: the MAXBAS unit hydrograph must integrate to 1; each routine is
# checked against its closed form with the other routines switched off; and
# mass balance (corrected input = evaporation + runoff + storage change)
# must close to machine precision.

pars <- function(...) {
  a <- list(tt = 0, cfmax = 3, cfr = 0.05, fc = 200, lp = 0.7, beta = 2,
            perc = 1.5, uzl = 20, k0 = 0.3, k1 = 0.1, k2 = 0.02, maxbas = 1)
  m <- list(...)
  a[names(m)] <- m
  a
}

# Parameters that isolate the snow routine: soil passes everything through and
# the upper box empties completely each day, so q_gw equals the soil input.
snow_only <- function(...) {
  pars(fc = 1e-9, lp = 1e-9, beta = 1, perc = 0,
       k0 = 0, k1 = 1, k2 = 0, uzl = 1e9, ...)
}

test_that("MAXBAS weights are a unit hydrograph", {
  for (mb in c(1, 2, 3, 4, 5, 5.5, 7, 10.25)) {
    w <- as.numeric(unlist(.hbvMod_maxbas_weights(mb)))
    expect_equal(sum(w), 1)
    expect_true(all(w >= 0))
    expect_equal(length(w), ceiling(mb))
  }
})

test_that("integer MAXBAS gives a symmetric triangle", {
  for (mb in c(3, 4, 5, 7)) {
    w <- as.numeric(unlist(.hbvMod_maxbas_weights(mb)))
    expect_equal(w, rev(w))
    # the peak sits in the middle, the tails are the smallest weights
    expect_equal(which.max(w), ceiling(mb / 2))
  }
  expect_equal(as.numeric(unlist(.hbvMod_maxbas_weights(1))), 1)
  expect_equal(as.numeric(unlist(.hbvMod_maxbas_weights(2))), c(0.5, 0.5))
})

test_that("precipitation phase splits at TT", {
  # at or below TT everything accumulates as snow, above TT none does
  expect_equal(morie_hbvMod(10, -0.01, 0, snow_only())$snow, 10)
  expect_equal(morie_hbvMod(10, 0, 0, snow_only())$snow, 10)
  expect_equal(morie_hbvMod(10, 0.01, 0, snow_only())$snow, 0)
  # TT is honoured as a parameter, not assumed to be zero
  expect_equal(morie_hbvMod(10, 1.5, 0, snow_only(tt = 2))$snow, 10)
  expect_equal(morie_hbvMod(10, 2.5, 0, snow_only(tt = 2))$snow, 0)
})

test_that("degree-day melt matches the closed form (Eq. 1)", {
  # 20 days of 10 mm snowfall at -5C, then melt at +5C.
  # With CWH = 0 the pack releases CFMAX * (T - TT) per day with no retention.
  cfmax <- 3
  p <- c(rep(10, 20), rep(0, 20))
  tm <- c(rep(-5, 20), rep(5, 20))
  r <- morie_hbvMod(p, tm, rep(0, 40), snow_only(cwh = 0, cfmax = cfmax))
  expect_equal(max(r$snow), 200)
  melt <- cfmax * (5 - 0)
  expect_equal(r$q_gw[21:30], rep(melt, 10))
  # the pack draws down linearly and is exhausted when its water is gone
  expect_equal(r$snow[21:30], 200 - melt * seq_len(10))
  expect_equal(sum(r$q_gw), 200)
})

test_that("melt is capped by the available snowpack", {
  # 10 mm of snow cannot yield 30 mm of melt
  r <- morie_hbvMod(c(10, 0), c(-5, 10), c(0, 0), snow_only(cwh = 0, cfmax = 3))
  expect_equal(r$q_gw[2], 10)
  expect_equal(r$snow[2], 0)
})

test_that("SFCF corrects snowfall and leaves rain alone", {
  # snowfall correction factor: the pack receives SFCF * P (Table 1)
  for (s in c(0.8, 1, 1.2, 1.5)) {
    expect_equal(morie_hbvMod(100, -5, 0, snow_only(sfcf = s))$snow, 100 * s)
  }
  # rain is not corrected
  expect_equal(morie_hbvMod(100, 5, 0, snow_only(sfcf = 1.5))$q_gw, 100)
  # default is 1, i.e. no correction
  expect_equal(morie_hbvMod(100, -5, 0, snow_only())$snow, 100)
  expect_equal(morie_hbvMod(1, 1, 1, pars())$params_used$sfcf, 1)
})

test_that("CWH retains meltwater in the pack", {
  p <- c(rep(10, 20), rep(0, 10))
  tm <- c(rep(-5, 20), rep(5, 10))
  # with no retention the release is immediate and flat
  r0 <- morie_hbvMod(p, tm, rep(0, 30), snow_only(cwh = 0))
  expect_equal(r0$q_gw[21], 15)
  # with retention the first melt day yields nothing: 15 mm of melt is held
  # below the 0.1 * 185 = 18.5 mm capacity
  r1 <- morie_hbvMod(p, tm, rep(0, 30), snow_only(cwh = 0.1))
  expect_equal(r1$q_gw[21], 0)
  expect_equal(r1$snow[21], 200)
  # a larger capacity delays release for longer, and total release is smaller
  # while more water is still locked up in the pack
  r3 <- morie_hbvMod(p, tm, rep(0, 30), snow_only(cwh = 0.3))
  expect_equal(r3$q_gw[21:23], c(0, 0, 0))
  expect_true(sum(r3$q_gw) < sum(r1$q_gw))
  expect_true(sum(r1$q_gw) < sum(r0$q_gw))
  expect_equal(morie_hbvMod(1, 1, 1, pars())$params_used$cwh, 0.1)
})

test_that("refreezing returns liquid water to the pack (Eq. 2)", {
  # melt at +5C, then refreeze at -4C: CFR * CFMAX * (TT - T) = 0.05*3*4 = 0.6
  cfr <- 0.05
  cfmax <- 3
  r <- morie_hbvMod(c(100, 0, 0), c(-5, 5, -4), c(0, 0, 0),
                    snow_only(cfr = cfr, cfmax = cfmax, cwh = 0.5))
  # day 2 melts 15 mm, all retained (capacity 0.5 * 85 = 42.5)
  expect_equal(r$q_gw[2], 0)
  # the refrozen water moves from liquid to solid, so pack total is unchanged
  expect_equal(r$snow[3], r$snow[2])
  # and no liquid leaves the pack on a freezing day
  expect_equal(r$q_gw[3], 0)
  # refreezing is capped by the liquid water present
  r2 <- morie_hbvMod(c(100, 0), c(-5, -5), c(0, 0), snow_only(cfr = 10))
  expect_equal(r2$snow[2], 100)
})

test_that("soil recharge follows the power law (Eq. 3)", {
  # recharge = insoil * (S_soil / FC)^BETA, evaluated at the start of the step
  fc <- 200
  for (beta in c(1, 2, 3)) {
    s0 <- 100
    insoil <- 20
    r <- morie_hbvMod(insoil, 10, 0,
                      pars(fc = fc, beta = beta, perc = 0, k0 = 0, k1 = 1,
                           k2 = 0, uzl = 1e9),
                      init = list(soil = s0))
    expect_equal(r$q_gw, insoil * (s0 / fc)^beta)
    expect_equal(r$soil, s0 + insoil - insoil * (s0 / fc)^beta)
  }
  # a dry soil absorbs everything
  expect_equal(morie_hbvMod(20, 10, 0, pars(perc = 0, k1 = 1, k0 = 0))$q_gw, 0)
})

test_that("soil moisture cannot exceed FC; the excess becomes recharge", {
  r <- morie_hbvMod(500, 10, 0,
                    pars(fc = 200, beta = 1, perc = 0, k0 = 0, k1 = 1,
                         k2 = 0, uzl = 1e9))
  expect_equal(r$soil, 200)
  expect_equal(r$q_gw, 300)
})

test_that("actual evaporation follows Eq. 4", {
  fc <- 200
  lp <- 0.5
  ep <- 5
  # above the threshold FC * LP evaporation runs at the potential rate
  r <- morie_hbvMod(0, 10, ep, pars(fc = fc, lp = lp, perc = 0),
                    init = list(soil = 150))
  expect_equal(r$e_act, ep)
  # below it evaporation is scaled linearly by S_soil / (FC * LP)
  for (s0 in c(10, 40, 90)) {
    r <- morie_hbvMod(0, 10, ep, pars(fc = fc, lp = lp, perc = 0),
                      init = list(soil = s0))
    expect_equal(r$e_act, ep * s0 / (fc * lp))
    expect_equal(r$soil, s0 - r$e_act)
  }
  # and is capped by the water actually in the soil
  r <- morie_hbvMod(0, 10, 1000, pars(fc = fc, lp = lp, perc = 0),
                    init = list(soil = 7))
  expect_equal(r$e_act, 7)
  expect_equal(r$soil, 0)
})

test_that("groundwater recession matches the analytic solution (Eq. 5)", {
  # with K0 disabled and no inflow, SUZ decays geometrically at rate K1
  k1 <- 0.2
  suz0 <- 100
  n <- 8
  r <- morie_hbvMod(rep(0, n), rep(10, n), rep(0, n),
                    pars(k0 = 0, k1 = k1, k2 = 0, uzl = 1e9, perc = 0),
                    init = list(suz = suz0))
  expect_equal(r$q_gw, k1 * suz0 * (1 - k1)^(seq_len(n) - 1))
  expect_equal(r$suz, suz0 * (1 - k1)^seq_len(n))
  # the same for the lower box at rate K2
  k2 <- 0.05
  r2 <- morie_hbvMod(rep(0, n), rep(10, n), rep(0, n),
                     pars(k0 = 0, k1 = 0, k2 = k2, uzl = 1e9, perc = 0),
                     init = list(slz = suz0))
  expect_equal(r2$q_gw, k2 * suz0 * (1 - k2)^(seq_len(n) - 1))
})

test_that("the K0 outlet only fires above UZL", {
  k0 <- 0.3
  k1 <- 0.1
  uzl <- 20
  p <- pars(k0 = k0, k1 = k1, k2 = 0, uzl = uzl, perc = 0)
  # below the threshold only K1 contributes
  r <- morie_hbvMod(0, 10, 0, p, init = list(suz = 15))
  expect_equal(r$q_gw, k1 * 15)
  # above it both outlets contribute
  r2 <- morie_hbvMod(0, 10, 0, p, init = list(suz = 50))
  expect_equal(r2$q_gw, k0 * (50 - uzl) + k1 * 50)
  # exactly at the threshold the near-surface outlet is silent
  r3 <- morie_hbvMod(0, 10, 0, p, init = list(suz = uzl))
  expect_equal(r3$q_gw, k1 * uzl)
})

test_that("percolation is capped by PERC and by available water", {
  perc <- 1.5
  p <- pars(perc = perc, k0 = 0, k1 = 0, k2 = 0, uzl = 1e9)
  r <- morie_hbvMod(0, 10, 0, p, init = list(suz = 100))
  expect_equal(r$slz, perc)
  expect_equal(r$suz, 100 - perc)
  # less water available than PERC allows
  r2 <- morie_hbvMod(0, 10, 0, p, init = list(suz = 0.4))
  expect_equal(r2$slz, 0.4)
  expect_equal(r2$suz, 0)
})

test_that("MAXBAS routing delays and smooths without losing water", {
  n <- 80
  p <- c(rep(50, 5), rep(0, n - 5))
  cfg <- pars(maxbas = 4, fc = 10, beta = 1, perc = 0, k0 = 0, k1 = 0.3,
              k2 = 0, uzl = 1e9)
  r <- morie_hbvMod(p, rep(10, n), rep(0, n), cfg)
  # routing is a convolution with a unit kernel: it conserves volume
  expect_equal(sum(r$q), sum(r$q_gw), tolerance = 1e-6)
  # the routed peak is later and lower than the unrouted one
  expect_true(which.max(r$q) > which.max(r$q_gw))
  expect_true(max(r$q) < max(r$q_gw))
  # MAXBAS = 1 is the identity
  r1 <- morie_hbvMod(p, rep(10, n), rep(0, n), pars(maxbas = 1, fc = 10,
        beta = 1, perc = 0, k0 = 0, k1 = 0.3, k2 = 0, uzl = 1e9))
  expect_equal(r1$q, r1$q_gw)
  # and the routed series is the explicit convolution of q_gw with the kernel
  w <- as.numeric(unlist(.hbvMod_maxbas_weights(4)))
  conv <- vapply(seq_len(n), function(i) {
    j <- seq_len(min(length(w), i))
    sum(w[j] * r$q_gw[i - j + 1])
  }, numeric(1))
  expect_equal(r$q, conv)
})

test_that("mass balance closes over a long simulation", {
  set.seed(7)
  n <- 400
  p <- pmax(0, rnorm(n, 3, 4))
  tm <- 2 + 12 * sin(2 * pi * seq_len(n) / 365)
  ep <- pmax(0, 2 + 1.5 * sin(2 * pi * seq_len(n) / 365))
  cfgs <- list(list(), list(sfcf = 1.25), list(cwh = 0.25),
               list(sfcf = 0.9, cwh = 0.05), list(maxbas = 5.5),
               list(beta = 3, lp = 0.3), list(perc = 0), list(uzl = 0))
  for (cfg in cfgs) {
    r <- morie_hbvMod(p, tm, ep, do.call(pars, cfg))
    expect_equal(r$mass_balance_error, 0, tolerance = 1e-8)
    expect_equal(r$n_days, n)
    expect_true(all(r$q >= 0))
    expect_true(all(r$snow >= 0))
    expect_true(all(r$e_act >= 0))
    # soil moisture stays within its physical bounds
    expect_true(all(r$soil >= 0 & r$soil <= 200 + 1e-9))
  }
})

test_that("no input means no runoff, and storage is carried in", {
  n <- 5
  r <- morie_hbvMod(rep(0, n), rep(10, n), rep(0, n), pars())
  expect_equal(r$q, rep(0, n))
  expect_equal(r$snow, rep(0, n))
  # init states are respected rather than silently zeroed
  r2 <- morie_hbvMod(rep(0, n), rep(-10, n), rep(0, n), pars(),
                     init = list(snow = 40, soil = 30, suz = 20, slz = 10))
  expect_equal(r2$snow, rep(40, n))
  expect_true(all(r2$q > 0))
  expect_true(r2$slz[1] > 0)
})

test_that("invalid input is rejected", {
  expect_error(morie_hbvMod(1:3, 1:2, 1:3, pars()), "equal length")
  expect_error(morie_hbvMod(1:3, 1:3, 1:2, pars()), "equal length")
  expect_error(morie_hbvMod(1, 1, 1, pars(fc = 0)), "fc must be positive")
  expect_error(morie_hbvMod(1, 1, 1, pars(fc = -5)), "fc must be positive")
  expect_error(morie_hbvMod(1, 1, 1, pars(maxbas = 0.5)), "maxbas >= 1")
  expect_error(morie_hbvMod(1, 1, 1, list(tt = 0)), "params missing")
  # the message names every parameter that is absent
  err <- tryCatch(morie_hbvMod(1, 1, 1, list()), error = conditionMessage)
  for (nm in c("tt", "cfmax", "fc", "lp", "beta", "k0", "k1", "k2",
               "uzl", "perc", "maxbas")) {
    expect_true(grepl(nm, err, fixed = TRUE))
  }
})

test_that("the reported snow state is total pack water", {
  # solid plus retained liquid: on the first melt day nothing has left the
  # pack, so its total is unchanged even though part of it is now liquid
  r <- morie_hbvMod(c(100, 0), c(-5, 1), c(0, 0), snow_only(cwh = 0.5))
  expect_equal(r$snow[1], 100)
  expect_equal(r$snow[2], 100)
  expect_equal(r$q_gw[2], 0)
})

test_that("the alias and cheatsheet are intact", {
  a <- morie_hbvMod(c(5, 0), c(1, 1), c(0, 0), pars())
  b <- hbv_hydrology(c(5, 0), c(1, 1), c(0, 0), pars())
  expect_equal(a, b)
  expect_equal(a$method, "HBV (Seibert & Vis 2012, Eqs. 1-6)")
  expect_type(.hbvMod_cheatsheet(), "character")
  expect_match(.hbvMod_cheatsheet(), "HBV")
})
