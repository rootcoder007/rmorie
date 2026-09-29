# Coverage for climate and assorted numeric helpers (ar1cl.R, clausC.R,
# co2RF_native.R, ecsTCR_native.R, basEvap_native.R, airbed.R,
# cgmth_native.R, crtT_mixedcase_native.R, cthrgr_native.R,
# doob_trends.R): formulas are recomputed, the two-layer energy-balance
# solver is checked against the eigen-decomposition solution of its
# linear ODE, and the CCRSO tables are checked as constants.

test_that("ar1cl fits the Hasselmann AR(1) climate model", {
  x <- c(0.3, 0.5, 0.2, -0.1, 0.4, 0.6, 0.1, -0.3, 0.0, 0.2)
  r <- ar1cl(x, dt = 2, freq = c(0, 0.1))
  m <- mean(x)
  c0 <- mean((x - m)^2)
  c1 <- sum((x[-1] - m) * (x[-10] - m)) / 10
  ph <- c1 / c0
  s2 <- c0 * (1 - ph^2)
  expect_equal(r$phi, ph, tolerance = 1e-12)
  expect_equal(r$sigma2_eps, s2, tolerance = 1e-12)
  expect_equal(r$spectrum, s2 * 2 / (1 - 2 * ph * cos(2 * pi * c(0, 0.1) * 2) + ph^2),
               tolerance = 1e-12)
  expect_equal(r$tau, if (ph > 0 && ph < 1) -2 / log(ph) else Inf, tolerance = 1e-12)
  expect_equal(ar1cl(x, phi = 0.5)$tau, -1 / log(0.5), tolerance = 1e-12)
  expect_same_function(morie_ar1_climate, ar1cl)
})

test_that("clausC gives the Clausius-Clapeyron rate", {
  Tk <- c(273.15, 288, 300)
  r <- clausC(Tk)
  es <- 611.2 * exp(2.501e6 / 461.5 * (1 / 273.15 - 1 / Tk))
  expect_equal(r$es, es, tolerance = 1e-12)
  expect_equal(r$rate, 2.501e6 / (461.5 * Tk^2), tolerance = 1e-12)
  expect_equal(r$des_dt, 2.501e6 * es / (461.5 * Tk^2), tolerance = 1e-12)
  expect_same_function(morie_clausius_clapeyron, clausC)
})

test_that("radiative_forcing_co2 follows the AR6 / Myhre forms", {
  a1 <- -2.4785e-7
  b1 <- 7.5906e-4
  c1 <- -2.1492e-3
  d1 <- 5.2488
  r <- radiative_forcing_co2(560, N = 300)
  alpha <- d1 + a1 * (560 - 277.15)^2 + b1 * (560 - 277.15)
  expect_equal(r$sarf, (alpha + c1 * sqrt(300)) * log(560 / 277.15), tolerance = 1e-12)
  hi <- radiative_forcing_co2(3000)
  expect_equal(hi$alpha_prime, d1 - b1^2 / (4 * a1), tolerance = 1e-12)
  lo <- radiative_forcing_co2(200)
  expect_equal(lo$alpha_prime, d1)
  my <- radiative_forcing_co2(560, C0 = 280, method = "myhre1998", erf_adjustment = TRUE)
  expect_equal(my$estimate, 1.05 * 5.35 * log(2), tolerance = 1e-12)
  expect_match(co2RF_cheatsheet(), "5.35 ln")
  expect_error(radiative_forcing_co2(0), "positive")
  expect_error(radiative_forcing_co2(400, N = -1), "non-negative")
  expect_error(radiative_forcing_co2(400, method = "x"), "ar6")
})

test_that("two-layer energy balance: ECS, TCR and the exact ODE solution", {
  expect_equal(morie_ecsTCR_co2_forcing(c(1, 2, 4), f2x = 3.7), c(0, 3.7, 7.4), tolerance = 1e-12)
  lam <- 1.2
  g <- 0.7
  ep <- 1.3
  C <- 8
  CD <- 100
  Fv <- c(1, 2, 3, 3.5)
  r <- morie_ecsTCR_integrate(Fv, lam, g, ep, C, CD, solver = "analytic", dt = 1)
  A <- rbind(c(-(lam + ep * g) / C, ep * g / C), c(g / CD, -g / CD))
  ev <- eigen(A)
  x <- c(0, 0)
  out <- matrix(0, 5, 2)
  for (i in 1:4) {
    eq <- -solve(A, c(Fv[i] / C, 0))
    x <- eq + Re(ev$vectors %*% diag(exp(ev$values)) %*% solve(ev$vectors) %*% (x - eq))
    out[i + 1, ] <- x
  }
  expect_equal(r$temperature, out[, 1], tolerance = 1e-10)
  expect_equal(r$deep_temperature, out[, 2], tolerance = 1e-10)
  N1 <- Fv - lam * out[1:4, 1] - (ep - 1) * g * (out[1:4, 1] - out[1:4, 2])
  expect_equal(r$imbalance, N1, tolerance = 1e-10)
  rk <- morie_ecsTCR_integrate(Fv, lam, g, ep, C, CD, solver = "rk4", dt = 1)
  # classical RK4 with h = 1 against the exact flow: the stiffest rate is
  # about 0.3 per step, so the local error is of order 0.3^5 / 120 ~ 2e-5
  expect_equal(rk$temperature, out[, 1], tolerance = 1e-4)
  eu <- morie_ecsTCR_integrate(Fv, lam, g, ep, C, CD, solver = "euler", dt = 1)
  expect_equal(eu$temperature[2], Fv[1] / C, tolerance = 1e-12)
  one <- morie_ecsTCR_integrate(Fv, lam, 0, ep, C, CD)
  expect_equal(one$temperature[2], Fv[1] / lam * (1 - exp(-lam / C)), tolerance = 1e-12)
  expect_error(morie_ecsTCR_integrate(Fv, lam, solver = "x"), "expected one of")
  e <- morie_ecsTCR(lam = 1.3, years = 80, rate = 0.01)
  expect_equal(e$ecs, 3.93 / 1.3, tolerance = 1e-12)
  yr <- which(1.01^(1:80) >= 2)[1]
  expect_equal(e$doubling_year, yr)
  expect_equal(e$tcr, e$temperature[yr + 1], tolerance = 1e-12)
  Tv <- c(0.2, 0.5, 0.9, 1.3, 1.6)
  Nv <- 3.8 - 1.1 * Tv
  gr <- morie_ecsTCR(route = "gregory", temperature = Tv, imbalance = Nv, forcing_multiple = 4)
  expect_equal(gr$lambda, 1.1, tolerance = 1e-12)
  expect_equal(gr$f2x, 3.8 / 2, tolerance = 1e-12)
  expect_error(morie_ecsTCR(route = "x"), "expected one of")
  expect_error(morie_ecsTCR(route = "gregory"), "needs both")
  expect_error(morie_ecsTCR(route = "gregory", temperature = 1:2, imbalance = 1), "entries")
  expect_error(morie_ecsTCR(route = "gregory", temperature = Tv, imbalance = Tv), "non-positive")
  expect_error(morie_ecsTCR(route = "gregory", temperature = rep(1, 3), imbalance = 1:3), "no spread")
  expect_error(morie_ecsTCR(), "give lam")
  expect_error(morie_ecsTCR(lam = -1), "non-positive")
})

test_that("morie_basEvap is the FAO-56 Penman-Monteith ET0", {
  r <- morie_basEvap(T = 21, R_n = 13.3, u2 = 2.1, VPD = 1.2, G = 0.1, P = 100.1)
  es <- 0.6108 * exp(17.27 * 21 / (21 + 237.3))
  de <- 4098 * es / (21 + 237.3)^2
  ga <- 0.665e-3 * 100.1
  et0 <- (0.408 * de * 13.2 + ga * 900 / 294 * 2.1 * 1.2) / (de + ga * (1 + 0.34 * 2.1))
  expect_equal(r$estimate, et0, tolerance = 1e-12)
  expect_error(morie_basEvap(20, 10, 2, -1), "VPD")
  expect_error(morie_basEvap(20, 10, -2, 1), "wind")
  expect_error(morie_basEvap(20, 10, 2, 1, P = 0), "pressure")
})

test_that("Emisinv sums activity x factor x GWP", {
  A <- rbind(c(10, 2), c(5, 8))
  E <- rbind(c(0.5, 1.5), c(2, 0.1))
  r <- Emisinv(A, E, gwp = c(1, 25))
  cell <- A * E * matrix(c(1, 25), 2, 2, byrow = TRUE)
  expect_equal(r$cell, cell, tolerance = 1e-12)
  expect_equal(c(r$total, r$bysector, r$byfuel), c(sum(cell), rowSums(cell), colSums(cell)),
               tolerance = 1e-12)
  expect_error(Emisinv(A, E[, 1]), "same shape")
  expect_error(Emisinv(A, E, gwp = 1), "one entry per column")
})

test_that("cgmth is Polak-Ribiere+ nonlinear conjugate gradient", {
  Q <- rbind(c(4, 1), c(1, 3))
  b <- c(1, 2)
  f <- function(x) 0.5 * sum(x * (Q %*% x)) - sum(b * x)
  gf <- function(x) as.numeric(Q %*% x - b)
  # the Armijo test compares f values, so a gradient tolerance much below
  # 1e-10 asks for decreases under double precision of f
  r <- cgmth(f, gf, c(0, 0), tol = 1e-10, full_output = TRUE)
  expect_equal(r$x_min, as.numeric(solve(Q, b)), tolerance = 1e-9)
  expect_true(r$info$converged)
  expect_equal(r$info$final_value, f(solve(Q, b)), tolerance = 1e-12)
  expect_equal(morie_cgmth(f, gf, c(1, 1), tol = 1e-10), as.numeric(solve(Q, b)), tolerance = 1e-9)
  nc <- cgmth(f, gf, c(5, 5), max_iter = 1, full_output = TRUE)
  expect_false(nc$info$converged)
})

test_that("crtT solves simultaneous congruences", {
  r <- crtT(c(2, 3, 2), c(3, 5, 7))
  expect_equal(r$estimate, 23)
  expect_equal(r$modulus, 105)
  expect_equal(r$estimate %% c(3, 5, 7), c(2, 3, 2))
  expect_equal(crtT(4, 9)$estimate, 4)
  expect_match(crtT_cheatsheet(), "Euclid")
  expect_same_function(chinese_remainder, crtT)
  expect_error(crtT(1:2, 3), "paired")
  expect_error(crtT(1, 1), ">= 2")
  expect_error(crtT(c(1, 2), c(4, 6)), "coprime")
})

test_that("morie_cthrgr decomposes the total effect into direct + indirect", {
  set <- 1:40
  X <- cbind(sin(set), cos(set / 2))
  D <- as.numeric(set %% 3 == 0 | set %% 5 == 0)
  M <- 0.5 * D + X[, 1] + 0.1 * cos(set)
  y <- D + 0.8 * M + X[, 2] + 0.05 * sin(3 * set)
  r <- morie_cthrgr(y, D, M, X, n_trees = 4L, min_leaf = 3L, max_depth = 2L, n_draw = 4L)
  expect_equal(r$total, r$direct + r$indirect, tolerance = 1e-12)
  expect_equal(r$estimate, mean(r$total), tolerance = 1e-12)
  expect_equal(r$nde, mean(r$direct), tolerance = 1e-12)
  expect_equal(r$proportion_mediated, r$nie / r$estimate, tolerance = 1e-12)
  mn <- morie_cthrgr(y, D, M, X, route = "mean", n_trees = 4L, newX = X[1:3, ])
  expect_equal(mn$n_draw, 1L)
  expect_equal(mn$n_query, 3L)
  expect_match(morie_cthrgr_cheatsheet(), "gcomputed, mean")
  expect_error(morie_cthrgr(y, D, M, X, route = "x"), "route must be")
  expect_error(morie_cthrgr(y[-1], D, M, X), "agree in length")
  expect_error(morie_cthrgr(y[1:5], D[1:5], M[1:5], X[1:5, ]), "eight")
  expect_error(morie_cthrgr(y, rep(1, 40), M, X), "both treatment arms")
})

test_that("CCRSO tables are well-formed constants", {
  expect_s3_class(CCRSO_TABLE1_RELEASES, "data.frame")
  expect_equal(nrow(CCRSO_TABLE1_RELEASES), 3L)
  expect_equal(CCRSO_TABLE1_RELEASES$type, c("Day Parole", "Full Parole", "Statutory Release"))
  expect_s3_class(CCRSO_TABLE2_FLOW, "data.frame")
  expect_equal(nrow(CCRSO_TABLE2_FLOW), 5L)
  expect_type(CCRSO_TABLE2_FLOW$avg_count, "double")
  expect_s3_class(CCRSO_TABLE3_AGE, "data.frame")
  expect_equal(CCRSO_TABLE3_AGE$age_group, c("18-49", "50-59", "60+"))
  expect_equal(sum(CCRSO_TABLE3_AGE$canada_adult_pop_pct), 100.1, tolerance = 1e-12)
})
