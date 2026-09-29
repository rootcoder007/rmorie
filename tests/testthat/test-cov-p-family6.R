# Coverage for polqnt .. ppoc exports. Every expectation is recomputed in
# the test body.

test_that("morie_polqnt round-trips exactly without quantisation and bins angles with it", {
  x <- c(0.5, -1.2, 0.3, 2.0, -0.7, -0.1, 1.1, 0.4)
  r0 <- morie_polqnt(x, quantize = FALSE)
  expect_equal(r0$reconstruction, x, tolerance = 1e-12)
  expect_equal(r0$radius, sqrt(sum(x^2)), tolerance = 1e-12)
  expect_equal(r0$mse, 0, tolerance = 1e-24)
  r <- morie_polqnt(x, bits_first = 3, bits_rest = 2)
  # level-1 angles of the pairs in [0, 2 pi), then radii recursively
  a1 <- atan2(x[c(2, 4, 6, 8)], x[c(1, 3, 5, 7)]) %% (2 * pi)
  r1 <- sqrt(x[c(1, 3, 5, 7)]^2 + x[c(2, 4, 6, 8)]^2)
  a2 <- atan2(r1[c(2, 4)], r1[c(1, 3)])
  r2 <- sqrt(r1[c(1, 3)]^2 + r1[c(2, 4)]^2)
  a3 <- atan2(r2[2], r2[1])
  q1 <- pmin(floor(a1 * 8 / (2 * pi)), 7)
  q2 <- pmin(floor(a2 * 4 / (pi / 2)), 3)
  q3 <- pmin(floor(a3 * 4 / (pi / 2)), 3)
  expect_equal(r$codes, c(q1, q2, q3))
  th3 <- (q3 + 0.5) * (pi / 2) / 4
  R2 <- r0$radius * c(cos(th3), sin(th3))
  th2 <- (q2 + 0.5) * (pi / 2) / 4
  R1 <- as.numeric(rbind(R2 * cos(th2), R2 * sin(th2)))
  th1 <- (q1 + 0.5) * 2 * pi / 8
  rec <- as.numeric(rbind(R1 * cos(th1), R1 * sin(th1)))
  expect_equal(r$reconstruction, rec, tolerance = 1e-12)
  expect_equal(r$mse, mean((rec - x)^2), tolerance = 1e-12)
  expect_equal(r$bits_per_coord, (4 * 3 + 2 * 2 + 1 * 2) / 8)
  expect_error(morie_polqnt(1:3), "power of two")
  expect_error(morie_polqnt(1:4, bits_first = 0), "at least 1")
})

test_that("morie_polyak averages iterates after the burn-in", {
  its <- list(c(1, 2), c(3, 1), c(2, 2), c(4, 0))
  r <- morie_polyak(its, burn_in = 1)
  expect_equal(r$average, colMeans(do.call(rbind, its[2:4])), tolerance = 1e-12)
  expect_equal(r$n_averaged, 3L)
  expect_equal(morie_polyak(its)$average, c(2.5, 1.25))
  expect_error(morie_polyak(list()), "no iterates")
  expect_error(morie_polyak(its, burn_in = 4), "discards all")
})

test_that("Chebbasis follows the three-term recurrence and cos(n acos x)", {
  x <- c(-1, -0.4, 0.2, 0.9, 1.5)
  r <- Chebbasis(x, K = 4)
  Tn <- cbind(1, x, 2 * x^2 - 1, 4 * x^3 - 3 * x, 8 * x^4 - 8 * x^2 + 1)
  expect_equal(r$basis, unname(Tn), tolerance = 1e-12)
  ok <- abs(x) <= 1
  expect_equal(r$trig[ok, ], unname(Tn[ok, ]), tolerance = 1e-12)
  expect_true(all(is.na(r$trig[!ok, ])))
  expect_equal(Chebbasis(x, K = 0)$basis, matrix(1, 5, 1))
  expect_error(Chebbasis(x, K = -1), "non-negative")
})

test_that("PopVar and Ppcrep evaluate their definitions", {
  x <- c(2, 4, 4, 5, 7, 9)
  r <- PopVar(x)
  expect_equal(r$variance, stats::var(x) * 5 / 6, tolerance = 1e-12)
  expect_error(PopVar(c(1, NA)), "non-empty numeric")
  tr <- c(1.2, 3.4, 2.2, 5.0, 0.8, 2.9, 4.1, 3.3)
  p <- Ppcrep(3, tr)
  expect_equal(p$p_value, mean(tr >= 3))
  expect_equal(p$p_two_sided, 2 * min(mean(tr >= 3), 1 - mean(tr >= 3)))
  to <- c(1, 4, 2, 6, 1, 2, 5, 3)
  expect_equal(Ppcrep(to, tr)$n_extreme, sum(tr >= to))
  expect_equal(Ppcrep(0, tr)$extreme, 1)
  expect_error(Ppcrep(1, 1), "two replicates")
  expect_error(Ppcrep(1:3, tr), "one value per draw")
})

test_that("Postrt and Poststs post-stratify to the population sizes", {
  y <- c(3, 5, 4, 10, 12, 7, 8)
  s <- c("a", "a", "a", "b", "b", "c", "c")
  Nh <- c(a = 50, b = 30, c = 20)
  w <- c(1, 2, 1, 3, 1, 2, 2)
  r <- Postrt(y, w, s, Nh)
  fac <- as.numeric(Nh / tapply(w, s, sum)[names(Nh)])
  names(fac) <- names(Nh)
  wa <- as.numeric(w * fac[s])
  expect_equal(r$weights, unname(wa), tolerance = 1e-12)
  expect_equal(r$factors, unname(fac), tolerance = 1e-12)
  expect_equal(r$estimate, sum(wa * y) / sum(wa), tolerance = 1e-12)
  expect_equal(r$N, 100, tolerance = 1e-12)
  p <- Poststs(y, s, Nh)
  mh <- tapply(y, s, mean)[names(Nh)]
  vh <- tapply(y, s, stats::var)[names(Nh)]
  nh <- table(s)[names(Nh)]
  W <- Nh / 100
  expect_equal(p$estimate, sum(W * mh), tolerance = 1e-12)
  expect_equal(p$variance, sum(W^2 * vh / nh), tolerance = 1e-12)
  # unnamed sizes follow the order strata first appear in
  expect_equal(Poststs(y, s, c(50, 30, 20))$estimate, p$estimate, tolerance = 1e-12)
  expect_error(Poststs(y, s, c(50, 30)), "one size per stratum")
  expect_error(Poststs(y, s, c(a = 50, b = 30)), "has no population size")
  expect_error(Postrt(y, -w, s, Nh), "weights must be positive")
  expect_error(Postrt(y, w[-1], s, Nh), "one entry per observation")
  expect_error(Poststs(numeric(0), character(0), Nh), "y is empty")
})

test_that("morie_potM fits the GPD to threshold exceedances", {
  y <- c(1.2, 3.5, 0.4, 5.8, 2.2, 7.9, 4.4, 0.9, 6.3, 2.8, 9.7, 3.1, 5.2, 1.7, 8.4, 4.9, 12.5, 2.5,
         6.9, 3.8)
  u <- 3
  r <- morie_potM(y, u, return_periods = c(5, 50))
  z <- y[y > u] - u
  nll <- function(p) {
    if (p[1] <= 0 || any(1 + p[2] * z / p[1] <= 0)) return(1e10)
    length(z) * log(p[1]) + (1 + 1 / p[2]) * sum(log(1 + p[2] * z / p[1]))
  }
  best <- stats::optim(c(mean(z), 0.1), nll, control = list(reltol = 1e-14, maxit = 5000))
  # BFGS stops at reltol = 1e-8 on the log-likelihood
  expect_equal(-r$loglik, best$value, tolerance = 1e-7)
  expect_equal(r$loglik, -nll(c(r$sigma, r$xi)), tolerance = 1e-12)
  rate <- length(z) / 20
  rl <- u + r$sigma / r$xi * ((c(5, 50) * rate)^r$xi - 1)
  expect_equal(unname(r$return_levels), rl, tolerance = 1e-12)
  expect_equal(r$n_exceedances, length(z))
  # a return period shorter than one exceedance interval has no level
  expect_true(is.na(morie_potM(y, u, return_periods = 1)$return_levels[[1]]))
  expect_same_function(morie_potm, morie_potM)
  expect_match(morie_potM_cheatsheet(), "GPD")
  expect_error(morie_potM(y, 100), "at least two exceedances")
})

test_that("the (1 + a)^n approximations and the series identity", {
  r <- PowExpApx(0.02, 30)
  expect_equal(r$exact, 1.02^30, tolerance = 1e-12)
  expect_equal(r$approx, exp(0.6), tolerance = 1e-12)
  expect_equal(r$na2, 30 * 0.0004, tolerance = 1e-12)
  expect_true(r$valid)
  r2 <- PowExpAp2(0.02, 30)
  expect_equal(r2$approx, exp(0.6 - 30 * 0.0004 / 2), tolerance = 1e-12)
  expect_equal(r2$na3, 30 * 0.02^3, tolerance = 1e-12)
  # the second-order form is closer than the first
  expect_lt(abs(r2$approx - r2$exact), abs(r$approx - r$exact))
  s <- PowLogSer(0.3, 7, terms = 40)
  expect_equal(s$product_form, 1.3^7, tolerance = 1e-12)
  s3 <- PowLogSer(0.3, 7, terms = 3)
  expect_equal(s3$product_form, exp(7 * (0.3 - 0.09 / 2 + 0.027 / 3)), tolerance = 1e-12)
  expect_error(PowExpApx(-1, 2), "a > -1")
  expect_error(PowExpAp2(-2, 2), "a > -1")
  expect_error(PowLogSer(1, 2), "abs\\(a\\) < 1")
  expect_error(PowLogSer(0.5, 2, terms = 0), "integer >= 1")
})

test_that("Powsrv is two-sided z power on the effective sample size", {
  r <- Powsrv(0.3, alpha = 0.05, DEFF = 1.5, n = 120)
  ne <- 120 / 1.5
  z <- stats::qnorm(0.975)
  expect_equal(r$power, stats::pnorm(0.3 * sqrt(ne) - z) + stats::pnorm(-0.3 * sqrt(ne) - z),
               tolerance = 1e-12)
  expect_equal(r$n_eff, ne)
  if (requireNamespace("pwr", quietly = TRUE)) {
    expect_equal(r$power, pwr::pwr.norm.test(d = 0.3, n = ne, sig.level = 0.05)$power, tolerance = 1e-12)
  }
  expect_error(Powsrv(0.3, alpha = 1), "alpha must lie")
  expect_error(Powsrv(0.3, DEFF = 0), "DEFF must be positive")
  expect_error(Powsrv(0.3, n = 0), "at least 1")
})

test_that("Ppoclip is the PPO clipped surrogate with value and entropy terms", {
  adv <- c(1.0, -0.5, 2.0, -1.5, 0.3)
  lpn <- c(-0.9, -1.4, -0.2, -2.0, -1.0)
  lpo <- c(-1.1, -1.0, -0.6, -1.5, -1.05)
  r <- Ppoclip(NULL, adv = adv, logp_new = lpn, logp_old = lpo, clip_eps = 0.2,
               v_pred = c(1, 2, 3), v_targ = c(1.5, 1.5, 2), entropy = c(0.7, 0.9), c1 = 0.5, c2 = 0.02)
  ratio <- exp(lpn - lpo)
  obj <- pmin(ratio * adv, pmin(pmax(ratio, 0.8), 1.2) * adv)
  expect_equal(r$l_clip, mean(obj), tolerance = 1e-12)
  expect_equal(r$frac_clipped, mean(pmin(pmax(ratio, 0.8), 1.2) * adv < ratio * adv))
  lvf <- mean((c(1, 2, 3) - c(1.5, 1.5, 2))^2)
  expect_equal(r$total, mean(obj) - 0.5 * lvf + 0.02 * 0.8, tolerance = 1e-12)
  # ratios given directly, or through the policy argument
  expect_equal(Ppoclip(adv, policy = ratio)$l_clip, mean(obj), tolerance = 1e-12)
  expect_equal(Ppoclip(NULL, adv = adv, ratio = ratio)$l_clip, mean(obj), tolerance = 1e-12)
})
