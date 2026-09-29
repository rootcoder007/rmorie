# Coverage for the Fauzi kernel-estimation suite (fz*.R): every assertion
# recomputes its expected value from the book's formula or from base R.

trap_ref <- function(y, g) sum(diff(g) * (y[-length(y)] + y[-1]) / 2)
rratio_ref <- function(z) sqrt(2 * pi) * z^(z + 0.5) / (exp(z) * gamma(z + 1))

test_that("Qasyvar is qp^2 p (1 - p) / n from either parametrisation", {
  expect_same_function(morie_fauzi_quantile_asymp_var, Qasyvar)
  r <- Qasyvar(p = 0.3, n = 50, density = 0.8)
  expect_equal(r$variance, (1 / 0.8)^2 * 0.3 * 0.7 / 50, tolerance = 1e-12)
  expect_equal(r$se, sqrt(r$variance), tolerance = 1e-12)
  r2 <- Qasyvar(p = 0.3, n = 50, qp = 1.25)
  expect_equal(r2$variance, r$variance, tolerance = 1e-12)
  expect_error(Qasyvar(p = 0.3, n = 50), "exactly one")
  expect_error(Qasyvar(p = 0.3, n = 50, density = 0), "positive")
  expect_error(Qasyvar(p = 1, n = 50, density = 1), "strictly")
})

test_that("Kdfassum checks B1-B3 against trapezoid moments", {
  expect_same_function(morie_fauzi_assumptions_b1_b5, Kdfassum)
  w <- seq(-8, 8, length.out = 4001)
  r <- Kdfassum(h = 0.1, n = 100)
  expect_equal(r$mass, trap_ref(dnorm(w), w), tolerance = 1e-12)
  expect_equal(r$mu4, trap_ref(w^4 * dnorm(w), w), tolerance = 1e-12)
  expect_equal(r$mu4, 3, tolerance = 1e-6)
  expect_true(r$b1 && r$b2 && r$b3)
  expect_true(is.na(Kdfassum()$b3))
  expect_false(Kdfassum(kernel = function(t) dunif(t, 0, 1))$b1)
  expect_false(Kdfassum(h = 0.001, n = 100)$b3)
  expect_error(Kdfassum(kernel = 1), "function")
})

test_that("KDFE bias coefficients b2, b4 and boundary-free c1, c2", {
  expect_same_function(morie_fauzi_b2_coefficient_kdfe, Kdfb2)
  expect_same_function(morie_fauzi_b4_coefficient, Kdfb4)
  expect_same_function(morie_fauzi_c1_coefficient, Bfc1)
  expect_same_function(morie_fauzi_c2_coefficient, Bfc2)
  expect_equal(Kdfb2(fp = 0.7, mu2 = 2)$estimate, 0.7 / 2 * 2, tolerance = 1e-12)
  expect_equal(Kdfb4(fppp = 1.3, mu4 = 3)$estimate, 1.3 * 3 / 24, tolerance = 1e-12)
  expect_equal(Bfc1(dg = 2, d2g = 0.5, density = 0.3, fp = -0.1)$estimate,
               0.5 * 0.3 + 4 * -0.1, tolerance = 1e-12)
  r <- Bfc2(dg = 2, d2g = 0.5, d3g = 0.25, density = 0.3, fp = -0.1, fpp = 0.2)
  v <- 0.25 * 0.3 + 3 * 0.5 * 2 * -0.1 + 8 * 0.2
  expect_equal(r$estimate, v, tolerance = 1e-12)
  expect_equal(r$scaled, v / 2, tolerance = 1e-12)
  expect_error(Bfc2(0, 1, 1, 1, 1, 1), "non-zero")
})

test_that("MRL coefficients b4 and b5", {
  expect_same_function(morie_fauzi_b4_coefficient_mrl, Mrlb4)
  expect_same_function(morie_fauzi_b5_coefficient_mrl, Mrlb5)
  r <- Mrlb4(surv = 0.4, cumsurv = 0.9)
  m <- 0.9 / 0.4
  expect_equal(r$mrl, m, tolerance = 1e-12)
  expect_equal(r$estimate, 2 * 0.9 - 0.4 * m^2, tolerance = 1e-12)
  expect_equal(r$varterm, r$estimate / 0.16, tolerance = 1e-12)
  expect_equal(Mrlb4(surv = 0.4, cumsurv = 0.9, mrl = 1)$estimate, 1.8 - 0.4,
               tolerance = 1e-12)
  expect_error(Mrlb4(surv = 0, cumsurv = 1), "positive")
  r5 <- Mrlb5(dg = 1.5, density = 0.2, mrl = 3, surv = 0.5)
  expect_equal(r5$estimate, 1.5 * 0.2 * 9, tolerance = 1e-12)
  expect_equal(r5$varterm, r5$estimate / 0.25, tolerance = 1e-12)
  expect_true(is.na(Mrlb5(dg = 1, density = 1, mrl = 1)$varterm))
  expect_error(Mrlb5(1, 1, 1, surv = 0), "positive")
})

test_that("Qbebnd gives 2 Phi(x) - 1 and the stated rates", {
  expect_same_function(morie_fauzi_berry_esseen_quantile, Qbebnd)
  r <- Qbebnd(x = c(0.5, 1.2), n = 64)
  expect_equal(r$estimate, 2 * pnorm(c(0.5, 1.2)) - 1, tolerance = 1e-12)
  expect_equal(r$bound, 64^-0.5, tolerance = 1e-12)
  expect_equal(Qbebnd(1, 64, improved = FALSE)$rate, 7 / 17)
  expect_equal(Qbebnd(1, 64, m = 2L)$rate, 1 / 3)
  expect_equal(Qbebnd(1, 64, m = 3L)$bound, 64^(-5 / 13), tolerance = 1e-12)
  expect_error(Qbebnd(1, 64, m = 5L), "m = 2, 3, 4")
  expect_error(Qbebnd(-1, 64), ">= 0")
})

test_that("Bfkde and Bfkdf are the log-transformed kernel estimators", {
  expect_same_function(morie_fauzi_bdfree_density_from_cdf, Bfkde)
  expect_same_function(morie_fauzi_bdfree_kdfe_test, Bfkdf)
  x <- c(0.5, 1, 1.5, 2, 3, 0.7)
  y <- log(x)
  h <- 1.06 * sd(y) * 6^(-0.2)
  r <- Bfkde(x, grid = c(1.2, 2.5))
  ref <- vapply(c(1.2, 2.5), function(t) mean(dnorm((log(t) - y) / h)) / (h * t),
                numeric(1))
  expect_equal(r$h, h, tolerance = 1e-12)
  expect_equal(r$estimate, ref, tolerance = 1e-12)
  r2 <- Bfkde(x, grid = 1, ginv = function(t) t, dg = function(z) 1, h = 0.4)
  expect_equal(r2$estimate, mean(dnorm((1 - x) / 0.4)) / 0.4, tolerance = 1e-12)
  expect_error(Bfkde(c(-1, 1)), "\\(0, Inf\\)")
  expect_error(Bfkde(x, ginv = log), "dg")
  expect_error(Bfkde(1), "two")
  s <- Bfkdf(x, grid = c(1.2, 2.5), h = 0.3)
  expect_equal(s$estimate, vapply(c(1.2, 2.5), function(t)
    mean(pnorm((log(t) - y) / 0.3)), numeric(1)), tolerance = 1e-12)
  sd0 <- Bfkdf(x)
  iq <- diff(quantile(y, c(0.25, 0.75), names = FALSE)) / 1.349
  expect_equal(sd0$h, 4^(1 / 3) * min(sd(y), iq) * 6^(-1 / 3), tolerance = 1e-12)
  expect_error(Bfkdf(c(0, 1)), "\\(0, Inf\\)")
  expect_error(Bfkdf(x, h = -1), "positive")
})

test_that("Qbwcheck applies the (3.8) window", {
  expect_same_function(morie_fauzi_quantile_bw_condition, Qbwcheck)
  r <- Qbwcheck(h = 0.05, n = 1000)
  expect_equal(r$upper, 0.05 < 1000^-0.25)
  expect_equal(r$lower, 0.05 * 1000^0.3 > 1)
  expect_equal(r$hbook, 1000^-0.25 / log(1000), tolerance = 1e-12)
  expect_false(Qbwcheck(h = 0.9, n = 1000)$ok)
  expect_error(Qbwcheck(h = 0, n = 10), "positive")
  expect_error(Qbwcheck(h = 0.1, n = 1), "at least 2")
})

test_that("Chungsmir scales the KDFE sup error by sqrt(2n / log log n)", {
  expect_same_function(morie_fauzi_chung_smirnov, Chungsmir)
  x <- (1:20) / 21
  r <- Chungsmir(x, cdf = punif, h = 0.05)
  khat <- vapply(sort(x), function(t) mean(pnorm((t - x) / 0.05)), numeric(1))
  sup <- max(abs(khat - sort(x)))
  expect_equal(r$supdiff, sup, tolerance = 1e-12)
  expect_equal(r$statistic, sqrt(40 / log(log(20))) * sup, tolerance = 1e-12)
  expect_error(Chungsmir(1:10, punif), "n > 15")
  expect_error(Chungsmir(x, punif, h = 0), "positive")
})

test_that("Srvcov1 and Srvcov2 are S F / n", {
  expect_same_function(morie_fauzi_cov_surv_est1, Srvcov1)
  expect_same_function(morie_fauzi_cov_surv_est2, Srvcov2)
  expect_equal(Srvcov1(n = 40, surv = 0.3)$covariance, 0.3 * 0.7 / 40,
               tolerance = 1e-12)
  expect_equal(Srvcov2(n = 40, surv = 0.3, cdf = 0.5)$covariance, 0.15 / 40,
               tolerance = 1e-12)
  expect_error(Srvcov1(n = 0, surv = 0.3), "at least 1")
  expect_error(Srvcov2(n = 0, surv = 0.3), "at least 1")
})

test_that("Cvmstat matches the W^2 formula and the Anderson-Darling series", {
  expect_same_function(morie_fauzi_cvm_statistic, Cvmstat)
  x <- c(0.9, 0.12, 0.33, 0.47, 0.61, 0.05, 0.78)
  n <- 7
  w2 <- 1 / (12 * n) + sum(((2 * (1:n) - 1) / (2 * n) - sort(x))^2)
  r <- Cvmstat(x, punif)
  expect_equal(r$statistic, w2, tolerance = 1e-12)
  k <- 0:99
  p <- 1 - sqrt(2 / w2) * sum(exp(-((4 * k + 1)^2) * pi^2 / (8 * w2)))
  expect_equal(r$p_value, min(1, max(0, p)), tolerance = 1e-12)
  expect_error(Cvmstat(1, punif), "two")
  expect_error(Cvmstat(x, 1), "function")
})

test_that("Bfassum checks D1-D5 and the monotonicity of g", {
  expect_same_function(morie_fauzi_conditions_d1_d5, Bfassum)
  v <- seq(-8, 8, length.out = 4001)
  r <- Bfassum(h = 0.1, n = 100, g = exp)
  expect_equal(r$mu2, trap_ref(v^2 * dnorm(v), v), tolerance = 1e-12)
  expect_true(r$d1 && r$d2 && r$d3 && r$d4)
  expect_identical(r$monotone, "increasing")
  expect_identical(Bfassum(g = function(t) -t)$monotone, "decreasing")
  expect_identical(Bfassum(g = function(t) t^2)$monotone, "neither")
  expect_identical(Bfassum()$monotone, "unknown")
  expect_error(Bfassum(kernel = "x"), "function")
})

test_that("Qedgew reduces to Phi and adds the Hermite corrections", {
  expect_same_function(morie_fauzi_gn_edgeworth_correction, Qedgew)
  n <- 50
  h <- 0.2
  s <- 1.1
  e <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  x <- 0.7
  r <- Qedgew(x, n, h, s, e[1], e[2], e[3], e[4], e[5], e[6])
  t1 <- (x^2 - 1) / (6 * sqrt(n) * s^3) * (e[1] + 3 * e[2] / h)
  inner <- x / (4 * s^2) * (4 * e[5] + e[6]) +
    (x^3 - 3 * x) / (6 * s^4) * (3 * e[3] + e[4]) +
    (x^5 - 10 * x^3 + 15 * x) / (8 * s^6) * e[2]^2
  expect_equal(r$estimate, pnorm(x) - dnorm(x) * (t1 + inner / (n * h^2)),
               tolerance = 1e-12)
  rb <- Qedgew(x, n, h, s, e[1], e[2], e[3], e[4], e[5], e[6], book = TRUE)
  inner_b <- inner + (x^3 - 3 * x) / (6 * s^4) * 3 * (e[2] - e[3])
  expect_equal(rb$correction, dnorm(x) * (t1 + inner_b / (n * h^2)),
               tolerance = 1e-12)
  rd <- Qedgew(0, n, h, s, 0, 0, 0, 0, 0, 0, delta = 1)
  expect_equal(rd$estimate, pnorm(-1 / (s * sqrt(n))), tolerance = 1e-12)
  expect_error(Qedgew(0, n, 0, s, 0, 0, 0, 0, 0, 0), "bandwidth")
})

test_that("Hsumcdf integrates F(2 theta + u) f(u)", {
  expect_same_function(morie_fauzi_g_theta_distribution, Hsumcdf)
  r <- Hsumcdf(theta = c(-0.4, 0, 0.3))
  # (X1 + X2) / 2 of standard normals is N(0, 1/2); trapezoid on [-10, 10]
  expect_equal(r$estimate, pnorm(c(-0.4, 0, 0.3), sd = sqrt(0.5)), tolerance = 1e-9)
  ru <- Hsumcdf(0.25, cdf = punif, density = dunif, lo = 0, hi = 1)
  u <- seq(0, 1, length.out = 4001)
  expect_equal(ru$estimate, trap_ref(punif(0.5 + u), u), tolerance = 1e-12)
  expect_error(Hsumcdf(0, cdf = 1), "functions")
})

test_that("Kdfmise, Kdfr1 and Kdfr2", {
  expect_same_function(morie_fauzi_kdfe_mise, Kdfmise)
  expect_same_function(morie_fauzi_r1_integral, Kdfr1)
  expect_same_function(morie_fauzi_r2_integral, Kdfr2)
  r1 <- 1 / (2 * sqrt(pi))
  expect_equal(Kdfr1()$estimate, r1, tolerance = 1e-12)
  yk <- seq(-8, 8, length.out = 4001)
  wk <- c(0, cumsum(diff(yk) * (dnorm(yk[-4001]) + dnorm(yk[-1])) / 2))
  expect_equal(Kdfr1(kernel = dnorm)$estimate, trap_ref(yk * dnorm(yk) * wk, yk),
               tolerance = 1e-12)
  # the trapezoid rule on a 0.004 grid is within 1e-6 of the closed form
  expect_equal(Kdfr1(kernel = dnorm)$estimate, r1, tolerance = 1e-6)
  expect_error(Kdfr1(kernel = 3), "gaussian")
  r <- Kdfmise(n = 80, h = 0.25, rfp = 0.3, varint = 0.4, mu2 = 1.5)
  expect_equal(r$mise, 0.25^4 / 4 * 2.25 * 0.3 + 0.4 / 80 - 2 * 0.25 / 80 * r1,
               tolerance = 1e-12)
  expect_equal(r$hopt, (2 * r1 / (80 * 2.25 * 0.3))^(1 / 3), tolerance = 1e-12)
  expect_true(is.na(Kdfmise(80, 0.2, rfp = 0, varint = 1)$hopt))
  expect_error(Kdfmise(80, 0, 1, 1), "bandwidth")
  y <- seq(-8, 8, length.out = 4001)
  a <- 2
  term <- dnorm(y) * pnorm(y / a) + pnorm(y) * dnorm(y / a) / a
  expect_equal(Kdfr2(a = 2)$estimate, trap_ref(y * term, y), tolerance = 1e-12)
  expect_equal(Kdfr2(a = 2, kernel = dnorm)$estimate, Kdfr2(a = 2)$estimate,
               tolerance = 1e-6)
  expect_error(Kdfr2(a = 1), "excluded")
  expect_error(Kdfr2(a = -1), "positive")
  expect_error(Kdfr2(a = 2, kernel = 1), "gaussian")
})

test_that("Ksstat computes D+ and D- and a two-sided p-value", {
  expect_same_function(morie_fauzi_ks_statistic, Ksstat)
  x <- c(0.9, 0.12, 0.33, 0.47, 0.61, 0.05, 0.78, 0.2)
  n <- 8
  xs <- sort(x)
  r <- Ksstat(x, punif)
  expect_equal(r$dplus, max((1:n) / n - xs), tolerance = 1e-12)
  expect_equal(r$dminus, max(xs - (0:(n - 1)) / n), tolerance = 1e-12)
  expect_equal(r$statistic, max(r$dplus, r$dminus), tolerance = 1e-12)
  ref <- stats::ks.test(x, "punif", exact = TRUE)
  expect_equal(r$statistic, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(r$p_value, ref$p.value, tolerance = 1e-9)
  x30 <- ((1:30) - 0.5)^1.3 / 30^1.3
  expect_equal(Ksstat(x30, punif)$p_value,
               stats::ks.test(x30, "punif", exact = TRUE)$p.value, tolerance = 1e-9)
  xl <- ((1:60) - 0.3)^1.1 / 60^1.1
  rl <- Ksstat(xl, punif)
  lam <- (sqrt(60) + 0.12 + 0.11 / sqrt(60)) * rl$statistic
  k <- 1:100
  expect_equal(rl$p_value, min(1, max(0, 2 * sum((-1)^(k - 1) * exp(-2 * k^2 * lam^2)))),
               tolerance = 1e-12)
  expect_error(Ksstat(1, punif), "two")
  expect_error(Ksstat(x, "punif"), "function")
})

test_that("Lstat weights order statistics by the integrated score", {
  x <- c(3, -1, 4, 1.5, 9, 2.6, 5)
  n <- 7
  expect_equal(Lstat(x)$estimate, mean(x), tolerance = 1e-12)
  r <- Lstat(x, score = function(u) 2 * u)
  w <- ((1:n) / n)^2 - ((0:(n - 1)) / n)^2
  expect_equal(r$estimate, sum(w * sort(x)), tolerance = 1e-12)
  uu <- (seq_len(50) - 0.5) / 50
  q <- quantile(x, uu, names = FALSE)
  h <- 1.06 * sd(x) * n^(-1 / 5)
  fq <- vapply(q, function(t) mean(dnorm((t - x) / h) / h), numeric(1))
  kmat <- (outer(uu, uu, pmin) - outer(uu, uu)) / (outer(fq, fq) + 1e-12)
  expect_equal(Lstat(x, n_quad = 50)$se, sqrt(max(mean(kmat) / n, 0)),
               tolerance = 1e-12)
  expect_true(is.na(Lstat(1)$estimate))
})

test_that("Mgkde is A_h^2 / A_4h with gamma kernels", {
  expect_same_function(morie_fauzi_modified_gamma_kde, Mgkde)
  x <- c(0.4, 1.1, 1.7, 2.2, 3.5)
  ag <- function(v, h) vapply(v, function(p)
    mean(dgamma(x, shape = 1 / sqrt(h), scale = p * sqrt(h) + h)), numeric(1))
  r <- Mgkde(x, grid = c(0.5, 1.5), h = 0.04)
  expect_equal(r$estimate, ag(c(0.5, 1.5), 0.04)^2 / ag(c(0.5, 1.5), 0.16),
               tolerance = 1e-12)
  expect_error(Mgkde(numeric(0), 1, 0.1), "at least one")
  expect_error(Mgkde(x, 1, 0), "positive")
})

test_that("Hdecmom is c n^(qk/2) E|rho|^q", {
  expect_same_function(morie_fauzi_moment_ineq_ustat, Hdecmom)
  r <- Hdecmom(n = 30, k = 3, q = 4, rhomom = 0.2, c = 1.5)
  expect_equal(r$bound, 1.5 * 30^6 * 0.2, tolerance = 1e-12)
  expect_equal(r$naive, 12)
  expect_error(Hdecmom(30, 0, 4), "order")
  expect_error(Hdecmom(30, 1, 1), "q >= 2")
})

test_that("Kerncvm and Kernks smooth the empirical CDF", {
  expect_same_function(morie_fauzi_naive_kernel_cvm, Kerncvm)
  expect_same_function(morie_fauzi_naive_kernel_ks, Kernks)
  x <- c(0.9, 0.12, 0.33, 0.47, 0.61, 0.05, 0.78, 0.2)
  u <- (1:101 - 0.5) / 101
  integrand <- vapply(u, function(v) (mean(pnorm((v - x) / 0.1)) - v)^2, numeric(1))
  r <- Kerncvm(x, qunif, h = 0.1, ngrid = 101)
  expect_equal(r$statistic, 8 * mean(integrand), tolerance = 1e-12)
  expect_error(Kerncvm(x, 1), "function")
  expect_error(Kerncvm(x, qunif, h = -1), "positive")
  g <- seq(min(x) - 0.4, max(x) + 0.4, length.out = 201)
  d <- abs(vapply(g, function(t) mean(pnorm((t - x) / 0.1)), numeric(1)) - punif(g))
  rk <- Kernks(x, punif, h = 0.1, ngrid = 201)
  expect_equal(rk$statistic, max(d), tolerance = 1e-12)
  expect_equal(rk$argmax, g[which.max(d)], tolerance = 1e-12)
  expect_error(Kernks(x, 1), "function")
})

test_that("Ssgnmom and Swilmom give the smoothed test moments", {
  expect_same_function(morie_fauzi_sign_moments, Ssgnmom)
  expect_same_function(morie_fauzi_wilcoxon_moments, Swilmom)
  r <- Ssgnmom(n = 40, ftheta = 0.3)
  expect_equal(c(r$mean, r$variance), c(12, 40 * 0.7 * 0.3), tolerance = 1e-12)
  rr <- Ssgnmom(n = 40, h = 0.1, f0 = 0.4, fpp0 = -0.2, a11 = 0.3, a13 = 0.1)
  expect_equal(rr$variance, 10 - 80 * 0.1 * 0.4 * 0.3 - 40 * 0.001 / 3 * -0.2 * 0.1,
               tolerance = 1e-12)
  expect_true(rr$refined)
  expect_error(Ssgnmom(40, ftheta = 2), "\\[0, 1\\]")
  w <- Swilmom(n = 12, gtheta = 0.4, projint = 0.3)
  expect_equal(w$mean, 12 * 13 / 2 * 0.4, tolerance = 1e-12)
  expect_equal(w$variance, 12 * 169 * (0.3 - 0.16), tolerance = 1e-12)
  expect_true(is.na(Swilmom(n = 5, gtheta = 1, projint = 0.5)$se))
  expect_error(Swilmom(0), "at least 1")
})

test_that("Smpqnt returns the inf{t: F_n(t) >= p} order statistic", {
  expect_same_function(morie_fauzi_sample_quantile, Smpqnt)
  x <- c(5, 1, 4, 2, 3)
  r <- Smpqnt(x, p = c(0.2, 0.21, 1))
  expect_equal(r$estimate, c(1, 2, 5))
  expect_equal(r$estimate, unname(quantile(x, c(0.2, 0.21, 1), type = 1)))
  expect_error(Smpqnt(x, 0), "\\(0, 1\\]")
})

test_that("Gkrawbv, Gkgeoext, Mgkbias and Mgkvar (Theorems 1.1-1.5)", {
  expect_same_function(morie_fauzi_thm1_1_bias_mgkde, Gkrawbv)
  expect_same_function(morie_fauzi_thm1_2_var_mgkde, Gkgeoext)
  expect_same_function(morie_fauzi_thm1_3_mise_mgkde, Mgkbias)
  expect_same_function(morie_fauzi_thm1_5_consistency_mgkde, Mgkvar)
  h <- 0.04
  rh <- 0.2
  r <- Gkrawbv(x = 1.5, h = h, n = 100, fp = 0.1, fpp = -0.3, f = 0.25)
  expect_equal(r$bias, (0.1 + 0.5 * 2.25 * -0.3) * rh, tolerance = 1e-12)
  rn <- rratio_ref(1 / rh - 1)
  rd <- rratio_ref(2 / rh - 2)
  v <- rn^2 * 0.25 / (2 * (1.5 + rh) * sqrt(pi) * (1 - rh) * rd * 100 * h^0.25)
  expect_equal(r$variance, v, tolerance = 1e-12)
  rb <- Gkrawbv(x = 0, h = h, n = 100, fp = 0.1, fpp = -0.3, f = 0.25,
                boundary = TRUE, c = 2)
  expect_equal(rb$variance,
               rn^2 * 0.25 / (2 * (2 * rh + 1) * sqrt(pi) * (1 - rh) * rd * 100 * h^0.75),
               tolerance = 1e-12)
  expect_error(Gkrawbv(0, h, 100, 0, 0, 1, boundary = TRUE), "needs c")
  expect_error(Gkrawbv(-1, h, 100, 0, 0, 1), "x >= 0")
  g <- Gkgeoext(jh = 0.3, j4h = 0.6, h = 0.1, a = 0.2, b = 0.5, f = 0.4)
  expect_equal(g$estimate, 0.09 / 0.6, tolerance = 1e-12)
  expect_equal(g$remainder, -2 * (0.5 - 0.04 / 0.8) * 0.1, tolerance = 1e-12)
  expect_error(Gkgeoext(1, 0), "non-zero")
  m <- Mgkbias(x = 1.2, h = 0.05, f = 0.4, fp = 0.1, fpp = -0.2, fppp = 0.3)
  a <- 0.1 + 0.5 * 1.44 * -0.2
  b <- 1.2 + 0.5 * -0.2 + 1.44 * (0.4 + 0.5) * 0.3
  expect_equal(m$bias, -2 * (b - a^2 / 0.8) * 0.05, tolerance = 1e-12)
  m2 <- Mgkbias(x = 1.2, h = 0.05, f = 0.4, fp = 0.1, fpp = -0.2, fppp = 0.3, book = FALSE)
  expect_equal(m2$b, 1.7 * -0.2 + 1.44 * 0.9 * 0.3, tolerance = 1e-12)
  expect_error(Mgkbias(1, 0.1, 0, 0, 0, 0), "non-zero")
  mv <- Mgkvar(varh = 0.03, var4h = 0.02, cov = 0.01, n = 64)
  expect_equal(mv$variance, 0.12 + 0.02 - 0.04, tolerance = 1e-12)
  expect_equal(mv$hopt, 64^(-4 / 9), tolerance = 1e-12)
  expect_equal(Mgkvar(0.03, 0.02, 0.01, n = 64, boundary = TRUE)$mserate,
               64^(-8 / 11), tolerance = 1e-12)
  expect_true(is.na(Mgkvar(0.03, 0.02, 0.01)$hopt))
})

test_that("Gkcov closed form, plug-in density and sample forms", {
  expect_same_function(morie_fauzi_thm1_4_asympnorm_mgkde, Gkcov)
  h <- 0.01
  s <- 0.1
  r <- Gkcov(x = 1, h = h, n = 50, f = 0.3)
  lv <- log(rratio_ref(1 / s - 1)) + log(rratio_ref(1 / (2 * s) - 1)) -
    log(rratio_ref(3 / (2 * s) - 2)) + (3 / (2 * s) - 1.5) * log(1.5 - 2 * s) -
    0.5 * log(pi) - log(2) - log(3 + 5 * s) - (1 / s - 0.5) * log(2 - 2 * s) -
    (1 / (2 * s) - 0.5) * log(1 - 2 * s) +
    (1 / (2 * s) - 1) * log((1 + s) / (3 + 5 * s)) +
    (1 / s - 1) * log((2 + 4 * s) / (3 + 5 * s))
  expect_equal(r$covariance, exp(lv) * 0.3 / (50 * h^0.25), tolerance = 1e-12)
  expect_identical(Gkcov(0, h, 50, f = 0.3, boundary = TRUE, c = 1)$form, "boundary")
  w <- c(0.3, 0.8, 1.1, 1.6, 2.4)
  k1 <- dgamma(w, shape = 1 / sqrt(h), scale = sqrt(h) + h)
  k2 <- dgamma(w, shape = 1 / sqrt(4 * h), scale = sqrt(4 * h) + 4 * h)
  rs <- Gkcov(x = 1, h = h, n = 50, sample = w)
  expect_equal(rs$covariance, (mean(k1 * k2) - mean(k1) * mean(k2)) / 50,
               tolerance = 1e-12)
  rd <- Gkcov(x = 1, h = h, n = 50, density = dexp, upper = 10, ngrid = 501)
  g <- seq(0, 10, length.out = 501)
  j1 <- trap_ref(dgamma(g, shape = 10, scale = 0.11) * dexp(g), g)
  expect_equal(rd$jh, j1, tolerance = 1e-12)
  expect_error(Gkcov(1, h, 50), "exactly one")
  expect_error(Gkcov(1, 0.3, 50, f = 1), "h < 1/4")
  expect_error(Gkcov(1, h, 50, sample = -1), "\\[0")
})

test_that("Kdfgeoext, Gekdfbias, Gekdfvar and Gekdfmise (Theorems 2.1-2.4)", {
  expect_same_function(morie_fauzi_thm2_1_expected_kdfe, Kdfgeoext)
  expect_same_function(morie_fauzi_thm2_2_bias_brdkdfe, Gekdfbias)
  expect_same_function(morie_fauzi_thm2_3_var_brdkdfe, Gekdfvar)
  expect_same_function(morie_fauzi_thm2_4_mise_brdkdfe, Gekdfmise)
  r <- Kdfgeoext(jh = 0.4, jah = 0.6, a = 3)
  expect_equal(r$estimate, 0.4^(9 / 8) * 0.6^(-1 / 8), tolerance = 1e-12)
  expect_error(Kdfgeoext(0.4, 0.6, 1), "excluded")
  expect_error(Kdfgeoext(0, 0.6, 2), "positive")
  b <- Gekdfbias(h = 0.2, a = 2, b2 = 0.3, b4 = 0.1, fx = 0.4)
  expect_equal(b$bias, 0.2^4 * 4 * (0.09 - 0.08) / 0.8, tolerance = 1e-12)
  expect_error(Gekdfbias(0.2, 2, 0, 0, 1), "strictly")
  r1 <- 1 / (2 * sqrt(pi))
  v <- Gekdfvar(n = 60, h = 0.2, a = 2, fx = 0.4, density = 0.3, r2 = 0.5)
  expect_equal(v$variance, 0.24 / 60 - 0.2 / 60 * (2 * 17 / 9 * r1 + 0.5) * 0.3,
               tolerance = 1e-12)
  expect_equal(Gekdfvar(60, 0.2, 2, 0.4, 0.3)$r2, Kdfr2(a = 2)$estimate,
               tolerance = 1e-12)
  expect_error(Gekdfvar(60, 0.2, 1, 0.4, 0.3), "close to 1")
  m <- Gekdfmise(n = 60, h = 0.2, a = 2, biasint = 0.7, varint = 0.5, r2 = 0.5)
  expect_equal(m$mise, 0.2^8 * 16 * 0.7 + 0.5 / 60 - 0.2 / 60 * (2 * 17 / 9 * r1 + 0.5),
               tolerance = 1e-12)
  expect_error(Gekdfmise(0, 0.2, 2, 1, 1), "at least 1")
})

test_that("Srvbv1 and Srvbv2 (Theorems 4.1-4.2)", {
  expect_same_function(morie_fauzi_thm4_1_surv_bias_var, Srvbv1)
  expect_same_function(morie_fauzi_thm4_2_surv2_bias_var, Srvbv2)
  r <- Srvbv1(t = 1, n = 50, h = 0.2, surv = 0.4, cdf = 0.6, cumsurv = 0.5,
              b1 = 0.2, b2 = 0.3, dg = 1.5, density = 0.35)
  expect_equal(r$biassurv, -0.02 * 0.2, tolerance = 1e-12)
  expect_equal(r$varsurv, 0.24 / 50 - 0.2 / 50 * 1.5 * 0.35 / sqrt(pi),
               tolerance = 1e-12)
  expect_equal(r$varcum, (1 - 0.16) / 50, tolerance = 1e-12)
  expect_error(Srvbv1(1, 50, 0, 0.4, 0.6, 0.5, 0, 0, 1, 1), "bandwidth")
  r2 <- Srvbv2(t = 1, n = 50, h = 0.2, surv = 0.4, cumsurv = 0.5, dg = 1.5,
               d2g = 0.5, density = 0.35)
  b3 <- 2.25 * 0.35 - 0.5 * 0.4
  expect_equal(r2$bias, 0.02 * b3, tolerance = 1e-12)
  expect_equal(r2$cov, 0.24 / 50, tolerance = 1e-12)
  expect_error(Srvbv2(1, 0, 0.2, 0.4, 0.5, 1, 0, 1), "at least 1")
})

test_that("Theorem 5.x helpers", {
  expect_same_function(morie_fauzi_thm5_1_naive_kernel_equiv, Kerngofeq)
  expect_same_function(morie_fauzi_thm5_2_bdfree_kdfe_bv, Bfkdfbv)
  expect_same_function(morie_fauzi_thm5_3_bdfree_normality, Bfkdfnorm)
  expect_same_function(morie_fauzi_thm5_5_bdfree_kde_bv, Bfkdebv)
  expect_same_function(morie_fauzi_thm5_6_bdfree_ks_equiv, Bfkseq)
  expect_same_function(morie_fauzi_thm5_7_bdfree_cvm_equiv, Bfcvmeq)
  expect_same_function(morie_fauzi_thm5_8_smoothed_convergence, Smthconv)
  expect_same_function(morie_fauzi_thm5_9_edgeworth_wilcoxon, Smthedge)
  k <- Kerngofeq(0.2, 0.3, 0.1, 0.12, tol = 0.05)
  expect_equal(k$ksdiff, 0.1, tolerance = 1e-12)
  expect_false(k$close)
  expect_error(Kerngofeq(0, 0, 0, 0, tol = 0), "positive")
  r1 <- 1 / (2 * sqrt(pi))
  b <- Bfkdfbv(n = 40, h = 0.3, fx = 0.2, density = 0.5, c1 = -0.4, dg = 2)
  expect_equal(b$bias, 0.045 * -0.4, tolerance = 1e-12)
  expect_equal(b$variance, 0.16 / 40 - 0.6 / 40 * 2 * 0.5 * r1, tolerance = 1e-12)
  expect_error(Bfkdfbv(40, 0, 0.2, 0.5, 1, 1), "bandwidth")
  z <- Bfkdfnorm(estimate = 0.6, variance = 0.01, null = 0.5, bias = 0.02, level = 0.9)
  expect_equal(z$statistic, (0.58 - 0.5) / 0.1, tolerance = 1e-12)
  expect_equal(z$p_value, 2 * pnorm(-0.8), tolerance = 1e-12)
  expect_equal(z$lower, 0.58 - qnorm(0.95) * 0.1, tolerance = 1e-12)
  expect_true(is.na(Bfkdfnorm(0.5, 0.01)$statistic))
  expect_error(Bfkdfnorm(0.5, 0), "positive")
  expect_error(Bfkdfnorm(0.5, 1, level = 1), "strictly")
  d <- Bfkdebv(n = 50, h = 0.3, density = 0.4, c2 = -0.1, dg = 1.2)
  expect_equal(d$bias, 0.09 * -0.1 / 2.4, tolerance = 1e-12)
  expect_equal(d$variance, 0.4 * r1 / (50 * 0.3 * 1.2), tolerance = 1e-12)
  expect_equal(d$hopt, (0.4 * r1 / (1.2 * 4 * (-0.1 / 2.4)^2 * 50))^0.2,
               tolerance = 1e-12)
  expect_true(is.na(Bfkdebv(50, 0.3, 0.4, c2 = 0, dg = 1)$hopt))
  expect_error(Bfkdebv(50, 0.3, 0.4, 0.1, dg = 0), "increasing")
  e1 <- Bfkseq(0.2, 0.4, h = 0.5, n = 16)
  expect_equal(e1$difference, 0.2, tolerance = 1e-12)
  expect_false(e1$close)
  expect_false(e1$bwok)
  expect_true(is.na(Bfkseq(0.2, 0.21)$bwok))
  expect_error(Bfkseq(0, 0, tol = -1), "positive")
  e2 <- Bfcvmeq(0.2, 0.22, h = 0.1, n = 16)
  expect_true(e2$close && e2$bwok)
  expect_error(Bfcvmeq(0, 0, tol = 0), "positive")
  s <- Smthconv(d = 0.3, c = 2, n = 100, zstd = 1.5, zsmooth = 1.2)
  expect_equal(s$h, 2 * 100^-0.3, tolerance = 1e-12)
  expect_equal(s$sqdiff, 0.09, tolerance = 1e-12)
  expect_false(Smthconv(d = 0.6)$ok)
  expect_error(Smthconv(0.3, c = 0), "positive")
  y <- c(-1, 0.4, 2)
  sg <- Smthedge(y, n = 30)
  expect_equal(sg$estimate, pnorm(y) - (y^3 - 3 * y) * dnorm(y) / 720, tolerance = 1e-12)
  sw <- Smthedge(y, n = 30, which = "wilcoxon")
  expect_equal(sw$correction, (y^3 - 3 * y) * dnorm(y) / 600, tolerance = 1e-12)
  sb <- Smthedge(y, n = 30, which = "wilcoxon", book = TRUE)
  expect_equal(sb$correction, (0.35 * y^3 - 1.05 * y) * dnorm(y), tolerance = 1e-12)
  expect_error(Smthedge(0, 30, which = "x"), "sign")
})

test_that("Edf is the empirical CDF with binomial standard errors", {
  expect_same_function(morie_fauzi_ecdf, Edf)
  x <- c(3, 1, 4, 1, 5, 9, 2)
  r <- Edf(x, grid = c(0, 1, 3.5, 9))
  est <- stats::ecdf(x)(c(0, 1, 3.5, 9))
  expect_equal(r$estimate, est, tolerance = 1e-12)
  expect_equal(r$se, sqrt(est * (1 - est) / 7), tolerance = 1e-12)
  expect_equal(Edf(x)$grid, sort(x))
  expect_error(Edf(numeric(0)), "at least one")
})
