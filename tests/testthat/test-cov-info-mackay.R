# Coverage tests for R/info_mackay.R (MacKay 2003). Expected values are
# recomputed from the printed equations with base R distributions.

test_that("R3 posterior and the repetition-code approximations", {
  r <- morie_r3post(c(1, 0, 1), 0.1)
  l1 <- 0.9 * 0.1 * 0.9
  l0 <- 0.1 * 0.9 * 0.1
  expect_equal(r$p1, l1 / (l0 + l1), tolerance = 1e-12)
  expect_equal(r$decoded, 1L)
  expect_equal(r$gamma, 9, tolerance = 1e-12)
  expect_equal(r$evidence, (l0 + l1) / 2, tolerance = 1e-12)
  expect_error(morie_r3post(c(1, 2, 0), 0.1), "three bits")
  expect_error(morie_r3post(c(1, 0, 0), 1), "strictly")
  cb <- morie_cbcapx(20)
  expect_equal(cb$exact, choose(20, 10))
  expect_equal(cb$approx, 2^20 / sqrt(2 * pi * 5), tolerance = 1e-12)
  expect_equal(cb$logapprox, log(cb$approx), tolerance = 1e-12)
  bs <- morie_binsumga(20)
  expect_equal(bs$total, choose(20, 10) / 2^20 * sqrt(2 * pi * 5), tolerance = 1e-12)
  expect_equal(bs$cbcapprox, cb$approx, tolerance = 1e-12)
  expect_error(morie_binsumga(0), "at least 1")
  rp <- morie_repcpb(9, 0.1)
  expect_equal(rp$leading, dbinom(5, 9, 0.1), tolerance = 1e-12)
  expect_equal(rp$approx2, 0.1 * (0.36)^4 / sqrt(pi * 9 / 8), tolerance = 1e-12)
  expect_equal(rp$logapprox2, log2(rp$approx2), tolerance = 1e-12)
  expect_error(morie_repcpb(4, 0.1), "odd")
})

test_that("repetition length iteration solves eq 1.50 by substitution", {
  r <- morie_repcn(1e-15, 0.1, n0 = 68, iters = 4)
  n <- 68
  for (i in 1:4) {
    half <- (log10(1e-15) + log10(sqrt(pi * n / 8) / 0.1)) / log10(0.36)
    n <- 2 * half + 1
  }
  expect_equal(r$n, n, tolerance = 1e-12)
  expect_equal(r$half, half, tolerance = 1e-12)
  expect_error(morie_repcn(1e-15, 0.6), "0.5")
})

test_that("urn posterior and predictive probability", {
  r <- morie_urnpost(3, 10, 10)
  u <- 0:10
  joint <- dbinom(3, 10, u / 10) / 11
  expect_equal(r$posterior, joint / sum(joint), tolerance = 1e-12)
  expect_equal(r$evidence, sum(joint), tolerance = 1e-12)
  expect_equal(r$map, 3L)
  p <- morie_urnpred(3, 10, 10)
  expect_equal(p$p, sum(u / 10 * joint / sum(joint)), tolerance = 1e-12)
  expect_equal(p$pmap, 0.3)
  expect_error(morie_urnpost(11, 10), "nb <= ntot")
})

test_that("bent coin likelihood, prior, Laplace rule and Bayes factor", {
  l <- morie_bcoinlik(0.3, 4, 6)
  expect_equal(l$likelihood, 0.3^4 * 0.7^6, tolerance = 1e-12)
  expect_equal(l$loglik, log(0.3^4 * 0.7^6), tolerance = 1e-12)
  expect_equal(morie_bcoinlik(0, 0, 3)$loglik, 0)
  expect_error(morie_bcoinlik(1.2, 1, 1), "pa in")
  expect_equal(morie_bcoinpri(0.4)$density, 1)
  expect_equal(morie_bcoinpri(1.4)$logdensity, -Inf)
  s <- morie_sucrule(4, 6)
  expect_equal(s$p, 5 / 12, tolerance = 1e-12)
  expect_equal(s$mle, 0.4)
  expect_true(is.na(morie_sucrule(0, 0)$mle))
  expect_error(morie_sucrule(-1, 0), "non-negative")
  bf <- morie_bcoinbf(4, 6, 1 / 6)
  e1 <- beta(5, 7)
  e0 <- (1 / 6)^4 * (5 / 6)^6
  expect_equal(bf$evidence1, e1, tolerance = 1e-12)
  expect_equal(bf$ratio, e1 / e0, tolerance = 1e-12)
  expect_error(morie_bcoinbf(-1, 2), "non-negative")
})

test_that("evidence mixtures, posterior odds and likelihood-ratio products", {
  m <- morie_evidmix(c(0.02, 0.05, 0.01), c(0.5, 0.3, 0.2))
  expect_equal(m$evidence, 0.01 + 0.015 + 0.002, tolerance = 1e-12)
  expect_equal(m$posterior, c(0.01, 0.015, 0.002) / 0.027, tolerance = 1e-12)
  expect_error(morie_evidmix(1:2, 1), "equal length")
  o <- morie_postodds(0.2, 0.05, 0.3, 0.7)
  expect_equal(o$odds, 0.06 / 0.035, tolerance = 1e-12)
  expect_equal(o$p1, 0.06 / 0.095, tolerance = 1e-12)
  expect_equal(o$bayesfactor, 4)
  lr <- morie_lrprod(c(0.2, 0.9, 0.4), c(0.1, 0.3, 0.8))
  expect_equal(lr$ratio, 2 * 3 * 0.5, tolerance = 1e-12)
  expect_equal(lr$p1, 3 / 4, tolerance = 1e-12)
  expect_error(morie_lrprod(1, numeric(0)), "equal length")
})

test_that("typical set membership and Gaussian channel posterior", {
  ts <- morie_typset(0.2^3 * 0.8^7, 10, -0.2 * log2(0.2) - 0.8 * log2(0.8), 0.1)
  info <- -log2(0.2^3 * 0.8^7)
  expect_equal(ts$info, info, tolerance = 1e-12)
  expect_equal(ts$deviation, info / 10 - (-0.2 * log2(0.2) - 0.8 * log2(0.8)), tolerance = 1e-12)
  expect_false(ts$member)
  expect_true(morie_typset(0.2^3 * 0.8^7, 10, -0.2 * log2(0.2) - 0.8 * log2(0.8), 0.25)$member)
  expect_error(morie_typset(0, 10, 1, 0.1), "0 < p")
  g <- morie_gchpost(1.5, 4, 1)
  expect_equal(g$mean, 4 / 5 * 1.5, tolerance = 1e-12)
  expect_equal(g$var, 4 / 5, tolerance = 1e-12)
  expect_equal(g$wdata, 4 / 5, tolerance = 1e-12)
  expect_error(morie_gchpost(1, 0, 1), "positive")
})

test_that("sex-versus-mutation fitness dynamics, ch. 19", {
  expect_equal(morie_sexbeta(0.25)$beta, 1 / 3, tolerance = 1e-12)
  expect_error(morie_sexbeta(1), "\\[0, 1\\)")
  eta <- sqrt(2 / (pi + 2))
  expect_equal(morie_sexdfdt(0.3, 100)$dfbardt, eta * sqrt(0.21 * 100), tolerance = 1e-12)
  expect_equal(morie_sexdfdt(0.3, 100, eta = 1)$dfbardt, sqrt(21), tolerance = 1e-12)
  s <- morie_sexfsol(5, 100, 0.2)
  cc <- 10 / eta * asin(-0.6)
  expect_equal(s$f, 0.5 * (1 + sin(eta * (5 + cc) / 10)), tolerance = 1e-12)
  expect_equal(s$tperfect, pi / eta * 10, tolerance = 1e-12)
  # the closed form solves df/dt = eta sqrt(f (1 - f) / G), i.e. dF/dt with F = G f;
  # central difference with h = 1e-5 is accurate to ~1e-10
  h <- 1e-5
  fp <- (morie_sexfsol(5 + h, 100, 0.2)$f - morie_sexfsol(5 - h, 100, 0.2)$f) / (2 * h)
  expect_equal(fp, eta * sqrt(s$f * (1 - s$f) / 100), tolerance = 1e-8)
  expect_error(morie_sexfsol(1, 0, 0.2), "G > 0")
})

test_that("Gaussian likelihood by sufficient statistics and the evidence for sigma", {
  x <- c(1.2, 0.4, 2.2, 1.9, 0.8)
  xb <- mean(x)
  S <- sum((x - xb)^2)
  g <- morie_gllsuff(xb, S, 5, 1.1, 0.7)
  expect_equal(g$loglik, sum(dnorm(x, 1.1, 0.7, log = TRUE)), tolerance = 1e-12)
  expect_equal(g$sigman, sqrt(S / 5), tolerance = 1e-12)
  m <- morie_mupostsg(xb, 5, 0.7)
  expect_equal(m$se, 0.7 / sqrt(5), tolerance = 1e-12)
  expect_error(morie_mupostsg(xb, 0, 1), "n >= 1")
  e <- morie_sigevid(S, 5, 0.7, 2)
  expect_equal(e$bestfit, sum(dnorm(x, xb, 0.7, log = TRUE)), tolerance = 1e-12)
  expect_equal(e$logoccam, log(sqrt(2 * pi) * 0.7 / sqrt(5) / 2), tolerance = 1e-12)
  expect_error(morie_sigevid(S, 5, 0.7, 0), "sigmamu")
})

test_that("Laplace posterior ratio, Occam factors, message lengths", {
  A <- rbind(c(4, 1), c(1, 3))
  dw <- c(0.5, -0.2)
  p <- morie_postgapx(dw, A)
  expect_equal(p$quadform, sum(dw * (A %*% dw)), tolerance = 1e-12)
  expect_equal(p$errorbars, sqrt(diag(solve(A))), tolerance = 1e-12)
  expect_error(morie_postgapx(1:3, A), "square")
  ev <- morie_evratio(c(2, 4, 0.5))
  expect_equal(ev$ratio, 1 / 4, tolerance = 1e-12)
  expect_error(morie_evratio(c(1, 0)), "positive")
  expect_equal(morie_msglen(p = 1 / 8)$length, 3, tolerance = 1e-12)
  expect_equal(morie_msglen(length = 5)$p, 1 / 32)
  expect_equal(morie_msglen(p = 0.5)$nats, log(2), tolerance = 1e-12)
  expect_error(morie_msglen(), "exactly one")
  md <- morie_mdlpost(0.25, 0.01, 0.5)
  expect_equal(md$total, 2 + -log2(0.005), tolerance = 1e-12)
  expect_error(morie_mdlpost(0, 1), "0 < ph")
})

test_that("linear-model evidence is the Gaussian marginal of t", {
  x <- c(-1, 0.2, 1.1, 2.3)
  t <- c(0.5, 1.4, 2.1, 3.9)
  C <- 1.5^2 * cbind(1, x) %*% rbind(1, x) + 0.4^2 * diag(4)
  lev <- -0.5 * (4 * log(2 * pi) + log(det(C)) + sum(t * solve(C, t)))
  r <- morie_linevid(x, t, sigma = 0.4, priorsd = 1.5)
  expect_equal(r$logevidence, lev, tolerance = 1e-12)
  expect_equal(r$k, 2L)
  C0 <- 1.5^2 * matrix(1, 4, 4) + 0.4^2 * diag(4)
  r0 <- morie_linevid(x, t, sigma = 0.4, slope = FALSE, priorsd = 1.5)
  expect_equal(r0$logevidence, -0.5 * (4 * log(2 * pi) + log(det(C0)) + sum(t * solve(C0, t))), tolerance = 1e-12)
  expect_error(morie_linevid(x, t[-1]), "same length")
})

test_that("uniform sampling needs 2^(N-H) samples", {
  r <- morie_rminsamp(1000, 200)
  expect_equal(r$log2rmin, 800)
  expect_equal(r$log10rmin, 800 * log10(2), tolerance = 1e-12)
  expect_equal(r$rmin, 2^800)
  expect_equal(morie_rminsamp(3000, 1000)$rmin, Inf)
  expect_error(morie_rminsamp(10, 11), "h <= n")
})
