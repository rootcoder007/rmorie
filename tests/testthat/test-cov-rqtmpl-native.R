# Interval mapping (Lander & Botstein 1989) recomputed from the
# backcross mixture likelihood, lm() and the normal quantile.

rq_data <- function() {
  set.seed(5)
  n <- 40
  left <- stats::rbinom(n, 1, 0.5)
  right <- ifelse(stats::runif(n) < 0.15, 1 - left, left)
  y <- 1 + 0.9 * left + stats::rnorm(n, sd = 0.7)
  list(y = y, left = left, right = right, n = n)
}

test_that("Haldane map function and its inverse", {
  for (d in c(0, 0.1, 0.35, 2)) {
    r <- morie_haldane(d)
    expect_equal(r, 0.5 * (1 - exp(-2 * d)), tolerance = 1e-12)
    if (r < 0.5) expect_equal(morie_inverse_haldane(r), d, tolerance = 1e-12)
  }
  expect_error(morie_haldane(-0.1), "negative")
  expect_error(morie_inverse_haldane(0.5), "0.5")
})

test_that("QTL genotype probabilities are Bayes over the flanking markers", {
  rl <- 0.1
  rr <- 0.25
  for (L in 0:1) for (R in 0:1) {
    lik <- vapply(0:1, function(q) (if (L == q) 1 - rl else rl) *
                    (if (R == q) 1 - rr else rr), numeric(1))
    expect_equal(morie_genotype_probabilities(L, R, rl, rr), lik / sum(lik),
                 tolerance = 1e-12)
  }
  expect_error(morie_genotype_probabilities(0, 1, 0.6, 0.1), "\\[0, 0.5\\]")
  expect_error(morie_genotype_probabilities(0, 1, 0.1, -0.1), "\\[0, 0.5\\]")
  expect_error(morie_genotype_probabilities(0, 1, 0, 0), "probability zero")
})

test_that("single-marker LOD equals (n/2) log10(RSS0/RSS1) from lm", {
  d <- rq_data()
  s <- morie_single_marker(d$y, d$left)
  fit <- stats::lm(d$y ~ d$left)
  rss1 <- sum(stats::residuals(fit)^2)
  rss0 <- sum((d$y - mean(d$y))^2)
  expect_equal(s$lod, d$n / 2 * log10(rss0 / rss1), tolerance = 1e-12)
  expect_equal(s$lod_likelihood, s$lod, tolerance = 1e-10)
  expect_equal(c(s$a, s$b), unname(stats::coef(fit)), tolerance = 1e-12)
  expect_equal(s$sigma2, rss1 / d$n, tolerance = 1e-12)
  expect_error(morie_single_marker(1:4, 1:3), "same length")
  expect_error(morie_single_marker(1:2, 1:2), "three")
  expect_error(morie_single_marker(1:4, rep(1, 4)), "monomorphic")
})

test_that("EM interval mapping reaches a fixed point of the mixture likelihood", {
  d <- rq_data()
  rl <- 0.05
  rr <- 0.1
  f <- morie_interval_map(d$y, d$left, d$right, rl, rr, max_iter = 2000, tol = 1e-13)
  G <- t(mapply(morie_genotype_probabilities, d$left, d$right, rl, rr))
  ll <- function(a, b, s2) {
    sum(log(G[, 1] * stats::dnorm(d$y, a, sqrt(s2)) +
              G[, 2] * stats::dnorm(d$y, a + b, sqrt(s2))))
  }
  expect_equal(f$loglik, ll(f$a, f$b, f$sigma2), tolerance = 1e-9)
  expect_true(all(diff(f$loglik_history) > -1e-9))
  # stationarity: small moves in any parameter do not increase the loglik
  h <- 1e-4
  for (dv in list(c(h, 0, 0), c(-h, 0, 0), c(0, h, 0), c(0, -h, 0),
                  c(0, 0, h), c(0, 0, -h))) {
    expect_lte(ll(f$a + dv[1], f$b + dv[2], f$sigma2 + dv[3]), f$loglik + 1e-8)
  }
  s0 <- mean((d$y - mean(d$y))^2)
  ll0 <- sum(stats::dnorm(d$y, mean(d$y), sqrt(s0), log = TRUE))
  expect_equal(f$loglik_null, ll0, tolerance = 1e-10)
  expect_equal(f$lod, (f$loglik - ll0) / log(10), tolerance = 1e-10)
  # at a fully informative marker the mixture collapses to the regression
  k <- morie_interval_map(d$y, d$left, d$left, 0, 0, max_iter = 2000, tol = 1e-13)
  expect_equal(k$lod, morie_single_marker(d$y, d$left)$lod, tolerance = 1e-8)
  expect_error(morie_interval_map(d$y, d$left[-1], d$right, 0.1, 0.1), "same length")
})

test_that("the interval scan evaluates the EM LOD along the interval", {
  d <- rq_data()
  L <- 0.2
  sc <- morie_scan_interval(d$y, d$left, d$right, L, step = 0.05, max_iter = 500)
  expect_equal(sc$position, seq(0, 0.2, by = 0.05), tolerance = 1e-12)
  ref <- vapply(sc$position, function(p) {
    morie_interval_map(d$y, d$left, d$right, morie_haldane(p),
                       morie_haldane(max(L - p, 0)), max_iter = 500)$lod
  }, numeric(1))
  expect_equal(sc$lod, ref, tolerance = 1e-12)
  expect_equal(sc$peak_lod, max(ref), tolerance = 1e-12)
  expect_equal(sc$peak_position, sc$position[which.max(ref)])
  expect_equal(morie_interval_mapping(d$y, d$left, d$right, L, step = 0.05,
                                      max_iter = 500)$lod, ref, tolerance = 1e-12)
  expect_error(morie_scan_interval(d$y, d$left, d$right, 0), "positive")
})

test_that("ELOD, the LOD threshold and the progeny requirement", {
  e <- morie_elod(0.3, 1.2)
  expect_equal(e$elod, 0.5 * log10(1 + 0.25), tolerance = 1e-12)
  expect_equal(e$approximation, 0.22 * 0.25, tolerance = 1e-12)
  expect_equal(e$gap, 0.22 * 0.25 - 0.5 * log10(1.25), tolerance = 1e-12)
  expect_error(morie_elod(1, 0), "residual")
  expect_error(morie_elod(-1, 1), "negative")
  for (a in c(0.05, 0.01)) {
    z <- stats::qnorm(1 - a / 2)
    th <- morie_threshold(a)
    expect_equal(th$z, z, tolerance = 1e-9)
    expect_equal(th$threshold, z^2 / (2 * log(10)), tolerance = 1e-9)
  }
  # the paper prints T = 0.83 (three digits), hence the loose tolerance
  expect_equal(morie_threshold(0.05)$threshold, 0.834, tolerance = 1e-3)
  expect_error(morie_threshold(1), "alpha")
  pr <- morie_progeny_required(0.3, 1.2)
  expect_equal(pr$n, morie_threshold()$threshold / (0.5 * log10(1.25)), tolerance = 1e-12)
  expect_error(morie_progeny_required(0, 1), "never detected")
})

test_that("morie_rqtmpl returns the function collection", {
  r <- morie_rqtmpl()
  expect_identical(r$haldane, morie_haldane)
  expect_identical(r$interval_mapping, morie_scan_interval)
  expect_equal(r$LOG10E, 1 / log(10), tolerance = 1e-15)
  expect_match(r$cheatsheet(), "MIXTURE", fixed = TRUE)
})
