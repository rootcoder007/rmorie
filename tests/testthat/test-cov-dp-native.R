# Coverage for the differential-privacy mechanisms (Dwork & Roth 2014).
# The deterministic parts -- true answers, sensitivities, noise scales,
# the exponential-mechanism and quantile probabilities -- are recomputed
# from their definitions, and each seeded release is regenerated from the
# same seed with the Laplace inverse CDF, rnorm, sample.int or runif.

.lap <- function(n, b) {
  u <- stats::runif(n) - 0.5
  -b * sign(u) * log(1 - 2 * abs(u))
}

test_that("Laplace mechanism: scale Delta/eps and the seeded noise", {
  r <- morie_dp_laplace_mechanism(c(10, 20, 30), sensitivity = 2, epsilon = 0.5, seed = 11)
  expect_equal(r$noise_scale, 4)
  expect_equal(r$noise_sd, 4 * sqrt(2))
  set.seed(11)
  expect_equal(r$release, c(10, 20, 30) + .lap(3, 4), tolerance = 1e-12)
  big <- morie_dp_laplace_mechanism(rep(0, 20000), sensitivity = 1, epsilon = 2, seed = 3)$release
  # sample sd of 20000 Laplace(0.5) draws is within ~3 SE of sqrt(2)/2
  expect_lt(abs(stats::sd(big) - sqrt(2) / 2), 0.02)
  expect_error(morie_dp_laplace_mechanism(1, epsilon = 0), "finite and positive")
  expect_error(morie_dp_laplace_mechanism(1, sensitivity = -1), "must be positive")
})

test_that("Gaussian mechanism: the classical sigma and its epsilon <= 1 warning", {
  r <- morie_dp_gaussian_mechanism(c(1, 2), sensitivity = 3, epsilon = 0.8, delta = 1e-6, seed = 5)
  sig <- 3 * sqrt(2 * log(1.25 / 1e-6)) / 0.8
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  set.seed(5)
  expect_equal(r$release, c(1, 2) + stats::rnorm(2, 0, sig), tolerance = 1e-12)
  expect_length(r$warnings, 0L)
  expect_match(morie_dp_gaussian_mechanism(1, epsilon = 2, seed = 1)$warnings, "epsilon <= 1")
  expect_error(morie_dp_gaussian_mechanism(1, delta = 0), "delta > 0")
  expect_error(morie_dp_gaussian_mechanism(1, delta = 1), "delta must be in")
})

test_that("counts, sums and histograms release truth plus seeded Laplace noise", {
  d <- c(TRUE, FALSE, TRUE, TRUE)
  cnt <- morie_dp_count(d, epsilon = 2, seed = 9)
  expect_equal(cnt$true_count, 3)
  set.seed(9)
  raw <- 3 + .lap(1, 0.5)
  expect_equal(cnt$raw, raw, tolerance = 1e-12)
  expect_equal(cnt$release, max(raw, 0), tolerance = 1e-12)
  expect_equal(morie_dp_count(1:10, predicate = function(r) r > 6, seed = 1)$true_count, 4)
  expect_equal(morie_dp_count(list("a", "b"), seed = 1)$true_count, 2)
  s <- morie_dp_sum(c(-3, 1, 2, 5, 12), a = 0, b = 10, epsilon = 1, seed = 4)
  expect_equal(s$true_sum, 0 + 1 + 2 + 5 + 10)
  expect_equal(c(s$sensitivity, s$noise_scale, s$clipped_fraction), c(10, 10, 0.4))
  set.seed(4)
  expect_equal(s$release, 18 + .lap(1, 10), tolerance = 1e-12)
  expect_match(s$warnings, "clipped")
  expect_error(morie_dp_sum(1:3, a = 2, b = 1), "need a < b")
  x <- c(0.1, 0.4, 0.45, 0.8, 1, 0.2, 0.95)
  h <- morie_dp_histogram(x, bins = 4, epsilon = 1, range_ = c(0, 1), seed = 2)
  expect_equal(h$true_counts, c(2L, 2L, 0L, 3L))
  expect_equal(h$edges, seq(0, 1, length.out = 5))
  set.seed(2)
  raw <- c(2, 2, 0, 3) + .lap(4, 2)
  expect_equal(h$raw, raw, tolerance = 1e-12)
  expect_equal(h$release, pmax(raw, 0), tolerance = 1e-12)
  he <- morie_dp_histogram(x, bins = c(0, 0.5, 1), nonneg = FALSE, seed = 2)
  expect_equal(he$true_counts, c(4L, 3L))
})

test_that("exponential mechanism probabilities are proportional to exp(eps u / 2 Delta)", {
  u <- c(1, 3, 2.5, 0)
  r <- morie_dp_exponential_mechanism(c("a", "b", "c", "d"), u, epsilon = 1.5, sensitivity = 2, seed = 8)
  p <- exp(1.5 * u / 4) / sum(exp(1.5 * u / 4))
  expect_equal(r$probabilities, p, tolerance = 1e-12)
  set.seed(8)
  k <- sample.int(4, 1, prob = p)
  expect_identical(r$index, k - 1L)
  expect_identical(r$selected, c("a", "b", "c", "d")[k])
  expect_error(morie_dp_exponential_mechanism(1:3, 1:2), "utility has 2 entries")
  expect_error(morie_dp_exponential_mechanism(1:2, c(1, Inf)), "finite")
})

test_that("private quantiles sample an interval with prob ~ gap * exp(eps u / 2)", {
  x <- c(2, 5, 3, 9, 7, 1, 6)
  r <- morie_dp_quantile(x, q = 0.3, epsilon = 2, a = 0, b = 10, seed = 6)
  s <- sort(x)
  edges <- c(0, s, 10)
  util <- -abs(0:7 - 0.3 * 7)
  p <- diff(edges) * exp(2 * util / 2)
  p <- p / sum(p)
  expect_equal(r$probabilities, p, tolerance = 1e-12)
  set.seed(6)
  i <- sample.int(8, 1, prob = p)
  expect_equal(r$release, stats::runif(1, edges[i], edges[i + 1]), tolerance = 1e-12)
  expect_equal(r$true_quantile, stats::quantile(x, 0.3, names = FALSE))
  expect_length(r$warnings, 0L)
  m <- morie_dp_median(x, epsilon = 2, seed = 6)
  expect_equal(m$true_median, stats::median(x))
  expect_match(m$warnings, "non-private")
  expect_error(morie_dp_quantile(x, q = 1), "q must be in")
})

test_that("randomized response keeps the truth with probability e^eps / (1 + e^eps)", {
  t <- c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1)
  r <- morie_randomized_response_dp(t, epsilon = log(3), seed = 12)
  p <- 3 / 4
  expect_equal(r$p_truth, p, tolerance = 1e-12)
  set.seed(12)
  keep <- stats::runif(10) < p
  resp <- ifelse(keep, t, 1 - t)
  expect_equal(r$responses, resp)
  expect_equal(r$estimate, (mean(resp) - 0.25) / 0.5, tolerance = 1e-12)
  expect_equal(r$se, sqrt(mean(resp) * (1 - mean(resp)) / 10) / 0.5, tolerance = 1e-12)
  expect_error(morie_randomized_response_dp(c(0, 2)), "only 0 and 1")
})
