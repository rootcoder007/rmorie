# Contact tracing branching process (Hellewell et al. 2020; Lloyd-Smith
# et al. 2005): negative binomial offspring moments, serial-interval
# draws replayed on the shared stream, outbreak bookkeeping, and the
# control probability over replicate seeds.

test_that("offspring are negative binomial with mean R0 and variance R0(1 + R0/k)", {
  e <- .ghc_rng(3)
  x <- vapply(1:20000, function(i) negbinom_offspring(2, 0.5, e), 1L)
  # 20000 draws: the standard error of the mean is sqrt(10 / 20000) = 0.022
  expect_lt(abs(mean(x) - 2), 0.1)
  expect_lt(abs(stats::var(x) / (2 * (1 + 2 / 0.5)) - 1), 0.1)
  # huge k is the Poisson limit: mean = variance
  e2 <- .ghc_rng(4)
  p <- vapply(1:20000, function(i) negbinom_offspring(1.5, 1e7, e2), 1L)
  expect_lt(abs(mean(p) - 1.5), 0.05)
  expect_lt(abs(stats::var(p) / 1.5 - 1), 0.05)
  expect_identical(negbinom_offspring(0, 1, e), 0L)
  expect_error(negbinom_offspring(-1, 1, e), "non-negative")
  expect_error(negbinom_offspring(1, 0, e), "positive")
})

test_that("serial intervals are mean + sd z on the shared stream", {
  z <- .ghc_norm(.ghc_rng(5), 1L)
  expect_equal(serial_interval_draw(4.7, 2.9, .ghc_rng(5)), 4.7 + 2.9 * z, tolerance = 1e-15)
  expect_equal(serial_interval_draw(-50, 1, .ghc_rng(5), allow_presymptomatic = FALSE), 0)
  expect_error(serial_interval_draw(1, 0, .ghc_rng(5)), "sd must be positive")
})

test_that("an outbreak with R0 = 0 stops at the index cases; weekly counts add up", {
  z <- simulate_outbreak(R0 = 0, n_initial = 7, seed = 1)
  expect_true(z$controlled)
  expect_identical(z$total_cases, 7L)
  expect_identical(z$weekly[1], 7L)
  for (s in 1:3) {
    o <- simulate_outbreak(R0 = 1.5, dispersion = 0.5, n_initial = 5, seed = s, max_weeks = 8)
    if (!o$hit_cap) expect_identical(sum(o$weekly), o$total_cases)
    expect_identical(o$controlled, !o$hit_cap && o$extinct)
  }
  big <- simulate_outbreak(R0 = 5, dispersion = 10, trace_prob = 0, n_initial = 20,
                           max_cases = 300, seed = 2)
  expect_true(big$hit_cap)
  expect_false(big$controlled)
  expect_error(simulate_outbreak(trace_prob = 2), "trace_prob")
  expect_error(simulate_outbreak(subclinical = -1), "subclinical")
  expect_error(simulate_outbreak(n_initial = 0), "at least one")
})

test_that("the control probability averages the replicate outcomes", {
  r <- probability_of_control(reps = 6, seed = 3, R0 = 1.2, dispersion = 0.3, n_initial = 3,
                              max_weeks = 6, max_cases = 400)
  outs <- lapply(0:5, function(k) simulate_outbreak(R0 = 1.2, dispersion = 0.3, n_initial = 3,
                                                    max_weeks = 6, max_cases = 400,
                                                    seed = 3L * 7919L + k))
  ctrl <- vapply(outs, `[[`, TRUE, "controlled")
  sz <- sort(vapply(outs, `[[`, 1L, "total_cases"))
  expect_equal(r$probability_of_control, mean(ctrl))
  expect_equal(r$se, sqrt(mean(ctrl) * (1 - mean(ctrl)) / 6), tolerance = 1e-15)
  expect_identical(r$median_size, sz[4])
  expect_identical(r$max_size, sz[6])
  expect_identical(r$max_cases, 400L)
  expect_equal(morie_ttrace(reps = 6, seed = 3, R0 = 1.2, dispersion = 0.3, n_initial = 3,
                            max_weeks = 6, max_cases = 400)$estimate, r$estimate)
  expect_identical(contact_tracing_yield, probability_of_control)
})

test_that("R_eff is R0 times the share of transmission before isolation", {
  r <- effective_reproduction_number(2.5, 4.7, 2.9, 3.8, 2.4, 0.6, subclinical = 0.1,
                                     draws = 500, seed = 9)
  e <- .ghc_rng(9)
  hit <- 0
  for (i in 1:500) {
    if (.ghc_unif(e, 1L) < 0.1) {
      hit <- hit + 1
      next
    }
    tr <- .ghc_unif(e, 1L) < 0.6
    tiso <- if (tr) 0 else max(0, 3.8 + 2.4 * .ghc_norm(e, 1L))
    si <- 4.7 + 2.9 * .ghc_norm(e, 1L)
    if (si < tiso) hit <- hit + 1
  }
  expect_equal(r$fraction_before_isolation, hit / 500)
  expect_equal(r$R_eff, 2.5 * hit / 500)
  expect_identical(r$controlled_in_expectation, 2.5 * hit / 500 < 1)
})
