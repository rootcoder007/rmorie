# Coverage tests for R/chronos_native.R (Ansari et al. 2024, Chronos
# tokenisation). Both spellings (bare and chronos_*) are checked against
# the same recomputed values.

chr_pairs <- list(
  list(ms = mean_scale, ub = uniform_bins, qb = quantile_bins, qz = quantize, dq = dequantize,
    tk = tokenize, dt = detokenize, fs = forecast_summary),
  list(ms = chronos_mean_scale, ub = chronos_uniform_bins, qb = chronos_quantile_bins,
    qz = chronos_quantize, dq = chronos_dequantize, tk = chronos_tokenize,
    dt = chronos_detokenize, fs = chronos_forecast_summary)
)

test_that("mean scaling divides by the mean absolute context value", {
  x <- c(2, -4, 6, 0, 10, 3)
  for (f in chr_pairs) {
    s <- f$ms(x)
    expect_equal(s$scale, mean(abs(x)), tolerance = 1e-12)
    expect_equal(s$scaled, x / mean(abs(x)), tolerance = 1e-12)
    s3 <- f$ms(x, context = 3)
    expect_equal(s3$scale, 4, tolerance = 1e-12)
    expect_true(f$ms(c(0, 0, 5), context = 2)$degenerate)
    expect_error(f$ms(numeric(0)), "empty")
    expect_error(f$ms(x, context = 9), "context length")
  }
})

test_that("uniform and quantile bins", {
  for (f in chr_pairs) {
    u <- f$ub(-2, 2, 9)
    expect_equal(u$centers, seq(-2, 2, length.out = 9), tolerance = 1e-12)
    expect_equal(u$edges, seq(-1.75, 1.75, by = 0.5), tolerance = 1e-12)
    expect_equal(u$range, c(-2, 2))
    expect_error(f$ub(n_bins = 1), "at least 2")
    expect_error(f$ub(1, 0), "hi must exceed")
    s <- c(5, 1, 9, 3, 7, 2, 8, 4, 6, 10)
    q <- f$qb(s, 4)
    v <- sort(s)
    expect_equal(q$centers, v[floor(((1:4) - 0.5) * 10 / 4) + 1])
    expect_equal(q$edges, (q$centers[-1] + q$centers[-4]) / 2)
    expect_error(f$qb(1:3, 4), "cannot define")
    expect_error(f$qb(rep(1, 8), 4), "too concentrated")
  }
})

test_that("quantisation to the nearest centre, dequantisation, clipping", {
  x <- c(-3, -1.2, -0.3, 0.2, 0.74, 0.76, 2.5)
  for (f in chr_pairs) {
    b <- f$ub(-2, 2, 9)
    qz <- f$qz(x, b)
    nearest <- vapply(x, function(z) which.min(abs(z - b$centers) + 1e-9 * seq_along(b$centers)) - 1L, 0L)
    expect_equal(qz$tokens, nearest)
    expect_equal(qz$n_clipped, 2L)
    expect_equal(qz$clipped_fraction, 2 / 7)
    expect_equal(f$dq(c(qz$tokens, -2L, -1L), b), b$centers[nearest + 1])
    expect_error(f$dq(9L, b), "outside the vocabulary")
  }
})

test_that("tokenise / detokenise round trip and the forecast summary", {
  x <- c(3, 5, 4, 6, 5, 7)
  for (f in chr_pairs) {
    b <- f$ub(-3, 3, 61)
    tk <- f$tk(x, b, pad_to = 9)
    sc <- mean(abs(x))
    expect_equal(tk$scale, sc)
    expect_equal(tk$tokens[1:2], c(-1L, -1L))
    expect_equal(tk$tokens[9], -2L)
    expect_equal(tk$vocab_size, 63L)
    back <- f$dt(tk$tokens, b, tk$scale)
    # uniform bins 0.1 apart: rounding error at most half a bin, times the scale
    expect_true(all(abs(back - x) <= 0.05 * sc + 1e-12))
    expect_equal(back, b$centers[tk$tokens[3:8] + 1] * sc, tolerance = 1e-12)
    expect_equal(length(f$tk(x, b, add_eos = FALSE)$tokens), 6L)
    b5 <- f$ub(0, 4, 5)
    p <- c(1, 2, 4, 2, 1)
    fs <- f$fs(p, b5, quantiles = c(0.1, 0.5, 0.95))
    pn <- p / 10
    expect_equal(fs$mean, sum(pn * 0:4), tolerance = 1e-12)
    expect_equal(unlist(fs$quantiles, use.names = FALSE), c(0, 2, 4))
    expect_equal(fs$mode, 2)
    expect_error(f$fs(p[-1], b5), "4 probabilities for 5")
    expect_error(f$fs(rep(0, 5), b5), "no mass")
  }
  expect_identical(morie_chronos(x, uniform_bins(-3, 3, 61))$tokens, tokenize(x, uniform_bins(-3, 3, 61))$tokens)
})
