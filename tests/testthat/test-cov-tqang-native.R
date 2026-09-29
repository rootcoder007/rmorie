# Uniform angle quantisation on [-pi, pi): wrapped differences, midpoint
# codewords and the delta^2 / 12 mean-squared error of a flat density.

test_that("wrap_angle maps into [-pi, pi) and the difference wraps", {
  for (f in list(wrap_angle, morie_tqang_wrap_angle)) {
    for (x in c(0.3, 3.5, -3.5, 7 * pi + 0.1, -pi, pi)) {
      ref <- ((x + pi) %% (2 * pi)) - pi
      expect_equal(f(x), ref, tolerance = 1e-12)
      expect_gte(f(x), -pi)
      expect_lt(f(x), pi)
    }
  }
  for (f in list(angular_difference, morie_tqang_angular_difference)) {
    # just below pi and just above -pi are neighbours
    expect_equal(f(pi - 0.01, -pi + 0.01), -0.02, tolerance = 1e-12)
    expect_equal(f(0.5, 0.2), 0.3, tolerance = 1e-12)
  }
})

test_that("quantisation picks the sector and reconstructs its midpoint", {
  th <- c(-3.1, -1, 0, 0.4, 2.9, 3.3, 10)
  for (f in list(morie_tqang, morie_quantize_angles, morie_turboquant_angle_quantization,
                 morie_tqang_quantize_angles, morie_tqang_tqang,
                 morie_tqang_turboquant_angle_quantization)) {
    r <- f(th, bits = 3)
    delta <- 2 * pi / 8
    w <- ((th + pi) %% (2 * pi)) - pi
    k <- pmin(floor((w + pi) / delta), 7)
    rec <- -pi + (k + 0.5) * delta
    expect_identical(r$indices, as.integer(k))
    expect_equal(r$values, rec, tolerance = 1e-12)
    err <- ((w - rec + pi) %% (2 * pi)) - pi
    expect_equal(r$errors, err, tolerance = 1e-12)
    expect_equal(r$mse, mean(err^2), tolerance = 1e-12)
    expect_true(all(abs(r$errors) <= delta / 2 + 1e-12))
    expect_equal(r$mse_bound, delta^2 / 12, tolerance = 1e-12)
    expect_identical(r$levels, 8L)
    expect_error(f(th, bits = 0), "1..30")
  }
})

test_that("the MSE of a uniform sweep approaches delta^2 / 12", {
  n <- 20000
  th <- -pi + (seq_len(n) - 0.5) * 2 * pi / n
  r <- morie_tqang(th, bits = 4)
  # the sweep is a midpoint rule for (1/delta) int u^2 du, exact up to O(1/n^2)
  expect_equal(r$mse, r$mse_bound, tolerance = 1e-6)
  expect_identical(morie_tqang_quantize_angles(numeric(0))$mse, 0)
  expect_match(morie_tqang_cheatsheet(), "delta^2/12", fixed = TRUE)
})
