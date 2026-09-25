# Daubechies filters: db2 against its closed form, and the orthonormality
# identities for every order at machine precision.

test_that("db2 is (1 + sqrt3, 3 + sqrt3, 3 - sqrt3, 1 - sqrt3) / (4 sqrt2)", {
  s3 <- sqrt(3)
  exact <- c(1 + s3, 3 + s3, 3 - s3, 1 - s3) / (4 * sqrt(2))
  r <- OrthFilt(2)
  expect_equal(r$rec_lo, exact, tolerance = 1e-15)
  expect_identical(r$dec_lo, rev(r$rec_lo))
  expect_identical(r$dec_hi, rev(r$rec_hi))
})

test_that("every order is orthonormal under double shifts", {
  for (k in 1:10) {
    h <- OrthFilt(k)$rec_lo
    L <- length(h)
    expect_equal(sum(h), sqrt(2), tolerance = 1e-14)
    expect_equal(sum(h^2), 1, tolerance = 1e-14)
    if (L > 2) {
      for (m in 1:(L / 2 - 1)) {
        expect_lt(abs(sum(h[1:(L - 2 * m)] * h[(1 + 2 * m):L])), 1e-15)
      }
    }
  }
})
