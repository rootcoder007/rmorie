# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native RNG: Philox4x32-10 (Salmon, Moraes, Dror & Shaw, SC'11) and
# Wichura's AS 241 (Applied Statistics 37(3), 1988).
#
# Correctness is established against PUBLISHED Known Answer Tests, not
# against whatever the implementation happens to produce.

philox_hex <- function(w) sprintf("%08x", as.integer(ifelse(w >= 2^31, w - 2^32, w)))

test_that("Philox matches the published Known Answer Tests", {
  # From the Random123 reference distribution accompanying Salmon et al.
  # (2011). The third vector is the leading hex digits of pi.
  cases <- list(
    list(c(0, 0, 0, 0), c(0, 0),
         c("6627e8d5", "e169c58d", "bc57ac4c", "9b00dbd8")),
    list(rep(4294967295, 4), rep(4294967295, 2),
         c("408f276d", "41c83b0e", "a20bc7c6", "6d5451fd")),
    list(c(0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344),
         c(0xa4093822, 0x299f31d0),
         c("d16cfe09", "94fdcceb", "5001e420", "24126ea1")))
  for (cs in cases) {
    got <- .morie_philox4x32(matrix(cs[[1]], nrow = 1), cs[[2]])[1, ]
    expect_equal(philox_hex(got), cs[[3]])
  }
})

test_that("AS 241 reproduces the normal quantile function", {
  # R's own qnorm IS AS 241, so this is a direct check against the reference
  # implementation of the algorithm being ported.
  p <- c(1e-10, 0.001, 0.025, 0.5, 0.975, 0.99, 1 - 1e-10)
  expect_equal(.morie_normal_quantile(p), qnorm(p), tolerance = 1e-12)
})

test_that("the normal quantile is antisymmetric", {
  # Only where 1 - p is representable without cancellation. Below about
  # p = 1e-9, forming 1 - p loses the low digits of p outright -- for
  # p = 1e-8, 1 - p rounds to 0.99999999 and the tail recovered inside AS 241
  # is 9.99999993923e-9, an eight-digit loss that shows up as ~1e-9 in
  # Phi^-1. That is the double-precision representation of the upper tail,
  # not the algorithm, and R's own qnorm has it too.
  p <- c(0.001, 0.01, 0.1, 0.3, 0.49)
  expect_equal(.morie_normal_quantile(1 - p), -.morie_normal_quantile(p),
               tolerance = 1e-12)
})

test_that("the extreme upper tail is limited by representing 1 - p", {
  # Documented, not hidden: ask for the tail through a value that IS exact
  # and the answer is exact; ask through 1 - p and it is not.
  expect_equal(.morie_normal_quantile(1e-8), qnorm(1e-8), tolerance = 1e-13)
  expect_lt(abs(.morie_normal_quantile(1 - 1e-8) + .morie_normal_quantile(1e-8)),
            1e-8)
})

test_that("uniforms never reach the endpoints", {
  u <- .morie_random_uniform(100000, seed = 7)
  expect_gt(min(u), 0)
  expect_lt(max(u), 1)
  expect_true(all(is.finite(.morie_normal_quantile(u))))
})

test_that("the moments are right", {
  z <- .morie_random_normal(200000, seed = 42)
  expect_equal(mean(z), 0, tolerance = 0.01)
  expect_equal(sd(z), 1, tolerance = 0.01)
  expect_equal(mean(((z - mean(z)) / sd(z))^3), 0, tolerance = 0.05)
  expect_equal(mean(((z - mean(z)) / sd(z))^4), 3, tolerance = 0.05)
})

test_that("seeds and streams are independent handles", {
  a <- .morie_random_uniform(1000, seed = 1, stream = 0)
  b <- .morie_random_uniform(1000, seed = 1, stream = 1)
  cc <- .morie_random_uniform(1000, seed = 2, stream = 0)
  expect_false(isTRUE(all.equal(a, b)))
  expect_false(isTRUE(all.equal(a, cc)))
  expect_equal(a, .morie_random_uniform(1000, seed = 1, stream = 0))
})

test_that("counter-based means any offset is reachable", {
  # Philox is a bijection of the index, so a long draw contains the short
  # one as a prefix; there is no state to wind forward.
  expect_equal(.morie_random_uniform(64, seed = 99)[1:9],
               .morie_random_uniform(9, seed = 99))
})

test_that("the stream agrees with the Python arm bit for bit", {
  u <- .morie_random_uniform(7, seed = 12345, stream = 3)
  z <- .morie_random_normal(7, seed = 12345, stream = 3)
  # The uniform stream IS bit-exact: it comes out of integer state, so
  # every platform reproduces it exactly and these stay identical().
  expect_identical(u[1], 0.82723027456086129)
  expect_identical(u[7], 0.36555732542183250)
  # The normal transform is AS 241 evaluated on those same uniforms.
  # It is deterministic, but the last bit of a long Horner chain is not
  # reproducible across platform double arithmetic -- macOS arm64
  # differs from x86_64 Linux by 1-2 ULP -- so this asserts agreement
  # to ~4 ULP. A real change in the transform moves it far more.
  expect_equal(z[1], 0.94327658191243779, tolerance = 1e-15)
  expect_equal(z[3], -1.19034287143374250, tolerance = 1e-15)
})

test_that("the multivariate draw reproduces the target covariance", {
  cov <- matrix(c(2.0, 0.8, 0.3, 0.8, 1.5, 0.2, 0.3, 0.2, 1.0), 3, 3)
  mu <- c(1, -2, 0.5)
  draws <- t(vapply(0:3999,
                    function(s) .morie_random_multivariate_normal(mu, cov, seed = 5, stream = s),
                    numeric(3)))
  expect_equal(colMeans(draws), mu, tolerance = 0.1)
  expect_equal(unname(stats::cov(draws)), cov, tolerance = 0.15)
})

test_that("the RNG rejects bad input", {
  expect_error(.morie_random_uniform(-1))
  expect_error(.morie_normal_quantile(0))
  expect_error(.morie_normal_quantile(1))
})
