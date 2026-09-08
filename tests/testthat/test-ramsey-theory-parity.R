# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for Ramsey theory. Everything here is integer
# arithmetic on exactly-known quantities, so the anchors are EXACT --
# no tolerance, no fixture, no random number generator. That is the
# advantage of a combinatorial shelf over a statistical one.
#
# F. P. Ramsey's combinatorics. J. B. Ramsey's RESET test is a
# different subject and lives in test-reset-parity.R.

cycle_colouring <- function(n) {
  C <- matrix(0L, n, n)
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    C[i, j] <- 1L
    C[j, i] <- 1L
  }
  C
}

all_colourings <- function(n) {
  e <- utils::combn(n, 2)
  m <- ncol(e)
  lapply(seq_len(2^m) - 1L, function(mask) {
    C <- matrix(0L, n, n)
    for (b in seq_len(m)) {
      if (bitwAnd(bitwShiftR(mask, b - 1L), 1L) == 1L) {
        C[e[1, b], e[2, b]] <- 1L
        C[e[2, b], e[1, b]] <- 1L
      }
    }
    C
  })
}

test_that("the nine known values match DS1 Table Ia exactly", {
  known <- list(c(3, 3, 6), c(3, 4, 9), c(3, 5, 14), c(3, 6, 18),
                c(3, 7, 23), c(3, 8, 28), c(3, 9, 36), c(4, 4, 18),
                c(4, 5, 25))
  for (kv in known) {
    out <- morie_ramsey_number(kv[1], kv[2])
    expect_equal(out$value, as.integer(kv[3]))
    expect_true(out$exact)
  }
})

test_that("Ramsey numbers are symmetric in their arguments", {
  for (p in list(c(3, 4), c(3, 7), c(4, 5), c(5, 5))) {
    a <- morie_ramsey_number(p[1], p[2])
    b <- morie_ramsey_number(p[2], p[1])
    expect_equal(a$value, b$value)
    expect_equal(a$lower, b$lower)
    expect_equal(a$upper, b$upper)
  }
})

test_that("the trivial cases are exact", {
  expect_equal(morie_ramsey_number(1, 9)$value, 1L)
  for (l in 2:7) expect_equal(morie_ramsey_number(2, l)$value, as.integer(l))
})

test_that("unknown values return an interval, never a number", {
  for (p in list(c(5, 5), c(6, 6), c(4, 6), c(3, 10))) {
    out <- morie_ramsey_number(p[1], p[2])
    expect_null(out$value)
    expect_false(out$exact)
    expect_lt(out$lower, out$upper)
    expect_true(any(grepl("never been determined", out$warnings)))
  }
})

test_that("the circulating wrong value for R(5,5) is flagged", {
  out <- morie_ramsey_number(5, 5)
  expect_equal(c(out$lower, out$upper), c(43L, 46L))
  expect_true(any(grepl("50 is incorrect", out$warnings)))
})

test_that("Goodman's identity matches brute force on random colourings", {
  set.seed(1)
  for (i in seq_len(60L)) {
    n <- sample(3:9, 1)
    C <- matrix(0L, n, n)
    up <- utils::combn(n, 2)
    for (b in seq_len(ncol(up))) {
      v <- as.integer(stats::runif(1) < 0.5)
      C[up[1, b], up[2, b]] <- v
      C[up[2, b], up[1, b]] <- v
    }
    out <- morie_goodman_triangles(C, brute_force = TRUE)
    expect_equal(out$identity_residual, 0)
    expect_equal(out$red_triangles + out$blue_triangles, out$monochromatic)
  }
})

test_that("monochromatic plus bichromatic is every triangle", {
  set.seed(2)
  for (n in c(4L, 6L, 9L)) {
    C <- matrix(0L, n, n)
    up <- utils::combn(n, 2)
    for (b in seq_len(ncol(up))) {
      v <- as.integer(stats::runif(1) < 0.5)
      C[up[1, b], up[2, b]] <- v
      C[up[2, b], up[1, b]] <- v
    }
    out <- morie_goodman_triangles(C)
    expect_equal(out$monochromatic + out$bichromatic, choose(n, 3))
  }
})

test_that("an all-red graph is entirely monochromatic", {
  n <- 7L
  C <- matrix(1L, n, n)
  diag(C) <- 0L
  expect_equal(morie_goodman_triangles(C)$monochromatic, choose(n, 3))
})

test_that("the five-cycle has no monochromatic triangle", {
  expect_equal(morie_goodman_triangles(cycle_colouring(5))$monochromatic, 0)
})

test_that("every colouring of K6 has at least two monochromatic triangles", {
  # the exhaustive half of R(3,3) <= 6: all 2^15 colourings
  worst <- Inf
  for (C in all_colourings(6L)) {
    worst <- min(worst, morie_goodman_triangles(C)$monochromatic)
  }
  expect_equal(worst, 2)
  expect_equal(morie_goodman_minimum(6)$minimum, 2)
})

test_that("Goodman's minimum is attained exhaustively for small n", {
  for (n in 3:6) {
    obs <- Inf
    for (C in all_colourings(n)) {
      obs <- min(obs, morie_goodman_triangles(C)$monochromatic)
    }
    expect_equal(obs, morie_goodman_minimum(n)$minimum)
  }
})

test_that("Goodman's minimum is zero below six and positive from six", {
  for (n in 3:5) expect_equal(morie_goodman_minimum(n)$minimum, 0)
  for (n in 6:10) expect_gte(morie_goodman_minimum(n)$minimum, 1)
})

test_that("the five-cycle witness certifies the lower bound", {
  w <- morie_ramsey_witness(cycle_colouring(5), 3, 3)
  expect_true(w$valid)
  expect_null(w$red_clique)
  expect_null(w$blue_clique)
  expect_equal(w$certifies, "R(3,3) > 5")
})

test_that("a bad witness is rejected with the offending clique", {
  C <- matrix(1L, 6, 6)
  diag(C) <- 0L
  w <- morie_ramsey_witness(C, 3, 3)
  expect_false(w$valid)
  expect_equal(length(w$red_clique), 3L)
})

test_that("the party problem is proved at six and fails at five", {
  six <- morie_party_problem(6)
  expect_true(six$guaranteed)
  expect_equal(six$minimum_monochromatic, 2)
  five <- morie_party_problem(5)
  expect_false(five$guaranteed)
  expect_true(five$witness_valid)
  expect_equal(morie_goodman_triangles(five$witness)$monochromatic, 0)
})

test_that("the pure recursion derives the classical values tightly", {
  # Greenwood and Gleason, with nothing looked up
  for (kv in list(c(3, 3, 6), c(3, 4, 9), c(3, 5, 14), c(4, 4, 18))) {
    b <- morie_ramsey_upper_bound(kv[1], kv[2], use_known = FALSE)
    expect_false(b$used_known_values)
    expect_equal(b$recursive, kv[3])
  }
})

test_that("the recursion is not tight everywhere", {
  b <- morie_ramsey_upper_bound(4, 5, use_known = FALSE)
  expect_equal(b$recursive, 31)
  expect_equal(morie_ramsey_number(4, 5)$value, 25L)
})

test_that("the binomial bound is weaker than the recursion", {
  for (p in list(c(3, 4), c(4, 4), c(4, 5), c(5, 5), c(6, 6))) {
    b <- morie_ramsey_upper_bound(p[1], p[2], use_known = FALSE)
    expect_gte(b$binomial, b$recursive)
    expect_equal(b$best, min(b$binomial, b$recursive))
  }
})

test_that("every upper bound actually bounds the known value", {
  for (p in list(c(3, 3), c(3, 4), c(3, 5), c(3, 6), c(4, 4), c(4, 5))) {
    v <- morie_ramsey_number(p[1], p[2])$value
    b <- morie_ramsey_upper_bound(p[1], p[2], use_known = FALSE)
    expect_lte(v, b$recursive)
    expect_lte(v, b$binomial)
  }
})

test_that("the probabilistic bound sits below every known value", {
  for (k in 3:4) {
    lb <- morie_ramsey_lower_bound_probabilistic(k)$bound
    expect_lt(lb, morie_ramsey_number(k, k)$value)
  }
})

test_that("the probabilistic bound beats the 2^(k/2) form", {
  for (k in c(10L, 15L, 20L)) {
    b <- morie_ramsey_lower_bound_probabilistic(k)
    expect_gt(b$bound, b$asymptotic_2_to_k_over_2)
    expect_lt(b$expected_at_bound, 1)
  }
})

test_that("these values match the Python core exactly", {
  # integer arithmetic on exactly-known quantities, so parity is exact
  expect_equal(morie_ramsey_number(3, 3)$value, 6L)
  expect_equal(morie_ramsey_number(4, 5)$value, 25L)
  expect_equal(morie_goodman_minimum(6)$minimum, 2)
  expect_equal(morie_goodman_minimum(7)$minimum, 4)
  expect_equal(morie_goodman_minimum(10)$minimum, 20)
  expect_equal(morie_ramsey_upper_bound(4, 5, use_known = FALSE)$recursive, 31)
  expect_equal(morie_ramsey_upper_bound(5, 5, use_known = FALSE)$recursive, 62)
  expect_equal(morie_ramsey_upper_bound(5, 5)$binomial, 70)
  expect_equal(morie_ramsey_lower_bound_probabilistic(10)$bound, 100L)
  expect_equal(morie_ramsey_lower_bound_probabilistic(20)$bound, 5817L)
})

test_that("Ramsey theory input validation", {
  expect_error(morie_ramsey_number(0, 3), "at least 1")
  expect_error(morie_goodman_triangles(matrix(0L, 2, 2)),
               "at least three vertices")
  expect_error(morie_goodman_triangles(matrix(0L, 3, 4)), "must be square")
  expect_error(morie_goodman_minimum(2), "at least 3")
  expect_error(morie_ramsey_lower_bound_probabilistic(1), "at least 2")
  C <- matrix(0L, 4, 4)
  C[1, 2] <- 1L
  expect_error(morie_goodman_triangles(C), "symmetric")
})
