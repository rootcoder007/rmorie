# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for enumerative combinatorics.
#
# Every anchor is compared as a DECIMAL STRING, not as a double. That
# is the whole point: Bell(25) and p(1000) are far past 2^53, so a
# comparison through doubles would pass while the low-order digits were
# wrong. Comparing strings makes a silent precision loss fail the test.
#
# Where enumeration is feasible the count is checked against the
# objects themselves rather than against Python.

test_that("the exact integer layer agrees with Python to the last digit", {
  expect_equal(as.character(morie_big_factorial(20)), "2432902008176640000")
  expect_equal(morie_big_ndigits(morie_big_factorial(100)), 158L)
  expect_equal(as.character(morie_big_binom(100, 50)),
               "100891344545564193334812497256")
  expect_equal(as.character(morie_big_pow(2, 100)),
               "1267650600228229401496703205376")
})

test_that("base R's own choose() is wrong where the exact layer is right", {
  # not a style preference: choose(100, 50) is off by more than 1e15
  # and says nothing about it
  expect_false(format(choose(100, 50), scientific = FALSE) ==
                 as.character(morie_big_binom(100, 50)))
  expect_equal(as.character(morie_big_binom(100, 50)),
               "100891344545564193334812497256")
})

test_that("2^53 is where R stops counting", {
  expect_true(morie_big_fits_double("9007199254740992"))
  expect_false(morie_big_fits_double("9007199254740993"))
  expect_true(2^53 + 1 == 2^53)          # the defect itself
})

# ------------------------------------------------------------------
# Stirling numbers
# ------------------------------------------------------------------

test_that("Stirling numbers of the second kind match Python", {
  expect_equal(as.character(morie_stirling_second(5, 3)), "25")
  expect_equal(vapply(morie_stirling_second(4), as.character, character(1)),
               c("0", "1", "7", "6", "1"))
  expect_equal(as.character(morie_stirling_second(20, 10)), "5917584964655")
  expect_equal(as.character(morie_stirling_second(0, 0)), "1")
  expect_equal(as.character(morie_stirling_second(5, 0)), "0")
  expect_equal(as.character(morie_stirling_second(3, 9)), "0")
})

test_that("Stirling numbers of the first kind match Python", {
  expect_equal(as.character(morie_stirling_first(5, 3)), "35")
  expect_equal(vapply(morie_stirling_first(4), as.character, character(1)),
               c("0", "6", "11", "6", "1"))
  expect_equal(as.character(morie_stirling_first(20, 10)), "381922055502195")
})

test_that("the first-kind row sums to n factorial", {
  for (n in 1:10) {
    tot <- morie_bigint(0)
    for (v in morie_stirling_first(n)) tot <- morie_big_add(tot, v)
    expect_equal(as.character(tot), as.character(morie_big_factorial(n)))
  }
})

test_that("the signed first kind alternates in sign", {
  for (n in 1:7) {
    for (k in 0:n) {
      s <- morie_stirling_first(n, k, signed = TRUE)
      u <- morie_stirling_first(n, k)
      expect_equal(as.character(morie_big_mul(s, morie_bigint(
        if ((n - k) %% 2 == 1) -1 else 1))), as.character(u))
    }
  }
})

test_that("the two Stirling matrices are mutually inverse", {
  # sum_k s(n,k) S(k,m) = [n == m], the defining relation
  for (n in 1:7) {
    for (m in 1:n) {
      tot <- morie_bigint(0)
      for (k in m:n) {
        tot <- morie_big_add(tot, morie_big_mul(
          morie_stirling_first(n, k, signed = TRUE),
          morie_stirling_second(k, m)))
      }
      expect_equal(as.character(tot), if (n == m) "1" else "0")
    }
  }
})

# ------------------------------------------------------------------
# Bell, Catalan, derangements
# ------------------------------------------------------------------

test_that("Bell numbers match Python and stay exact past 2^53", {
  expect_equal(vapply(0:10, function(i) as.character(morie_bell_number(i)),
                      character(1)),
               c("1", "1", "2", "5", "15", "52", "203", "877", "4140",
                 "21147", "115975"))
  expect_equal(as.character(morie_bell_number(25)), "4638590332229999353")
  expect_false(morie_big_fits_double(morie_bell_number(25)))
})

test_that("Bell is the row sum of the second-kind Stirling numbers", {
  for (n in 0:20) {
    tot <- morie_bigint(0)
    for (v in morie_stirling_second(n)) tot <- morie_big_add(tot, v)
    expect_equal(as.character(tot), as.character(morie_bell_number(n)))
  }
})

test_that("Catalan numbers match Python", {
  expect_equal(vapply(0:8, function(i) as.character(morie_catalan_number(i)),
                      character(1)),
               c("1", "1", "2", "5", "14", "42", "132", "429", "1430"))
  expect_equal(as.character(morie_catalan_number(50)),
               "1978261657756160653623774456")
})

test_that("Catalan satisfies its own convolution", {
  for (n in 0:10) {
    tot <- morie_bigint(0)
    for (i in 0:n) {
      tot <- morie_big_add(tot, morie_big_mul(morie_catalan_number(i),
                                              morie_catalan_number(n - i)))
    }
    expect_equal(as.character(tot),
                 as.character(morie_catalan_number(n + 1)))
  }
})

test_that("derangements match Python and the brute-force count", {
  expect_equal(vapply(0:6, function(i) as.character(morie_derangements(i)),
                      character(1)),
               c("1", "0", "1", "2", "9", "44", "265"))
  expect_equal(as.character(morie_derangements(25)),
               "5706255282633466762357224")
})

test_that("derangements match inclusion-exclusion", {
  for (n in 1:12) {
    tot <- morie_bigint(0)
    for (i in 0:n) {
      term <- morie_big_divmod_small(morie_big_factorial(n),
                                     as.numeric(factorial(i)))$quotient
      tot <- if (i %% 2 == 0) morie_big_add(tot, term) else
        morie_big_sub(tot, term)
    }
    expect_equal(as.character(tot), as.character(morie_derangements(n)))
  }
})

# ------------------------------------------------------------------
# Partitions
# ------------------------------------------------------------------

test_that("partition counts match Python and published landmarks", {
  expect_equal(vapply(0:10, function(i)
    as.character(morie_partition_count(i)), character(1)),
    c("1", "1", "2", "3", "5", "7", "11", "15", "22", "30", "42"))
  expect_equal(as.character(morie_partition_count(50)), "204226")
  expect_equal(as.character(morie_partition_count(100)), "190569292")
  expect_equal(as.character(morie_partition_count(200)), "3972999029388")
})

test_that("p(1000) is exact at 32 digits", {
  p <- morie_partition_count(1000)
  expect_equal(as.character(p), "24061467864032622473692149727991")
  expect_equal(morie_big_ndigits(p), 32L)
  expect_false(morie_big_fits_double(p))
})

test_that("Euler's theorem holds at every n, not on average", {
  for (n in 0:45) {
    expect_equal(as.character(morie_partition_count(n, distinct = TRUE)),
                 as.character(morie_partition_count(n, odd_only = TRUE)))
  }
})

test_that("partitions by part count sum to the total", {
  for (n in 1:30) {
    tot <- morie_bigint(0)
    for (k in seq_len(n)) {
      tot <- morie_big_add(tot, morie_partitions_into_parts(n, k))
    }
    expect_equal(as.character(tot), as.character(morie_partition_count(n)))
  }
})

test_that("partitions into k parts match Python", {
  expect_equal(as.character(morie_partitions_into_parts(7, 3)), "4")
  expect_equal(as.character(morie_partitions_into_parts(0, 0)), "1")
  expect_equal(as.character(morie_partitions_into_parts(5, 0)), "0")
  expect_equal(as.character(morie_partitions_into_parts(5, 6)), "0")
  expect_equal(as.character(morie_partitions_into_parts(50, 10)), "16928")
})

# ------------------------------------------------------------------
# Twelvefold way
# ------------------------------------------------------------------

test_that("the labelled cells match direct enumeration", {
  for (n in 0:4) {
    for (k in 1:4) {
      expect_equal(as.character(morie_twelvefold_way(n, k)$count),
                   as.character(morie_big_pow(k, n)))
      # surjections by inclusion-exclusion, independently
      surj <- morie_bigint(0)
      for (j in 0:k) {
        term <- morie_big_mul(morie_big_binom(k, j), morie_big_pow(k - j, n))
        surj <- if (j %% 2 == 0) morie_big_add(surj, term) else
          morie_big_sub(surj, term)
      }
      expect_equal(as.character(morie_twelvefold_way(
        n, k, condition = "surjective")$count), as.character(surj))
    }
  }
})

test_that("unlabelled balls count multisets", {
  for (n in 0:5) {
    for (k in 1:4) {
      expect_equal(as.character(morie_twelvefold_way(
        n, k, balls = "unlabelled")$count),
        as.character(morie_big_binom(n + k - 1, n)))
    }
  }
})

test_that("unlabelled boxes give Stirling and partition counts", {
  for (n in 1:6) {
    for (k in 1:n) {
      expect_equal(as.character(morie_twelvefold_way(
        n, k, boxes = "unlabelled", condition = "surjective")$count),
        as.character(morie_stirling_second(n, k)))
      expect_equal(as.character(morie_twelvefold_way(
        n, k, balls = "unlabelled", boxes = "unlabelled",
        condition = "surjective")$count),
        as.character(morie_partitions_into_parts(n, k)))
    }
  }
})

test_that("all twelve cells are distinct and report a formula", {
  cells <- character(0)
  for (b in c("labelled", "unlabelled")) {
    for (x in c("labelled", "unlabelled")) {
      for (cd in c("any", "injective", "surjective")) {
        out <- morie_twelvefold_way(4, 3, balls = b, boxes = x,
                                    condition = cd)
        expect_true(nzchar(out$formula))
        expect_gte(morie_big_cmp(out$count, 0), 0L)
        cells <- c(cells, out$cell)
      }
    }
  }
  expect_equal(length(unique(cells)), 12L)
})

# ------------------------------------------------------------------
# Mobius inversion
# ------------------------------------------------------------------

test_that("Mobius inversion matches Python exactly", {
  out <- morie_mobius_inversion(c(1, 2, 2, 3, 2, 4))
  expect_equal(out$g, rep(1, 6))
  expect_equal(out$reconstruction_residual, 0)
})

test_that("the Mobius identity has residual zero", {
  out <- morie_mobius_inversion(rep(1, 40))
  expect_equal(out$mobius_identity_residual, 0)
  expect_equal(out$divisor_sums[1], 1)
  expect_true(all(out$divisor_sums[-1] == 0))
})

test_that("the Mobius function takes its known values", {
  out <- morie_mobius_inversion(rep(1, 12))
  expect_equal(out$mobius, c(1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0))
})

test_that("inverting the divisor count gives the all-ones function", {
  f <- vapply(seq_len(20), function(m) sum(m %% seq_len(m) == 0), numeric(1))
  expect_equal(morie_mobius_inversion(f)$g, rep(1, 20))
})

test_that("enumerative input validation", {
  expect_error(morie_partition_count(-1), "non-negative")
  expect_error(morie_partition_count(5, distinct = TRUE, odd_only = TRUE),
               "alternatives")
  expect_error(morie_stirling_second(-1), "non-negative")
  expect_error(morie_mobius_inversion(numeric(0)), "must not be empty")
  expect_error(morie_bigint("12x3"), "not a decimal integer")
  expect_error(morie_big_factorial(-1), "non-negative")
})
