# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for extremal combinatorics and design theory.
#
# Almost everything here is an exact integer or a decision, so the
# anchors are exact rather than to a tolerance -- and where a bound is
# claimed, the construction attaining it is built and verified rather
# than compared against a number.

FANO <- list(c(1, 2, 3), c(1, 4, 5), c(1, 6, 7), c(2, 4, 6),
             c(2, 5, 7), c(3, 4, 7), c(3, 5, 6))

all_graphs <- function(n) {
  e <- utils::combn(n, 2)
  m <- ncol(e)
  lapply(seq_len(2^m) - 1L, function(mask) {
    A <- matrix(0L, n, n)
    for (b in seq_len(m)) {
      if (bitwAnd(bitwShiftR(mask, b - 1L), 1L) == 1L) {
        A[e[1, b], e[2, b]] <- 1L
        A[e[2, b], e[1, b]] <- 1L
      }
    }
    A
  })
}

# ------------------------------------------------------------------
# Turan and Mantel
# ------------------------------------------------------------------

test_that("Turan matches exhaustive search over all graphs", {
  for (n in 2:6) {
    for (r in 2:3) {
      best <- -1
      for (A in all_graphs(n)) {
        if (is.null(morie_has_clique(A, r + 1L))) {
          best <- max(best, morie_count_edges(A))
        }
      }
      expect_equal(morie_turan_number(n, r)$count, best)
    }
  }
})

test_that("the Turan construction attains the bound and is clique-free", {
  for (n in 1:16) {
    for (r in 1:4) {
      g <- morie_turan_graph(n, r)
      expect_equal(g$edges, morie_turan_number(n, r)$count)
      expect_true(morie_turan_number(n, r)$attained)
      expect_null(morie_has_clique(g$adjacency, r + 1L))
    }
  }
})

test_that("Turan matches the Python core exactly", {
  expect_equal(morie_turan_number(5, 2)$count, 6)
  expect_equal(morie_turan_number(10, 3)$count, 33)
  expect_equal(morie_turan_number(9, 3)$count, 27)
  expect_equal(morie_turan_number(30, 5)$count, 360)
})

test_that("the rounded formula is exact only when r divides n", {
  expect_true(morie_turan_number(9, 3)$formula_is_exact)
  loose <- morie_turan_number(10, 3)
  expect_false(loose$formula_is_exact)
  expect_equal(loose$count, 33)
  expect_equal(loose$rounded_formula, 100 / 3, tolerance = 1e-9)
})

test_that("Mantel is floor(n^2/4) and equals Turan at r = 2", {
  for (n in 0:40) {
    expect_equal(morie_mantel_number(n)$count, (n^2) %/% 4)
    expect_equal(morie_mantel_number(n)$count, morie_turan_number(n, 2)$count)
  }
})

# ------------------------------------------------------------------
# Sperner and Erdos-Ko-Rado
# ------------------------------------------------------------------

test_that("Sperner width is the middle binomial", {
  for (n in 0:12) {
    expect_equal(morie_sperner_width(n)$count, choose(n, n %/% 2))
  }
  expect_equal(vapply(1:6, function(n) morie_sperner_width(n)$count,
                      numeric(1)),
               c(1, 2, 3, 6, 10, 20))
})

test_that("Sperner uniqueness follows the parity of n", {
  expect_true(morie_sperner_width(4)$unique_extremal)
  expect_false(morie_sperner_width(5)$unique_extremal)
  expect_equal(morie_sperner_width(5)$extremal_layers, c(2, 3))
})

test_that("EKR is the star inside its own regime", {
  for (p in list(c(6, 3), c(8, 3), c(10, 4), c(20, 5))) {
    out <- morie_erdos_ko_rado(p[1], p[2])
    expect_true(out$ekr_regime)
    expect_equal(out$count, choose(p[1] - 1, p[2] - 1))
  }
})

test_that("below the regime every family is intersecting", {
  out <- morie_erdos_ko_rado(5, 3)
  expect_false(out$ekr_regime)
  expect_equal(out$count, choose(5, 3))
  expect_equal(out$star_size, choose(4, 2))
  expect_gt(out$count, out$star_size)
  expect_true(any(grepl("below 2k", out$warnings)))
})

test_that("EKR matches exhaustive search on small cases", {
  max_intersecting <- function(n, k) {
    sets <- utils::combn(n, k, simplify = FALSE)
    m <- length(sets)
    best <- 0
    for (mask in seq_len(2^m) - 1L) {
      idx <- which(bitwAnd(bitwShiftR(mask, seq_len(m) - 1L), 1L) == 1L)
      if (length(idx) <= best) next
      ok <- TRUE
      if (length(idx) >= 2L) {
        pr <- utils::combn(idx, 2)
        for (c in seq_len(ncol(pr))) {
          if (length(intersect(sets[[pr[1, c]]], sets[[pr[2, c]]])) == 0L) {
            ok <- FALSE
            break
          }
        }
      }
      if (ok) best <- length(idx)
    }
    best
  }
  for (p in list(c(4, 2), c(5, 2), c(5, 3))) {
    expect_equal(morie_erdos_ko_rado(p[1], p[2])$count,
                 max_intersecting(p[1], p[2]))
  }
})

# ------------------------------------------------------------------
# Dilworth
# ------------------------------------------------------------------

divisibility_poset <- function(n) {
  outer(seq_len(n), seq_len(n), function(i, j) j %% i == 0)
}

test_that("Dilworth equality holds on the divisibility poset", {
  for (n in c(6, 8, 10, 12)) {
    out <- morie_dilworth_decomposition(divisibility_poset(n))
    expect_true(out$dilworth_holds)
    expect_equal(out$antichain_size, out$chain_cover_size)
  }
})

test_that("the antichain on 1..8 is the primes above four", {
  out <- morie_dilworth_decomposition(divisibility_poset(8))
  expect_equal(out$antichain_size, 4L)
  expect_equal(sort(out$antichain), c(2L, 3L, 5L, 7L))
})

test_that("a chain needs one chain and an antichain needs n", {
  n <- 6L
  chain <- outer(seq_len(n), seq_len(n), function(i, j) j >= i)
  out <- morie_dilworth_decomposition(chain)
  expect_equal(out$antichain_size, 1L)
  expect_equal(out$chain_cover_size, 1L)
  anti <- diag(TRUE, 5)
  out2 <- morie_dilworth_decomposition(anti)
  expect_equal(out2$antichain_size, 5L)
  expect_equal(out2$chain_cover_size, 5L)
})

test_that("Dilworth rejects a relation that is not a partial order", {
  expect_error(morie_dilworth_decomposition(
    matrix(c(FALSE, TRUE, FALSE, TRUE), 2, 2)), "reflexive")
  expect_error(morie_dilworth_decomposition(matrix(TRUE, 2, 2)),
               "antisymmetric")
})

# ------------------------------------------------------------------
# BIBD and Steiner
# ------------------------------------------------------------------

test_that("the Fano plane verifies as a BIBD", {
  c7 <- morie_incidence_check(FANO, 7)
  expect_true(c7$is_bibd)
  expect_equal(c(c7$k, c7$r, c7$lambda, c7$b), c(3, 3, 1, 7))
})

test_that("predicted parameters match the actual Fano plane", {
  p <- morie_bibd_parameters(7, 3, 1)
  c7 <- morie_incidence_check(FANO, 7)
  expect_equal(p$r, c7$r)
  expect_equal(p$b, c7$b)
  expect_true(p$feasible)
})

test_that("the counting conditions rule out impossible parameters", {
  out <- morie_bibd_parameters(8, 3, 1)
  expect_false(out$feasible)
  expect_false(out$exists)
})

test_that("feasible is not the same claim as exists", {
  out <- morie_bibd_parameters(22, 7, 2)
  expect_true(out$divisibility_ok)
  expect_true(out$fisher_ok)
  expect_true(out$feasible)
  expect_false(out$exists)
  expect_true(any(grepl("Bruck-Ryser-Chowla", out$warnings)))
})

test_that("an undetermined case says so rather than claiming existence", {
  out <- morie_bibd_parameters(13, 4, 1)
  expect_true(out$feasible)
  expect_null(out$exists)
  expect_true(any(grepl("NECESSARY, not", out$warnings)))
})

test_that("an incomplete design is rejected", {
  out <- morie_incidence_check(FANO[1:5], 7)
  expect_false(out$is_bibd)
  expect_gt(out$uncovered_pairs, 0)
  expect_true(any(grepl("no block", out$warnings)))
})

test_that("STS existence follows v mod 6", {
  for (v in 3:40) {
    expect_equal(morie_steiner_triple_system(v, construct = FALSE)$exists,
                 (v %% 6) %in% c(1, 3))
  }
})

test_that("the Bose construction covers every pair exactly once", {
  for (v in c(9, 15, 21, 27)) {
    out <- morie_steiner_triple_system(v)
    expect_true(out$verified)
    expect_equal(length(out$triples), out$n_triples)
    expect_equal(out$n_triples, choose(v, 2) / 3)
  }
})

test_that("a constructed system verifies as a BIBD", {
  out <- morie_steiner_triple_system(9)
  chk <- morie_incidence_check(out$triples, 9)
  expect_true(chk$is_bibd)
  expect_equal(c(chk$k, chk$lambda), c(3, 1))
})

# ------------------------------------------------------------------
# Latin squares
# ------------------------------------------------------------------

test_that("the cyclic construction is Latin at every order", {
  for (n in 1:12) {
    expect_true(morie_latin_square(n)$valid)
    expect_true(morie_is_latin_square(morie_latin_square(n)$square)$valid)
  }
})

test_that("a non-Latin grid is rejected", {
  expect_false(morie_is_latin_square(matrix(c(0, 0, 1, 1), 2, 2))$valid)
})

test_that("orthogonality requires both squares to be Latin", {
  A <- morie_latin_square(4)$square
  B <- morie_latin_square(4, method = "shifted")$square
  out <- morie_are_orthogonal(A, B)
  expect_false(morie_is_latin_square(B)$valid)
  expect_true(out$pair_condition_holds)
  expect_false(out$both_are_latin)
  expect_false(out$orthogonal)
})

test_that("a genuine orthogonal pair is recognised", {
  for (n in c(3, 5, 7)) {
    out <- morie_are_orthogonal(morie_latin_square(n)$square,
                                morie_latin_square(n, "shifted")$square)
    expect_true(out$both_are_latin)
    expect_true(out$orthogonal)
  }
})

test_that("no orthogonal pair exists at order two", {
  # Euler was right here; by exhaustion over both Latin squares
  sq <- list(matrix(c(0, 1, 1, 0), 2, 2, byrow = TRUE),
             matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE))
  for (a in sq) for (b in sq) {
    expect_false(morie_are_orthogonal(a, b)$orthogonal)
  }
})

# ------------------------------------------------------------------
# Coding bounds
# ------------------------------------------------------------------

test_that("the Hamming bound on the classical perfect codes", {
  expect_equal(morie_hamming_bound(7, 3)$bound, 16)
  expect_true(morie_hamming_bound(7, 3)$is_perfect_possible)
  expect_equal(morie_hamming_bound(23, 7)$bound, 4096)
  expect_equal(morie_hamming_bound(11, 5, q = 3)$bound, 729)
})

test_that("the ball volume is the sum of binomials", {
  for (p in list(c(7, 3), c(15, 3), c(23, 7))) {
    t <- (p[2] - 1) %/% 2
    expect_equal(morie_hamming_bound(p[1], p[2])$ball_volume,
                 sum(choose(p[1], 0:t)))
  }
})

test_that("a non-perfect case is flagged", {
  out <- morie_hamming_bound(5, 3)
  expect_false(out$is_perfect_possible)
  expect_equal(out$bound, 5)
})

test_that("the Singleton bound and which is tighter", {
  expect_equal(morie_singleton_bound(7, 3)$bound, 32)
  out <- morie_singleton_bound(23, 7)
  expect_true(out$hamming_is_tighter)
  expect_equal(out$tighter, out$hamming_bound)
  for (n in 2:7) expect_equal(morie_singleton_bound(n, n)$bound, 2)
})

test_that("extremal and design input validation", {
  expect_error(morie_turan_number(-1, 2), "non-negative")
  expect_error(morie_turan_number(5, 0), "at least 1")
  expect_error(morie_erdos_ko_rado(3, 5), "must not exceed n")
  expect_error(morie_bibd_parameters(5, 7, 1), "cannot exceed")
  expect_error(morie_latin_square(0), "at least 1")
  expect_error(morie_hamming_bound(3, 5), "must not exceed n")
  expect_error(morie_singleton_bound(3, 5), "must not exceed n")
})
