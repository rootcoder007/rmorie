# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/phacf3_native.R (three-point 3D pharmacophore
# fingerprint): distance binning, the canonical triangle key under the
# six vertex permutations, the enumerated key space and the
# min/max Tanimoto coefficient.

.ph_perm <- function(t, d) {
  # every relabelling of (i, j, k) with edges (ij, ik, jk)
  D <- matrix(0, 3, 3)
  D[1, 2] <- D[2, 1] <- d[1]
  D[1, 3] <- D[3, 1] <- d[2]
  D[2, 3] <- D[3, 2] <- d[3]
  ps <- list(1:3, c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  m <- t(vapply(ps, function(p) c(t[p], D[p[1], p[2]], D[p[1], p[3]], D[p[2], p[3]]), numeric(6)))
  m[order(m[, 1], m[, 2], m[, 3], m[, 4], m[, 5], m[, 6])[1], ]
}

test_that("bin finds the half-open interval, -1 outside the range", {
  e <- c(2, 4.5, 7, 10)
  expect_identical(morie_phacf3_bin(2, e), 0L)
  expect_identical(morie_phacf3_bin(4.4999, e), 0L)
  expect_identical(morie_phacf3_bin(4.5, e), 1L)
  expect_identical(morie_phacf3_bin(9.99, e), 2L)
  expect_identical(morie_phacf3_bin(10, e), -1L)
  expect_identical(morie_phacf3_bin(1.9, e), -1L)
})

test_that("canonical picks the lexicographically smallest relabelling", {
  expect_identical(morie_phacf3_canonical(2, 0, 1, 3, 4, 5), as.integer(.ph_perm(c(2, 0, 1), c(3, 4, 5))))
  expect_identical(morie_phacf3_canonical(0, 1, 2, 5, 4, 3), as.integer(.ph_perm(c(0, 1, 2), c(5, 4, 3))))
  # already canonical, and invariant under any relabelling of the same triangle
  k <- morie_phacf3_canonical(0, 1, 2, 1, 2, 3)
  expect_identical(k, c(0L, 1L, 2L, 1L, 2L, 3L))
  expect_identical(morie_phacf3_canonical(2, 1, 0, 3, 2, 1), k)
  expect_identical(morie_phacf3_canonical(1, 0, 2, 1, 3, 2), k)
})

test_that("space enumerates every canonical key once, in sorted order", {
  sp <- morie_phacf3_space(c("donor", "acceptor"), n_bins = 2, edges = c(2, 5, 9))
  expect_length(sp$keys, length(sp$strings))
  expect_false(anyDuplicated(sp$strings) > 0)
  expect_identical(sp$strings, sort(sp$strings))
  # each key is its own canonical form, and every (type, bin) triple maps in
  for (k in sp$keys) expect_identical(do.call(morie_phacf3_canonical, as.list(k)), k)
  seen <- character(0)
  for (a in 0:1) for (b in 0:1) for (cc in 0:1) for (p in 0:1) for (q in 0:1) for (r in 0:1) {
    seen <- c(seen, sprintf("%02d,%02d,%02d,%02d,%02d,%02d",
                            morie_phacf3_canonical(a, b, cc, p, q, r)[1],
                            morie_phacf3_canonical(a, b, cc, p, q, r)[2],
                            morie_phacf3_canonical(a, b, cc, p, q, r)[3],
                            morie_phacf3_canonical(a, b, cc, p, q, r)[4],
                            morie_phacf3_canonical(a, b, cc, p, q, r)[5],
                            morie_phacf3_canonical(a, b, cc, p, q, r)[6]))
  }
  expect_setequal(sp$strings, unique(seen))
  expect_equal(get(sp$strings[3], envir = sp$index), 2L)
  expect_error(morie_phacf3_space(c("donor"), n_bins = 0), "at least one distance bin")
})

test_that("tanimoto is sum(min) / sum(max)", {
  a <- c(1, 0, 2, 3)
  b <- c(1, 1, 0, 5)
  expect_equal(morie_phacf3_tanimoto(a, b), (1 + 0 + 0 + 3) / (1 + 1 + 2 + 5), tolerance = 1e-12)
  expect_equal(morie_phacf3_tanimoto(a, a), 1)
  expect_true(is.nan(morie_phacf3_tanimoto(c(0, 0), c(0, 0))))
  expect_error(morie_phacf3_tanimoto(a, b[-1]), "same length")
})

test_that("morie_phacf3 sets the bit of every in-range triangle", {
  ed <- c(2, 5, 9)
  sp <- morie_phacf3_space(c("donor", "acceptor"), 2, ed)
  mol <- list(list(0, 0, 0, "donor"), list(3, 0, 0, "acceptor"),
              list(0, 4, 0, "donor"), list(30, 0, 0, "acceptor"))
  r <- morie_phacf3(mol, c("donor", "acceptor"), ed, space = sp)
  # the one usable triangle is (1, 2, 3): sides 3, 4, 5 -> bins 0, 0, 1
  expect_equal(r$n_triangles, 1L)
  expect_equal(r$n_out_of_range, 3L)
  key <- morie_phacf3_canonical(0, 1, 0, 0, 0, 1)
  bit <- get(sprintf("%02d,%02d,%02d,%02d,%02d,%02d", key[1], key[2], key[3], key[4], key[5], key[6]),
             envir = sp$index)
  expect_identical(r$bits_on, bit)
  expect_equal(r$total, 1)
  expect_equal(r$density, 1 / r$n_bits)
  cm <- morie_phacf3(c(mol[1:3], mol[1:3]), c("donor", "acceptor"), ed, mode = "count", space = sp)
  # duplicated points give several triangles with the same key
  expect_gt(max(cm$fingerprint), 1)
  expect_equal(cm$total, cm$n_triangles)
  expect_equal(sum(cm$fingerprint), cm$n_triangles)
  # a distance matrix instead of coordinates: the triangle inequality fails
  deg <- list(list(0, 0, 0, "donor"), list(3, 0, 0, "donor"), list(60, 0, 0, "donor"))
  expect_equal(morie_phacf3(deg, c("donor", "acceptor"), c(2, 100), space =
                              morie_phacf3_space(c("donor", "acceptor"), 1, c(2, 100)))$n_degenerate, 0L)
  expect_equal(morie_phacf3(mol[1:2], c("donor", "acceptor"), ed, space = sp)$n_triangles, 0L)
  expect_error(morie_phacf3(mol, "donor", ed, space = sp), "not in the alphabet")
  expect_error(morie_phacf3(mol, c("donor", "acceptor"), ed, mode = "tversky"), "mode must be one of")
})

test_that("morie_phacf3_cheatsheet lists the modes", {
  expect_match(morie_phacf3_cheatsheet(), "binary, count", fixed = TRUE)
})
