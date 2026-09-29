# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tncomp_native.R (Snarey et al. 1997 diverse subset
# selection). Tanimoto distances are computed by hand from the bit
# sets, and both greedy rules are re-run step by step (the seed is the
# compound with the largest total distance, ties go to the lower index).

.tn_fps <- c("11000", "11100", "00111", "00011", "10101")
.tn_bits <- lapply(.tn_fps, function(s) which(strsplit(s, "")[[1]] == "1"))
.tn_D <- local({
  n <- length(.tn_bits)
  D <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) {
    if (i == j) next
    a <- .tn_bits[[i]]
    b <- .tn_bits[[j]]
    D[i, j] <- 1 - length(intersect(a, b)) / length(union(a, b))
  }
  D
})
.tn_greedy <- function(D, k, rule, seed = NULL) {
  n <- nrow(D)
  ch <- if (is.null(seed)) which.max(colSums(D)) else as.integer(seed) + 1L
  while (length(ch) < k) {
    rest <- setdiff(seq_len(n), ch)
    sc <- vapply(rest, function(i) if (rule == "maxmin") min(D[i, ch]) else sum(D[i, ch]), 0)
    ch <- c(ch, min(rest[sc == max(sc)]))
  }
  ch
}

test_that("distance_matrix is one minus the Tanimoto coefficient", {
  D <- distance_matrix(.tn_fps)
  expect_equal(D, .tn_D, tolerance = 1e-12)
  expect_equal(diag(D), rep(0, 5))
  expect_equal(D, t(D))
  # 11000 vs 11100: intersection 2, union 3
  expect_equal(D[1, 2], 1 - 2 / 3, tolerance = 1e-12)
  # 11000 vs 00111: disjoint
  expect_equal(D[1, 3], 1)
  # the same bits in the other accepted forms
  expect_equal(distance_matrix(list(c(TRUE, TRUE, FALSE, FALSE, FALSE),
                                    c(TRUE, TRUE, TRUE, FALSE, FALSE)))[1, 2],
               1 - 2 / 3, tolerance = 1e-12)
  expect_equal(distance_matrix(list(c(1, 2), c(1, 2, 3)))[1, 2], 1 - 2 / 3, tolerance = 1e-12)
  # all-zero numeric fingerprints are read as bit lists, so {0} vs {0}
  # has Tanimoto 1 only for a shared bit; two empty bit sets give 0
  expect_equal(distance_matrix(list(integer(0), integer(0)))[1, 2], 1)
  expect_error(distance_matrix(.tn_fps[1]), "at least two compounds")
})

test_that("maxmin and maxsum selections follow the greedy rules", {
  expect_identical(maxmin_selection(.tn_fps, 3), .tn_greedy(.tn_D, 3, "maxmin"))
  expect_identical(maxsum_selection(.tn_fps, 3), .tn_greedy(.tn_D, 3, "maxsum"))
  expect_identical(maxmin_selection(.tn_fps, 3, seed = 0), .tn_greedy(.tn_D, 3, "maxmin", 0))
  expect_identical(maxmin_selection(.tn_fps, 1, seed = 2), 3L)
  expect_identical(maxmin_selection(.tn_fps, 5), .tn_greedy(.tn_D, 5, "maxmin"))
  # maxmin keeps the minimum pairwise distance high; maxsum need not
  mm <- diversity(.tn_fps, maxmin_selection(.tn_fps, 3))
  ms <- diversity(.tn_fps, maxsum_selection(.tn_fps, 3))
  expect_gte(mm$min_distance, ms$min_distance)
  expect_error(maxmin_selection(.tn_fps, 0), "k must lie in \\[1, 5\\]")
  expect_error(maxmin_selection(.tn_fps, 6), "k must lie in \\[1, 5\\]")
  expect_error(maxmin_selection(.tn_fps, 2, seed = 5), "not a compound index")
})

test_that("diversity summarises the pairwise distances of a subset", {
  d <- diversity(.tn_fps, c(1, 3, 5))
  vals <- c(.tn_D[1, 3], .tn_D[1, 5], .tn_D[3, 5])
  expect_equal(d$min_distance, min(vals), tolerance = 1e-12)
  expect_equal(d$mean_distance, mean(vals), tolerance = 1e-12)
  expect_equal(d$max_distance, max(vals), tolerance = 1e-12)
  expect_equal(d$n_pairs, 3L)
  expect_equal(diversity(.tn_fps, c(1, 2), D = .tn_D)$mean_distance, .tn_D[1, 2], tolerance = 1e-12)
  expect_error(diversity(.tn_fps, 1), "at least two selected")
  expect_error(diversity(.tn_fps, c(1, 1)), "repeats a compound")
})

test_that("morie_tncomp reports the selection with its diversity", {
  r <- morie_tncomp(.tn_fps, 3)
  expect_identical(r$selection, .tn_greedy(.tn_D, 3, "maxmin"))
  expect_identical(r$estimate, r$selection)
  expect_equal(r$seed, r$selection[1])
  expect_equal(r$n_compounds, 5L)
  expect_equal(r$min_distance, diversity(.tn_fps, r$selection)$min_distance, tolerance = 1e-12)
  s <- morie_tncomp(.tn_fps, 3, objective = "maxsum", seed = 1)
  expect_identical(s$selection, .tn_greedy(.tn_D, 3, "maxsum", 1))
  expect_equal(s$seed, 2L)
  one <- morie_tncomp(.tn_fps, 1, seed = 0)
  expect_null(one$min_distance)
  expect_error(morie_tncomp(.tn_fps, 2, objective = "sphere"), "objective must be one of")
})
