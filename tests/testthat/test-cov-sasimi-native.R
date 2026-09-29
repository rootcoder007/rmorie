# Binary fingerprint coefficients (Willett, Barnard & Downs 1998),
# recomputed from set sizes of base-R intersect/union.

ss_a <- c(1, 4, 7, 9, 12, 20)
ss_b <- c(4, 7, 8, 12, 30)
ss_n <- function(a, b) c(a = length(a), b = length(b), c = length(intersect(a, b)))

test_that("fingerprints accept indices or 0/1 vectors", {
  expect_identical(sasimi_fingerprint(c(7, 1, 7, 4)), c(1L, 4L, 7L))
  expect_identical(sasimi_fingerprint(c(TRUE, FALSE, TRUE)), c(0L, 2L))
  expect_identical(sasimi_fingerprint(c(0, 1, 1, 0)), c(1L, 2L))
  # a 0/1 vector whose length is not n_bits is read as indices
  expect_identical(sasimi_fingerprint(c(0, 1), n_bits = 8), c(0L, 1L))
  expect_error(sasimi_fingerprint(c(-1, 3)), "negative")
  expect_error(sasimi_fingerprint(c(3, 9), n_bits = 8), "outside")
})

test_that("counts, Tanimoto, Dice, cosine and Tversky follow their formulas", {
  n <- ss_n(ss_a, ss_b)
  cn <- sasimi_counts(ss_a, ss_b)
  expect_identical(unlist(cn), c(a = 6L, b = 5L, c = 3L, union = length(union(ss_a, ss_b))))
  tan <- n[["c"]] / (n[["a"]] + n[["b"]] - n[["c"]])
  expect_equal(sasimi_tanimoto(ss_a, ss_b), tan, tolerance = 1e-12)
  expect_equal(sasimi_dice(ss_a, ss_b), 2 * n[["c"]] / (n[["a"]] + n[["b"]]), tolerance = 1e-12)
  expect_equal(sasimi_cosine(ss_a, ss_b), n[["c"]] / sqrt(n[["a"]] * n[["b"]]), tolerance = 1e-12)
  expect_equal(sasimi_tversky(ss_a, ss_b, 0.9, 0.1),
               3 / (0.9 * 3 + 0.1 * 2 + 3), tolerance = 1e-12)
  # Tversky identities: (1,1) is Tanimoto, (1/2,1/2) is Dice
  expect_equal(sasimi_tversky(ss_a, ss_b), tan, tolerance = 1e-12)
  expect_equal(sasimi_tversky(ss_a, ss_b, 0.5, 0.5), sasimi_dice(ss_a, ss_b), tolerance = 1e-12)
  # Dice = 2T / (1 + T)
  expect_equal(sasimi_dice(ss_a, ss_b), 2 * tan / (1 + tan), tolerance = 1e-12)
  expect_identical(sasimi_cosine(integer(0), ss_b), 0)
  expect_error(sasimi_tanimoto(integer(0), integer(0)), "both fingerprints are empty")
  expect_error(sasimi_tversky(ss_a, ss_b, -1, 1), "negative")
  expect_error(sasimi_tversky(1:3, 1:3 + 10, 0, 0), "vanishes")
})

test_that("Tanimoto distance obeys the triangle inequality on 4-bit prints", {
  fps <- lapply(1:15, function(k) as.integer(bitwAnd(k, c(1, 2, 4, 8)) > 0))
  D <- outer(seq_along(fps), seq_along(fps), Vectorize(function(i, j)
    sasimi_distance(fps[[i]], fps[[j]])))
  for (i in seq_along(fps)) for (j in seq_along(fps)) {
    expect_true(all(D[i, j] <= D[i, ] + D[, j] + 1e-12))
  }
  expect_equal(sasimi_distance(ss_a, ss_b, "dice"), 1 - sasimi_dice(ss_a, ss_b), tolerance = 1e-12)
  expect_error(sasimi_distance(ss_a, ss_b, "jaccard"), "coefficient must be")
})

test_that("similarity matrix and nearest neighbours", {
  fps <- list(ss_a, ss_b, c(1, 4, 7), c(50, 51))
  M <- sasimi_similarity_matrix(fps, "cosine")
  ref <- outer(1:4, 1:4, Vectorize(function(i, j) {
    if (i == j) return(1)
    k <- ss_n(fps[[i]], fps[[j]])
    k[["c"]] / sqrt(k[["a"]] * k[["b"]])
  }))
  expect_equal(M, ref, tolerance = 1e-12)
  expect_error(sasimi_similarity_matrix(list(ss_a)), "at least two")
  # bit 0 given as a logical vector must not be lost to re-normalisation
  lg <- list(c(TRUE, FALSE), c(TRUE, TRUE))
  expect_equal(sasimi_similarity_matrix(lg)[1, 2], 0.5, tolerance = 1e-12)
  expect_equal(sasimi_nearest_neighbours(c(TRUE, FALSE), lg)[[1]]$similarity, 1)
  nn <- sasimi_nearest_neighbours(c(1, 4, 7, 9), fps, k = 3)
  sc <- vapply(fps, function(f) sasimi_tanimoto(c(1, 4, 7, 9), f), numeric(1))
  ord <- order(-sc, seq_along(sc))
  expect_identical(vapply(nn, `[[`, 1L, "index"), ord[1:3] - 1L)
  expect_equal(vapply(nn, `[[`, 1, "similarity"), sc[ord[1:3]], tolerance = 1e-12)
  expect_length(sasimi_nearest_neighbours(ss_a, fps, k = 10), 4L)
  expect_length(sasimi_nearest_neighbours(ss_a, list()), 0L)
  expect_error(sasimi_nearest_neighbours(ss_a, fps, k = 0), "at least 1")
})

test_that("morie_sasimi reports the coefficient with the raw bit counts", {
  r <- morie_sasimi(ss_a, ss_b)
  expect_equal(r$similarity, sasimi_tanimoto(ss_a, ss_b), tolerance = 1e-12)
  expect_equal(r$distance, 1 - r$similarity, tolerance = 1e-12)
  expect_identical(c(r$bits_a, r$bits_b, r$bits_shared), c(6L, 5L, 3L))
  tv <- morie_sasimi(ss_a, ss_b, alpha = 0.9)
  expect_equal(tv$estimate, sasimi_tversky(ss_a, ss_b, 0.9, 1), tolerance = 1e-12)
  expect_identical(tv$coefficient, "Tversky(alpha=0.9, beta=1)")
  expect_identical(morie_sasimi(ss_a, ss_b, "dice")$coefficient, "dice")
})
