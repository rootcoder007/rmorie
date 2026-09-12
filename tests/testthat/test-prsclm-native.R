# PLINK-style LD clumping followed by p-value thresholding into polygenic
# scores (Purcell et al. 2007; International Schizophrenia Consortium 2009;
# Choi & O'Reilly 2019).
#
# Clumping is fully deterministic, so the anchor is a design small enough
# to clump by hand: index variants are taken in ascending p-value order and
# a neighbour is absorbed only when it is both inside the window and above
# the r-squared threshold. The scores are then the plain weighted sums of
# the retained genotypes, computed here independently.

# five variants 100 bp apart; v1 and v2 are in LD with each other, the rest
# are independent. Ordered by p-value the indices are v4, v1, v2, v0, v3.
POS <- c(0, 100, 200, 300, 400)
PV <- c(0.5, 0.001, 0.002, 0.9, 1e-9)
BETA <- c(0.1, -0.4, 0.3, 0.05, 0.8)
LD <- diag(5)
LD[2, 3] <- 0.8
LD[3, 2] <- 0.8
SS <- list(beta = BETA, p = PV, position = POS,
           snp = c("v0", "v1", "v2", "v3", "v4"))

test_that("clumping takes index variants most-significant first", {
  r <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 1000,
                                 p_threshold = 1.0)
  # v4 is most significant and independent; v1 absorbs v2; v0 and v3 stand
  # alone. So four clumps with index variants 0, 1, 3, 4, reported 0-based.
  expect_equal(r$n_clumps, 4L)
  expect_equal(r$index_variants, c(0, 1, 3, 4))
  expect_equal(r$index_variant_names, c("v0", "v1", "v3", "v4"))
  # v2 belongs to v1's clump, everyone else to their own
  expect_equal(r$clump_of, c(0, 1, 1, 3, 4))
  expect_equal(r$clump_sizes, c(1L, 2L, 1L, 1L))
  expect_equal(r$clump_members[[2]], c(1, 2))
  # the module's own invariant: an index variant leads its clump
  expect_true(r$index_is_most_significant)
  expect_equal(r$n_variants, 5L)
  expect_equal(r$r2, 0.1)
  expect_equal(r$window, 1000)
  expect_equal(r$weights, BETA)
  expect_match(r$method, "Purcell")
})

test_that("the r-squared threshold decides what is absorbed", {
  # above the observed 0.8 nothing is absorbed, so every variant leads
  loose <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.9, window = 1000,
                                     p_threshold = 1.0)
  expect_equal(loose$n_clumps, 5L)
  expect_equal(loose$index_variants, 0:4)
  expect_equal(loose$clump_sizes, rep(1L, 5))
  # a threshold of zero absorbs every positively correlated neighbour
  tight <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0, window = 1000,
                                     p_threshold = 1.0)
  expect_equal(tight$n_clumps, 4L)
  expect_equal(tight$clump_sizes, c(1L, 2L, 1L, 1L))
  # perfectly correlated variants in one window collapse to one clump led by
  # the most significant of them
  ones <- matrix(1, 5, 5)
  all_one <- morie_prsclm_prs_cs_clump(SS, ones, r2 = 0.1, window = 1000,
                                       p_threshold = 1.0)
  expect_equal(all_one$n_clumps, 1L)
  expect_equal(all_one$index_variants, 4)
  expect_equal(all_one$clump_sizes, 5L)
  # independent variants are never absorbed whatever the threshold
  ind <- morie_prsclm_prs_cs_clump(SS, diag(5), r2 = 0, window = 1e9,
                                   p_threshold = 1.0)
  expect_equal(ind$n_clumps, 5L)
})

test_that("the window decides what is close enough to absorb", {
  # v1 and v2 are 100 bp apart, so a window below that cannot pair them
  narrow <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 50,
                                      p_threshold = 1.0)
  expect_equal(narrow$n_clumps, 5L)
  # exactly 100 does, since the comparison is inclusive
  edge <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 100,
                                    p_threshold = 1.0)
  expect_equal(edge$n_clumps, 4L)
  expect_equal(edge$clump_members[[2]], c(1, 2))
  # a zero window leaves every variant on its own
  zero <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 0,
                                    p_threshold = 1.0)
  expect_equal(zero$n_clumps, 5L)
  # positions default to the variant order when none are supplied
  nopos <- morie_prsclm_prs_cs_clump(list(beta = BETA, p = PV), LD,
                                     r2 = 0.1, window = 1,
                                     p_threshold = 1.0)
  expect_equal(nopos$n_clumps, 4L)
  expect_equal(nopos$index_variant_names, c("v0", "v1", "v3", "v4"))
})

test_that("thresholding keeps the index variants strictly below each cut", {
  thr <- c(1e-8, 0.01, 1.0)
  r <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 1000,
                                 p_threshold = thr)
  expect_equal(r$thresholds, thr)
  # only v4 clears 1e-8; v1 and v4 clear 0.01; all four clear 1.0
  expect_equal(r$n_retained, c(1L, 2L, 4L))
  expect_equal(r$retained[[1]], 4)
  expect_equal(r$retained[[2]], c(1, 4))
  expect_equal(r$retained[[3]], c(0, 1, 3, 4))
  expect_equal(r$estimate, as.numeric(r$n_retained))
  # thresholds are sorted and de-duplicated
  dup <- morie_prsclm_prs_cs_clump(SS, LD, p_threshold = c(1, 0.01, 1))
  expect_equal(dup$thresholds, c(0.01, 1))
  # the comparison is strict, so a threshold equal to a p-value excludes it
  exact <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 1000,
                                     p_threshold = 0.001)
  expect_equal(exact$n_retained, 1L)
  expect_equal(exact$retained[[1]], 4)
  # the default ladder is used when none is given
  d <- morie_prsclm_prs_cs_clump(SS, LD)
  expect_length(d$thresholds, 9L)
  expect_true(all(diff(d$n_retained) >= 0))
})

test_that("scores are the weighted sums of the retained genotypes", {
  set.seed(2)
  n <- 12
  G <- matrix(rbinom(n * 5, 2, 0.3), n, 5)
  thr <- c(1e-8, 0.01, 1.0)
  r <- morie_prsclm_prs_cs_clump(SS, LD, r2 = 0.1, window = 1000,
                                 p_threshold = thr, genotypes = G)
  expect_equal(r$n_individuals, n)
  # each threshold's score is the dot product over its retained variants
  for (u in seq_along(thr)) {
    keep <- r$retained[[u]] + 1
    expect_equal(r$scores_by_threshold[[u]],
                 as.numeric(G[, keep, drop = FALSE] %*% BETA[keep]))
  }
  # the headline score is the most stringent threshold that kept anything
  expect_equal(r$score_threshold, 1e-8)
  expect_equal(r$score, as.numeric(G[, 5, drop = FALSE] * BETA[5]))
  # a threshold that retains nothing scores everyone at zero
  none <- morie_prsclm_prs_cs_clump(SS, LD, p_threshold = 1e-12,
                                    genotypes = G)
  expect_equal(none$n_retained, 0L)
  expect_equal(none$scores_by_threshold[[1]], rep(0, n))
  expect_null(none$score)
  expect_null(none$score_threshold)
  # without genotypes there is nothing to score
  expect_null(morie_prsclm_prs_cs_clump(SS, LD)$score)
})

test_that("standardising the genotypes rescales the scores", {
  set.seed(4)
  n <- 20
  G <- matrix(rbinom(n * 5, 2, 0.4), n, 5)
  r <- morie_prsclm_prs_cs_clump(SS, LD, p_threshold = 1.0, genotypes = G,
                                 standardize = TRUE)
  expect_true(r$standardized)
  keep <- r$retained[[1]] + 1
  Z <- scale(G)
  expect_equal(r$scores_by_threshold[[1]],
               as.numeric(Z[, keep, drop = FALSE] %*% BETA[keep]),
               tolerance = 1e-10)
  # a standardised score is centred
  expect_equal(mean(r$scores_by_threshold[[1]]), 0, tolerance = 1e-10)
  # a monomorphic variant standardises to zero rather than dividing by zero
  Gm <- G
  Gm[, 5] <- 1
  rm_ <- morie_prsclm_prs_cs_clump(SS, LD, p_threshold = 1.0, genotypes = Gm,
                                   standardize = TRUE)
  expect_true(all(is.finite(rm_$scores_by_threshold[[1]])))
})

test_that("prsclm refuses input it cannot clump", {
  expect_error(morie_prsclm_prs_cs_clump(42, LD), "must be a mapping")
  expect_error(morie_prsclm_prs_cs_clump(list(p = PV), LD), "missing 'beta'")
  expect_error(morie_prsclm_prs_cs_clump(list(beta = BETA), LD), "missing 'p'")
  expect_error(morie_prsclm_prs_cs_clump(list(beta = numeric(0),
                                              p = numeric(0)), LD),
               "no variants")
  expect_error(morie_prsclm_prs_cs_clump(list(beta = BETA, p = PV[1:3]), LD),
               "effect sizes but .* p-values")
  expect_error(morie_prsclm_prs_cs_clump(list(beta = BETA,
                                              p = c(1.5, PV[2:5])), LD),
               "p-value outside")
  expect_error(morie_prsclm_prs_cs_clump(SS, diag(3)), "ld_ref must be")
  # a matrix of correlations rather than squared correlations is refused
  # outright rather than silently squared
  neg <- LD
  neg[1, 2] <- -0.5
  neg[2, 1] <- -0.5
  expect_error(morie_prsclm_prs_cs_clump(SS, neg),
               "squared correlations, not correlations")
  asym <- LD
  asym[1, 2] <- 0.5
  expect_error(morie_prsclm_prs_cs_clump(SS, asym), "not symmetric")
  expect_error(morie_prsclm_prs_cs_clump(SS, LD, r2 = 1.5),
               "r2 must be in")
  expect_error(morie_prsclm_prs_cs_clump(SS, LD, window = -1),
               "window cannot be negative")
  expect_error(morie_prsclm_prs_cs_clump(SS, LD, p_threshold = 2),
               "threshold outside")
  expect_error(morie_prsclm_prs_cs_clump(SS, LD,
                                         genotypes = matrix(0, 4, 3)),
               "genotypes must have 5 columns")
  # mis-sized annotations are caught
  expect_error(morie_prsclm_prs_cs_clump(list(beta = BETA, p = PV,
                                              position = c(1, 2)), LD),
               "variants but .* positions")
  expect_error(morie_prsclm_prs_cs_clump(list(beta = BETA, p = PV,
                                              snp = c("a", "b")), LD),
               "variants but .* names")
})
