# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/toptor_native.R (Nilakantan et al. 1987 topological
# torsions). Torsion codes (NPI, TYPE, NBR) are derived by hand for
# butane, 2-methylbutane and Kekule benzene; similarity is
# 2 D / (d_i + d_j) and the trend vector (1/N) sum (a_i - A) S_i.

.tt_butane <- list(c("C", "C", "C", "C"), list(c(0, 1), c(1, 2), c(2, 3)))
.tt_ipent <- list(c("C", "C", "C", "C", "C"), list(c(0, 1), c(1, 2), c(2, 3), c(1, 4)))
.tt_benz <- list(rep("C", 6), list(c(0, 1, 2), c(1, 2, 1), c(2, 3, 2), c(3, 4, 1), c(4, 5, 2), c(5, 0, 1)))
.tt_chloro <- list(c("Cl", "C", "C", "Zz"), list(c(0, 1), c(1, 2), c(2, 3)))

test_that("topological torsions code four-atom paths once in canonical direction", {
  for (fn in list(topological_torsions, morie_topological_torsions)) {
    expect_identical(fn(.tt_butane[[1]], .tt_butane[[2]]), list("0:C:0|0:C:0|0:C:0|0:C:0" = 1L))
    # both paths through the branch atom (NBR 1 there) read the same way reversed
    expect_identical(fn(.tt_ipent[[1]], .tt_ipent[[2]]), list("0:C:0|0:C:0|0:C:1|0:C:0" = 2L))
    # Kekule benzene: every atom has one pi electron, six identical torsions
    expect_identical(fn(.tt_benz[[1]], .tt_benz[[2]]), list("1:C:1|1:C:0|1:C:0|1:C:1" = 6L))
    # uncommon elements become Y; the canonical direction is the smaller code
    expect_identical(names(fn(.tt_chloro[[1]], .tt_chloro[[2]])), "0:Cl:0|0:C:0|0:C:0|0:Y:0")
    expect_identical(names(fn(.tt_chloro[[1]], .tt_chloro[[2]], common_types = "C")), "0:Y:0|0:C:0|0:C:0|0:Y:0")
    expect_length(fn(c("C", "C", "C"), list(c(0, 1), c(1, 2))), 0L)
    expect_error(fn(character(0), list()), "no heavy atoms")
    expect_error(fn(c("C", "C"), list(c(0, 0))), "itself")
    expect_error(fn(c("C", "C"), list(c(0, 2))), "outside the molecule")
    expect_error(fn(c("C", "C"), list(c(0, 1, 0.5))), "below 1")
  }
})

test_that("torsion similarity is 2 D / (d_i + d_j) over distinct descriptors", {
  a <- list(x = 1L, y = 2L, z = 1L)
  b <- list(y = 1L, w = 3L)
  for (fn in list(torsion_similarity, morie_torsion_similarity)) {
    expect_equal(fn(a, b), 2 * 1 / (3 + 2))
    expect_equal(fn(c("x", "y"), c("x", "y")), 1)
    expect_equal(fn(list(), c("q")), 0)
    expect_error(fn(list(), list()), "undefined")
  }
})

test_that("trend vector weights descriptor presence by centred activity", {
  sets <- list(c("p", "q"), c("q"), c("r", "p"), c("q", "r"))
  act <- c(2, 5, 1, 4)
  keys <- c("p", "q", "r")
  S <- t(vapply(sets, function(s) as.numeric(keys %in% s), numeric(3)))
  expected <- colSums((act - mean(act)) * S) / 4
  r <- morie_trend_vector(sets, act, permutations = 6, seed = 2)
  expect_identical(r$descriptors, keys)
  expect_equal(r$vector, expected, tolerance = 1e-12)
  expect_equal(r$length, sqrt(sum(expected^2)), tolerance = 1e-12)
  # the null: Fisher-Yates shuffles of the activities on the package stream
  e <- .ghc_rng(2)
  lens <- numeric(6)
  for (k in 1:6) {
    o <- 1:4
    for (t in 4:2) {
      u <- floor(.ghc_unif(e, 1L) * t) + 1
      o[c(t, u)] <- o[c(u, t)]
    }
    lens[k] <- sqrt(sum((colSums((act[o] - mean(act)) * S) / 4)^2))
  }
  expect_equal(r$null_mean, mean(lens), tolerance = 1e-12)
  expect_equal(r$null_sd, sd(lens), tolerance = 1e-12)
  expect_equal(r$z, (r$length - mean(lens)) / sd(lens), tolerance = 1e-12)
  expect_error(morie_trend_vector(sets, act[-1]), "one activity")
  expect_error(morie_trend_vector(sets[1], act[1]), "at least two")
  expect_error(morie_trend_vector(sets, act, permutations = 0), "permutations")
  expect_error(morie_trend_vector(list(character(0), character(0)), c(1, 2)), "no descriptors")
})

test_that("morie_toptor describes one molecule or ranks many against a reference", {
  for (fn in list(morie_toptor, morie_topological_torsion)) {
    one <- fn(.tt_ipent[[1]], .tt_ipent[[2]])
    expect_equal(one$n_distinct, 1L)
    expect_equal(one$n_total, 2L)
    mols <- list(.tt_butane[[1]], .tt_benz[[1]], .tt_chloro[[1]])
    bonds <- list(.tt_butane[[2]], .tt_benz[[2]], .tt_chloro[[2]])
    many <- fn(lapply(mols, as.list), bonds, reference = .tt_butane)
    expect_equal(many$n_total, c(1L, 6L, 1L))
    expect_equal(many$similarity, c(1, 0, 0))
    expect_identical(many$ranking[1], 0L)
    tr <- fn(lapply(mols, as.list), bonds, activities = c(1, 3, 2), permutations = 4)
    expect_equal(tr$trend$vector,
                 morie_trend_vector(tr$torsions, c(1, 3, 2), permutations = 4)$vector, tolerance = 1e-12)
  }
})

test_that("morie_toptor_cheatsheet gives the similarity formula", {
  s <- morie_toptor_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "S = 2D/(d_i + d_j)", fixed = TRUE)
})
