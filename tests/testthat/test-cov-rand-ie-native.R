# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/randIE_native.R (randomised interventional
# direct/indirect effects, Didelez, Dawid & Geneletti 2006). The
# mediator law, the g-formula mean sum_c P(c) sum_m p(m|a*,c) E[Y|a,m,c]
# and the weighting route are recomputed by hand on a small table where
# every cell is filled.

.ri_A <- c("1", "1", "1", "1", "0", "0", "0", "0", "1", "1", "0", "0")
.ri_M <- c("hi", "hi", "lo", "lo", "hi", "lo", "lo", "lo", "hi", "lo", "hi", "lo")
.ri_C <- c(rep("x", 6), rep("y", 6))
.ri_Y <- c(5, 7, 2, 4, 3, 1, 2, 0, 9, 4, 6, 2)

.ri_p <- function(a, c, lv, lap = 0) {
  v <- .ri_M[.ri_A == a & .ri_C == c]
  (sum(v == lv) + lap) / (length(v) + lap * 2)
}
.ri_ybar <- function(a, m, c) mean(.ri_Y[.ri_A == a & .ri_M == m & .ri_C == c])
.ri_psi <- function(a, astar) {
  tot <- 0
  for (cc in c("x", "y")) {
    pc <- mean(.ri_C == cc)
    for (lv in c("hi", "lo")) {
      w <- .ri_p(astar, cc, lv)
      if (w > 0) tot <- tot + pc * w * .ri_ybar(a, lv, cc)
    }
  }
  tot
}

test_that("mediator_distribution is the per-(arm, stratum) mediator law", {
  d <- morie_randIE_mediator_distribution(.ri_A, .ri_M, .ri_C)
  expect_identical(d$levels, c("hi", "lo"))
  expect_identical(d$arms, c("0", "1"))
  expect_identical(d$strata, c("x", "y"))
  expect_equal(d$n, 12L)
  expect_equal(unname(d$p[["1\rx"]]), c(.ri_p("1", "x", "hi"), .ri_p("1", "x", "lo")), tolerance = 1e-12)
  expect_equal(unname(d$p[["0\ry"]]), c(.ri_p("0", "y", "hi"), .ri_p("0", "y", "lo")), tolerance = 1e-12)
  for (k in names(d$p)) expect_equal(sum(d$p[[k]]), 1, tolerance = 1e-12)
  # Laplace smoothing moves probability towards the uniform law
  s <- morie_randIE_mediator_distribution(.ri_A, .ri_M, .ri_C, laplace = 1)
  expect_equal(unname(s$p[["1\rx"]]), c(.ri_p("1", "x", "hi", 1), .ri_p("1", "x", "lo", 1)),
               tolerance = 1e-12)
  # no strata: one pooled cell per arm
  n0 <- morie_randIE_mediator_distribution(.ri_A, .ri_M)
  expect_identical(n0$strata, "*")
  expect_equal(unname(n0$p[["1\r*"]][1]), mean(.ri_M[.ri_A == "1"] == "hi"), tolerance = 1e-12)
  expect_equal(morie_randIE(.ri_A, .ri_M, .ri_C)$n, 12L)
  expect_error(morie_randIE_mediator_distribution(character(0), .ri_M), "A is empty")
  expect_error(morie_randIE_mediator_distribution(.ri_A, .ri_M[-1]), "12 treatments but 11")
  expect_error(morie_randIE_mediator_distribution(.ri_A, .ri_M, .ri_C[-1]), "11 strata for 12")
})

test_that("interventional_mean follows the g-formula, and the weighting route agrees in form", {
  r <- morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, .ri_C, a = "1", a.star = "0")
  expect_equal(r$estimate, .ri_psi("1", "0"), tolerance = 1e-12)
  expect_equal(r$own.mediator.mean, mean(.ri_Y[.ri_A == "1"]), tolerance = 1e-12)
  expect_equal(r$n.arm, 6L)
  # psi(a, a) is not the arm mean: M is redrawn from its own law
  same <- morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, .ri_C, a = "1", a.star = "1")
  expect_equal(same$estimate, .ri_psi("1", "1"), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(same$estimate, same$own.mediator.mean)))
  # weighting: the mediator density ratio p(m|a*,c) / p(m|a,c) on arm a
  w <- morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, .ri_C, a = "1", a.star = "0",
                                        route = "weighting")
  num <- 0
  den <- 0
  for (i in seq_along(.ri_Y)) {
    if (.ri_A[i] != "1") next
    po <- .ri_p("1", .ri_C[i], .ri_M[i])
    if (po <= 0) next
    ww <- .ri_p("0", .ri_C[i], .ri_M[i]) / po
    num <- num + ww * .ri_Y[i]
    den <- den + ww
  }
  expect_equal(w$estimate, num / den, tolerance = 1e-12)
  expect_identical(w$route, "weighting")
  expect_error(morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, .ri_C, route = "tmle"),
               "route must be gformula or weighting")
  expect_error(morie_randIE_interventional_mean(.ri_Y[-1], .ri_A, .ri_M), "must agree in length")
  expect_error(morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, a = "2"), "not observed")
  expect_error(morie_randIE_interventional_mean(.ri_Y, .ri_A, .ri_M, a.star = "9"), "not observed")
  # an empty cell makes the parameter unidentified
  A2 <- .ri_A
  A2[.ri_M == "hi" & .ri_A == "1"] <- "0"
  expect_error(morie_randIE_interventional_mean(.ri_Y, A2, .ri_M, .ri_C, a = "1", a.star = "0"),
               "not identified from this sample")
})

test_that("the effect decomposition is exactly total = direct + indirect", {
  e <- morie_randIE_randomized_interventional_effect(.ri_Y, .ri_A, .ri_M, .ri_C)
  p11 <- .ri_psi("1", "1")
  p10 <- .ri_psi("1", "0")
  p00 <- .ri_psi("0", "0")
  p01 <- .ri_psi("0", "1")
  expect_equal(e$total, p11 - p00, tolerance = 1e-12)
  expect_equal(e$direct, p10 - p00, tolerance = 1e-12)
  expect_equal(e$indirect, p11 - p10, tolerance = 1e-12)
  expect_equal(e$direct.control.arm, p00 - p01, tolerance = 1e-12)
  expect_equal(e$psi[["10"]], p10, tolerance = 1e-12)
  d <- morie_randIE_decompose(e)
  expect_equal(d$residual, 0, tolerance = 1e-12)
  expect_equal(d$proportion.mediated, e$indirect / e$total, tolerance = 1e-12)
  expect_true(is.nan(morie_randIE_decompose(list(total = 0, direct = 0, indirect = 0))$proportion.mediated))
  w <- morie_randIE_randomized_interventional_effect(.ri_Y, .ri_A, .ri_M, .ri_C, route = "weighting")
  expect_equal(morie_randIE_decompose(w)$residual, 0, tolerance = 1e-12)
})
