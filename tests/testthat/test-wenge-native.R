# Inverse-odds weighting for causal mediation (Tchetgen Tchetgen & Shpitser
# 2012).
#
# The module's own governing claim is the anchor: the three representations of
# the mediation functional theta_0 = E(Y_{1,M_0}) are the same number on a
# saturated model and diverge only once working models are imposed. That
# identity is exact, not asymptotic, so it is checked to machine precision.
# Separately the plug-in is checked against the empirical mediation formula
# computed here by hand, the estimates against a data-generating process whose
# theta_0, natural direct and natural indirect effects are known in closed
# form, and the reported effects against their defining decomposition.

# Y = 1 + 2E + 3M + 0.5X with M ~ Bernoulli(0.2 + 0.3E + 0.3X), X ~ Bern(0.5)
#   E(Y_e)     = 2.30 + 2.9e   so E(Y_1) = 5.20, E(Y_0) = 2.30
#   theta_0    = 1 + 2 + 3 * E[0.2 + 0.3X] + 0.5 * 0.5 = 4.30
#   NDE        = theta_0 - E(Y_0) = 2.00
#   NIE        = E(Y_1) - theta_0 = 0.90
gen <- function(n, seed) {
  set.seed(seed)
  X <- rbinom(n, 1, 0.5)
  E <- rbinom(n, 1, 0.3 + 0.4 * X)
  M <- rbinom(n, 1, 0.2 + 0.3 * E + 0.3 * X)
  Y <- 1 + 2 * E + 3 * M + 0.5 * X + rnorm(n, 0, 0.5)
  list(X = X, E = E, M = M, Y = Y)
}

# the empirical mediation formula, written out directly
hand_theta <- function(Y, E, M, X) {
  th <- 0
  for (x in sort(unique(X))) {
    px <- mean(X == x)
    for (m in sort(unique(M))) {
      i0 <- X == x & E == 0
      fm0 <- if (any(i0)) mean(M[i0] == m) else 0
      i1 <- X == x & E == 1 & M == m
      ey <- if (any(i1)) mean(Y[i1]) else 0
      th <- th + px * fm0 * ey
    }
  }
  th
}

test_that("cell keys are stable under equal numeric values", {
  expect_equal(.wenge_key(c(1, 2)), .wenge_key(c(1, 2)))
  expect_equal(.wenge_key(1), .wenge_key(1L))
  expect_false(.wenge_key(c(1, 2)) == .wenge_key(c(2, 1)))
  # the separator keeps neighbouring coordinates apart
  expect_false(.wenge_key(c(1, 23)) == .wenge_key(c(12, 3)))
  # values equal to twelve significant figures land in the same cell
  expect_equal(.wenge_key(1 / 3), .wenge_key(1 / 3 + 1e-15))
})

test_that("the saturated models are the empirical cell frequencies", {
  E <- c(0, 0, 1, 1, 1, 0, 1, 1)
  M <- c(0, 1, 0, 1, 1, 0, 0, 1)
  X <- c(0, 0, 0, 0, 1, 1, 1, 1)
  Mm <- matrix(M, ncol = 1)
  Xm <- matrix(X, ncol = 1)
  sm <- .wenge_saturated_models(E, Mm, Xm)
  # X = 0 has four rows of which two are exposed; X = 1 has four of which three
  expect_equal(sm$fe1(1), 0.5)
  expect_equal(sm$fe1(5), 0.75)
  # f(M = 0 | E = 0, X = 0): one of the two unexposed rows at X = 0
  expect_equal(sm$fm(1, 0, 0), 0.5)
  expect_equal(sm$fm(1, 0, 1), 0.5)
  # f(M | E = 1, X = 0): both exposed rows there have M = 0 and M = 1 once
  expect_equal(sm$fm(1, 1, 0), 0.5)
  # an unpopulated conditioning cell gives zero rather than an error
  expect_equal(sm$fm(1, 0, 99), 0)
  # each conditional distribution sums to one over the observed support
  for (i in c(1, 5)) {
    for (e in c(0, 1)) {
      expect_equal(sm$fm(i, e, 0) + sm$fm(i, e, 1), 1)
    }
  }
  ey <- .wenge_saturated_outcome(c(1, 2, 3, 4, 5, 6, 7, 8), E, Mm, Xm)
  # E(Y | E = 1, M = 0, X = 0) is the single row 3
  expect_equal(ey(1, 1, 0), 3)
  # E(Y | E = 1, M = 1, X = 1) averages rows 5 and 8
  expect_equal(ey(5, 1, 1), mean(c(5, 8)))
  expect_equal(ey(1, 1, 99), 0)
})

test_that("the three strategies agree exactly on a saturated model", {
  d <- gen(600, 4)
  a <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = "all")
  expect_named(a, c("ye", "em", "ym"), ignore.order = TRUE)
  expect_equal(a$ye, a$em)
  expect_equal(a$em, a$ym)
  # requesting one strategy returns that number, not a list
  for (s in c("ye", "em", "ym")) {
    got <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = s)
    expect_true(is.numeric(got) && length(got) == 1L)
    expect_equal(got, a[[s]])
  }
})

test_that("the agreement survives sparse cells and several confounders", {
  # a tiny design in which some (E, M, X) cells are empty
  X <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 1)
  E <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 0)
  M <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0)
  Y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 2)
  b <- morie_wenge_mediation_functional(Y, E, M, X, strategy = "all")
  expect_equal(b$ye, b$em)
  expect_equal(b$em, b$ym)
  expect_equal(b$ym, hand_theta(Y, E, M, X))
  # two binary confounders, so sixteen cells
  d <- gen(600, 11)
  X2 <- cbind(d$X, rbinom(600, 1, 0.4))
  c2 <- morie_wenge_mediation_functional(d$Y, d$E, d$M, X2, strategy = "all")
  expect_equal(c2$ye, c2$em)
  expect_equal(c2$em, c2$ym)
  # a three-level confounder
  set.seed(3)
  X3 <- sample(0:2, 600, replace = TRUE)
  E3 <- rbinom(600, 1, 0.25 + 0.2 * X3)
  M3 <- rbinom(600, 1, 0.2 + 0.2 * E3 + 0.15 * X3)
  Y3 <- 1 + 2 * E3 + 3 * M3 + 0.5 * X3 + rnorm(600, 0, 0.5)
  c3 <- morie_wenge_mediation_functional(Y3, E3, M3, X3, strategy = "all")
  expect_equal(c3$ye, c3$em)
  expect_equal(c3$em, c3$ym)
  expect_equal(c3$ym, hand_theta(Y3, E3, M3, X3))
})

test_that("the plug-in is the empirical mediation formula", {
  for (sd in c(4, 11, 21)) {
    d <- gen(500, sd)
    got <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = "ym")
    expect_equal(got, hand_theta(d$Y, d$E, d$M, d$X))
  }
})

test_that("the functional recovers the known theta_0", {
  d <- gen(4000, 99)
  for (s in c("ye", "em", "ym")) {
    got <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = s)
    expect_equal(got, 4.3, tolerance = 0.1)
  }
  # and it is not simply reporting either marginal mean
  expect_true(abs(morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X) -
                  mean(d$Y[d$E == 1])) > 0.4)
  expect_true(abs(morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X) -
                  mean(d$Y[d$E == 0])) > 1)
})

test_that("working models estimate the same target but need not agree", {
  d <- gen(2000, 7)
  p <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = "all",
                                        saturated = FALSE)
  for (s in c("ye", "em", "ym")) expect_equal(p[[s]], 4.3, tolerance = 0.25)
  # off a saturated model the three representations are no longer identical,
  # which is the whole reason the module implements all of them
  expect_true(diff(range(unlist(p))) > 1e-6)
  # the saturated route on the same data is still self-consistent
  a <- morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X, strategy = "all")
  expect_equal(a$ye, a$ym)
})

test_that("the effect decomposition holds identically", {
  d <- gen(600, 4)
  w <- morie_wenge_weight_based_mediation(d$E, d$M, d$X, d$Y, strategy = "all")
  # ey1 and ey0 are inverse-probability weighted, so they match the
  # Horvitz-Thompson estimator written out by hand and not the crude means
  fe1 <- vapply(d$X, function(x) mean(d$E[d$X == x]), numeric(1))
  expect_equal(w$ey1, sum((d$Y / fe1)[d$E == 1]) / 600)
  expect_equal(w$ey0, sum((d$Y / (1 - fe1))[d$E == 0]) / 600)
  expect_false(isTRUE(all.equal(w$ey1, mean(d$Y[d$E == 1]))))
  expect_equal(w$total, w$ey1 - w$ey0)
  # the natural direct effect changes the exposure with the mediator held at
  # its unexposed level; the indirect effect is the remainder
  expect_equal(w$nde, w$theta - w$ey0)
  expect_equal(w$nie, w$ey1 - w$theta)
  expect_equal(w$total, w$nde + w$nie)
  expect_equal(w$n, 600L)
  expect_equal(w$strategy, "all")
  expect_match(w$method, "Tchetgen Tchetgen")
  # the per-strategy functionals are reported and agree
  expect_equal(w$theta_ye, w$theta_em)
  expect_equal(w$theta_em, w$theta_ym)
  expect_equal(w$theta, w$theta_em)
})

test_that("the effects recover their closed-form values", {
  d <- gen(4000, 99)
  w <- morie_wenge_weight_based_mediation(d$E, d$M, d$X, d$Y)
  expect_equal(w$nde, 2.0, tolerance = 0.2)
  expect_equal(w$nie, 0.9, tolerance = 0.25)
  expect_equal(w$total, 2.9, tolerance = 0.2)
  # the weighted marginals land on E(Y_1) = 5.2 and E(Y_0) = 2.3, which the
  # crude subgroup means do not: X confounds the exposure here
  expect_equal(w$ey1, 5.2, tolerance = 0.2)
  expect_equal(w$ey0, 2.3, tolerance = 0.2)
  crude <- mean(d$Y[d$E == 1]) - mean(d$Y[d$E == 0])
  expect_true(abs(w$total - 2.9) < abs(crude - 2.9))
})

test_that("a mediator unaffected by the exposure has no indirect effect", {
  set.seed(31)
  n <- 4000
  X <- rbinom(n, 1, 0.5)
  E <- rbinom(n, 1, 0.3 + 0.4 * X)
  # M does not depend on E, so all of the effect is direct
  M <- rbinom(n, 1, 0.3 + 0.3 * X)
  Y <- 1 + 2 * E + 3 * M + 0.5 * X + rnorm(n, 0, 0.5)
  w <- morie_wenge_weight_based_mediation(E, M, X, Y)
  expect_equal(w$nie, 0, tolerance = 0.15)
  expect_equal(w$nde, 2, tolerance = 0.2)
})

test_that("an outcome unaffected by the exposure has no direct effect", {
  set.seed(32)
  n <- 4000
  X <- rbinom(n, 1, 0.5)
  E <- rbinom(n, 1, 0.3 + 0.4 * X)
  M <- rbinom(n, 1, 0.2 + 0.3 * E + 0.3 * X)
  # Y does not depend on E once M is fixed, so all of the effect is indirect
  Y <- 1 + 3 * M + 0.5 * X + rnorm(n, 0, 0.5)
  w <- morie_wenge_weight_based_mediation(E, M, X, Y)
  expect_equal(w$nde, 0, tolerance = 0.15)
  expect_equal(w$nie, 0.9, tolerance = 0.25)
})

test_that("absent mediators and confounders are handled", {
  d <- gen(600, 4)
  # with no mediator there is nothing to hold fixed, so theta_0 collapses to
  # the weighted mean of the exposed outcomes
  fe1 <- vapply(d$X, function(x) mean(d$E[d$X == x]), numeric(1))
  expect_equal(morie_wenge_mediation_functional(d$Y, d$E, NULL, d$X,
                                                strategy = "em"),
               sum((d$Y / fe1)[d$E == 1]) / 600)
  # with no confounder the three strategies still agree
  z <- morie_wenge_mediation_functional(d$Y, d$E, d$M, NULL, strategy = "all")
  expect_equal(z$ye, z$em)
  expect_equal(z$em, z$ym)
  expect_equal(z$ym, hand_theta(d$Y, d$E, d$M, rep(0, 600)))
})

test_that("the functional refuses inputs it cannot identify", {
  d <- gen(200, 4)
  expect_error(morie_wenge_mediation_functional(d$Y, d$E, d$M, d$X,
                                                strategy = "zz"),
               "strategy must be one of")
  expect_error(morie_wenge_mediation_functional(d$Y, d$X + d$E, d$M, d$X),
               "must be binary")
  expect_error(morie_wenge_mediation_functional(d$Y[1:5], d$E, d$M, d$X),
               "same length")
  # every unit at X = 0 is unexposed, so f(E|X) is degenerate there and the
  # functional is not identified
  expect_error(morie_wenge_mediation_functional(c(1, 2, 3, 4), c(0, 0, 1, 1),
                                                c(0, 1, 0, 1), c(0, 0, 1, 1)),
               "positivity fails")
  expect_error(morie_wenge_weight_based_mediation(d$E, d$M, d$X, d$Y,
                                                  strategy = "zz"),
               "strategy must be one of")
})

test_that("the cheatsheet is present", {
  expect_type(morie_wenge_cheatsheet(), "character")
  expect_match(morie_wenge_cheatsheet(), "wenge")
})

test_that("an empty conditioning cell gives zero density, not an error", {
  # nobody at X = 1 is unexposed, so f(M | E = 0, X = 1) conditions on a cell
  # that has no observations at all
  E <- c(0, 0, 1, 1)
  M <- c(0, 1, 0, 1)
  X <- c(0, 0, 1, 1)
  sm <- .wenge_saturated_models(E, matrix(M, ncol = 1), matrix(X, ncol = 1))
  expect_equal(sm$fm(3, 0, 0), 0)
  expect_equal(sm$fm(3, 0, 1), 0)
  # the populated cells are unaffected
  expect_equal(sm$fm(1, 0, 0), 0.5)
  expect_equal(sm$fe1(3), 1)
  expect_equal(sm$fe1(1), 0)
})

test_that("a mediator that is an exact function of E and X still estimates", {
  # M = E + 2X leaves the Gaussian mediator model with only rounding-level
  # residuals, so its density becomes extremely peaked. The estimate must stay
  # finite rather than dividing through by a vanishing variance.
  set.seed(41)
  n <- 60
  X <- rbinom(n, 1, 0.5)
  E <- rbinom(n, 1, 0.4)
  M <- E + 2 * X
  Y <- 1 + 2 * E + 3 * M + rnorm(n, 0, 0.5)
  got <- morie_wenge_mediation_functional(Y, E, M, X, strategy = "em",
                                          saturated = FALSE)
  expect_true(is.finite(got))
  # the saturated route has no working model at all and agrees with itself
  z <- morie_wenge_mediation_functional(Y, E, M, X, strategy = "all")
  expect_true(all(vapply(z, is.finite, logical(1))))
  expect_equal(z$ye, z$em)
  expect_equal(z$em, z$ym)
})

test_that("the effect route also runs on working models", {
  d <- gen(2000, 7)
  w <- morie_wenge_weight_based_mediation(d$E, d$M, d$X, d$Y, strategy = "all",
                                          saturated = FALSE)
  expect_false(w$saturated)
  expect_equal(w$total, w$nde + w$nie)
  expect_equal(w$nde, w$theta - w$ey0)
  expect_equal(w$nie, w$ey1 - w$theta)
  expect_equal(w$nde, 2.0, tolerance = 0.4)
  expect_equal(w$total, 2.9, tolerance = 0.3)
  # and it reports that it used working models, not cell means
  s <- morie_wenge_weight_based_mediation(d$E, d$M, d$X, d$Y)
  expect_true(s$saturated)
})
