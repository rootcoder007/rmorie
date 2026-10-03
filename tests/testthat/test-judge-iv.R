# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P14: the judge-leniency identities must agree with research/lean/P14Instrument.lean.

test_that("population decomposition: ITT and first stage split by type; Wald = complier effect without defiers", {
  set.seed(14)
  for (k in 1:40) {
    n <- 300
    d0 <- rbinom(n, 1, 0.3); d1 <- rbinom(n, 1, 0.6)
    y0 <- rnorm(n); y1 <- y0 + rnorm(n, 1)
    w <- runif(n)
    r <- morie_judge_iv_population(d0, d1, y0, y1, w)
    ww <- w / sum(w)
    comp <- d0 == 0 & d1 == 1; def <- d0 == 1 & d1 == 0
    expect_equal(r$itt, sum(ww[comp] * (y1 - y0)[comp]) - sum(ww[def] * (y1 - y0)[def]), tolerance = 1e-12)  # itt_decomposition
    expect_equal(r$first_stage, sum(ww[comp]) - sum(ww[def]), tolerance = 1e-12)                            # first_stage_decomposition
    expect_equal(sum(r$shares), 1, tolerance = 1e-12)
    pc <- r$shares[["complier"]]; pd <- r$shares[["defier"]]
    if (pd > 0 && pc != pd) {
      expect_equal(r$wald, (pc * r$effects[["complier"]] - pd * r$effects[["defier"]]) / (pc - pd), tolerance = 1e-9)  # wald_with_defiers
    }
    # impose monotonicity: no defiers
    d1m <- pmax(d0, d1)
    rm_ <- morie_judge_iv_population(d0, d1m, y0, y1, w)
    expect_true(rm_$monotone)
    expect_equal(rm_$wald, rm_$late, tolerance = 1e-9)                                                        # late_identification
  }
  # the witness: every effect positive, Wald negative (defiers_can_flip)
  r <- morie_judge_iv_population(d0 = c(0, 1), d1 = c(1, 0), y0 = c(0, 0), y1 = c(1, 4), weights = c(0.6, 0.4))
  expect_equal(r$wald, -5, tolerance = 1e-12)
  expect_true(all(r$effects[c("complier", "defier")] > 0))
})

test_that("observational Wald equals the population LATE exactly on a cloned design, and the sensitivity table inverts the identity", {
  set.seed(15)
  n <- 200
  d0 <- rbinom(n, 1, 0.2); d1 <- pmax(d0, rbinom(n, 1, 0.5))
  y0 <- rnorm(n); y1 <- y0 + runif(n, 0, 2)
  # every person appears once under each instrument value: observed means equal population means
  z <- rep(c(0, 1), each = n)
  d <- c(d0, d1); y <- c(ifelse(d0 == 1, y1, y0), ifelse(d1 == 1, y1, y0))
  obs <- morie_judge_iv(z, d, y)
  pop <- morie_judge_iv_population(d0, d1, y0, y1)
  expect_equal(obs$wald, pop$late, tolerance = 1e-12)
  expect_equal(obs$first_stage, pop$first_stage, tolerance = 1e-12)
  expect_equal(unname(obs$shares_if_monotone), unname(pop$shares[c("complier", "always", "never")]), tolerance = 1e-12)
  s <- obs$sensitivity
  expect_equal(s$implied_complier_effect[s$defier_share == 0], obs$wald, tolerance = 1e-12)
  # with an assumed defier share the implied complier effect satisfies wald_with_defiers
  row <- s[s$defier_share == 0.1, ][1, ]
  expect_equal(obs$wald, (row$complier_share * row$implied_complier_effect - 0.1 * row$defier_effect) / (row$complier_share - 0.1), tolerance = 1e-12)
  expect_error(morie_judge_iv(c(1, 1), c(0, 1), c(1, 2)), "both values")
  expect_error(morie_judge_iv_population(c(0, 2), c(1, 1), c(0, 0), c(1, 1)), "0/1")
  expect_error(morie_judge_iv_population(c(0), c(1, 1), c(0, 0), c(1, 1)), "equal length")
  expect_error(morie_judge_iv_population(c(0, 1), c(1, 1), c(0, 0), c(1, 1), weights = c(-1, 1)), "weights")
  expect_error(morie_judge_iv(c(0, 1), c(0), c(1, 2)), "equal length")
  expect_error(morie_judge_iv(c(0, 1), c(0, 2), c(1, 2)), "0/1")
  expect_error(morie_judge_iv(c(0, 1), c(0, 1), c(1, 2), weights = c(1)), "weights")
  expect_true(is.na(morie_judge_iv_population(c(1, 1), c(1, 1), c(0, 0), c(1, 1))$wald))
  expect_true(is.na(morie_judge_iv(c(0, 0, 1, 1), c(1, 1, 1, 1), c(1, 2, 3, 4))$wald))
})
