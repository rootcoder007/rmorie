# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/muzero_native.R (Schrittwieser et al. 2020 MuZero
# MCTS). pUCT (eq. 2) and the discounted backup (eqs. 3-4) are
# recomputed by hand on small trees; the Marsaglia-Tsang gamma sampler
# is checked against its mean and variance; search counts one dynamics
# and one prediction call per simulation.

.mz_rep <- function(o) 0
.mz_dyn <- function(s, a) list(if (identical(a, "good")) 1 else 0, s + 1)
.mz_pred <- function(s) list(c(0.5, 0.5), 0)

test_that("muzero_gamma_rv draws Gamma(alpha, 1): mean and variance alpha", {
  set.seed(42)
  for (alpha in c(0.3, 2.5)) {
    x <- vapply(1:4000, function(i) muzero_gamma_rv(alpha), 0)
    # 4000 draws: the mean's standard error is sqrt(alpha / 4000)
    expect_lt(abs(mean(x) - alpha), 5 * sqrt(alpha / 4000))
    expect_lt(abs(var(x) / alpha - 1), 0.15)
    expect_true(all(x > 0))
  }
})

test_that("muzero_add_noise mixes the prior with normalised gamma draws", {
  p <- c(0.2, 0.3, 0.5)
  set.seed(7)
  g <- vapply(1:3, function(i) muzero_gamma_rv(0.3), 0)
  expect_equal(muzero_add_noise(p, 0.3, 0.25, 7), 0.75 * p + 0.25 * g / sum(g), tolerance = 1e-12)
  expect_equal(muzero_add_noise(p, 0.3, 0, 1), p)
  expect_equal(sum(muzero_add_noise(p, 1, 1, 3)), 1, tolerance = 1e-12)
  expect_error(muzero_add_noise(p, 0, 0.2, 1), "dirichlet_alpha")
  expect_error(muzero_add_noise(p, 1, 2, 1), "exploration_fraction")
})

test_that("muzero_MinMax normalises over the running range", {
  mm <- muzero_MinMax()
  expect_equal(mm$normalize(3), 3)
  mm$update(2)
  expect_equal(mm$normalize(3), 3)
  mm$update(6)
  mm$update(4)
  expect_equal(mm$normalize(3), 0.25)
  expect_equal(c(mm$lo, mm$hi), c(2, 6))
})

test_that("muzero_Node keeps a running mean and expands children", {
  nd <- muzero_Node(0.4)
  expect_equal(nd$value(), 0)
  nd$expand("s", c(0.7, 0.3), list("x", "y"))
  expect_true(nd$expanded)
  expect_equal(nd$children$y$prior, 0.3)
  expect_identical(nd$state, "s")
})

test_that("muzero_backup propagates the discounted return G = r + gamma G", {
  a <- muzero_Node()
  b <- muzero_Node()
  c <- muzero_Node()
  b$reward <- 1
  c$reward <- 2
  mm <- muzero_MinMax()
  muzero_backup(list(a, b, c), 5, 0.9, mm)
  expect_equal(c$value(), 5)
  expect_equal(b$value(), 2 + 0.9 * 5)
  expect_equal(a$value(), 1 + 0.9 * (2 + 0.9 * 5))
  expect_equal(c(mm$lo, mm$hi), c(5, 1 + 0.9 * 6.5))
})

test_that("muzero_select maximises normalised Q plus the pUCT bonus (eq. 2)", {
  nd <- muzero_Node()
  nd$expand(NULL, c(0.6, 0.4), list("a", "b"))
  nd$children$a$visits <- 3
  nd$children$a$value_sum <- 0.3
  nd$children$b$visits <- 1
  nd$children$b$value_sum <- 0.5
  mm <- muzero_MinMax()
  mm$update(0)
  mm$update(1)
  sc <- function(pr, n, q, c1, c2) q + pr * sqrt(4) / (1 + n) * (c1 + log((4 + c2 + 1) / c2))
  sa <- sc(0.6, 3, 0.1, 1.25, 19652)
  sb <- sc(0.4, 1, 0.5, 1.25, 19652)
  expect_identical(muzero_select(nd, c("a", "b"), mm, 1.25, 19652), c("a", "b")[which.max(c(sa, sb))])
  # a large exploration constant lets the prior dominate
  expect_identical(muzero_select(nd, c("a", "b"), mm, 50, 19652),
                   c("a", "b")[which.max(c(sc(0.6, 3, 0.1, 50, 19652), sc(0.4, 1, 0.5, 50, 19652)))])
})

test_that("muzero_search and morie_muzero prefer the rewarding action", {
  for (fn in list(muzero_search, morie_muzero)) {
    r <- fn(0, list("good", "bad"), .mz_rep, .mz_dyn, .mz_pred, simulations = 20, gamma = 0.9)
    v <- unlist(r$visits)
    expect_equal(sum(v), 20)
    expect_equal(unname(r$policy), unname(v / 20), tolerance = 1e-12)
    expect_gt(v[["good"]], v[["bad"]])
    expect_identical(r$action, "good")
    expect_equal(r$n_dynamics_calls, 20L)
    expect_equal(r$n_prediction_calls, 21L)
    t0 <- fn(0, list("good", "bad"), .mz_rep, .mz_dyn, .mz_pred, simulations = 8, temperature = 0)
    expect_equal(t0$policy, c(1, 0))
    t2 <- fn(0, list("good", "bad"), .mz_rep, .mz_dyn, .mz_pred, simulations = 8, temperature = 2)
    w <- sqrt(unlist(t2$visits))
    expect_equal(unname(t2$policy), unname(w / sum(w)), tolerance = 1e-12)
    nz <- fn(0, list("good", "bad"), .mz_rep, .mz_dyn, .mz_pred, simulations = 4, dirichlet_alpha = 0.3,
             exploration_fraction = 0.5, seed = 3)
    expect_equal(sum(unlist(nz$prior)), 1, tolerance = 1e-12)
    expect_error(fn(0, list(), .mz_rep, .mz_dyn, .mz_pred), "non-empty")
    expect_error(fn(0, list("a"), .mz_rep, .mz_dyn, .mz_pred, simulations = 0), "simulations")
    expect_error(fn(0, list("a"), .mz_rep, .mz_dyn, .mz_pred, c2 = 0), "c2")
    expect_error(fn(0, list("a"), .mz_rep, .mz_dyn, .mz_pred), "1 actions")
    expect_error(fn(0, list("a", "b"), .mz_rep, .mz_dyn, function(s) list(c(0, 0), 0)), "positive mass")
  }
  # the MuZero value of the root: with gamma = 0 each child's Q is its reward
  r <- morie_muzero(0, list("good", "bad"), .mz_rep, .mz_dyn, .mz_pred, simulations = 10, gamma = 0)
  expect_equal(unlist(r$Q), c(good = 1, bad = 0))
  v <- unlist(r$visits)
  expect_equal(r$value, v[["good"]] / 10)
  expect_error(muzero_search(0, list("a"), 1, .mz_dyn, .mz_pred), "representation must be callable")
})

test_that("muzero_cheatsheet names pUCT constants", {
  s <- muzero_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "c1=1.25, c2=19652", fixed = TRUE)
})
