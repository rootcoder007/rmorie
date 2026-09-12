# MuZero: MCTS over a learned latent model (Schrittwieser et al. 2020).
#
# Anchors outside the module: the paper's formulas written out longhand --
# the min-max normalisation, the node value as the mean of its backed-up
# returns, the PUCT selection score, and the discounted backup -- plus a
# deterministic bandit whose better action is known, so the search has to
# concentrate on it.

test_that("the min-max tracker and its normalisation are exact", {
  mm <- list(lo = NULL, hi = NULL)
  # an unset tracker cannot normalise, so it passes the value through
  expect_equal(.ghc_muzero_minmax_norm(mm, 3.5), 3.5)
  mm <- .ghc_muzero_minmax_update(mm, 2)
  expect_equal(mm$lo, 2)
  expect_equal(mm$hi, 2)
  # a degenerate range also passes through rather than dividing by zero
  expect_equal(.ghc_muzero_minmax_norm(mm, 2), 2)
  mm <- .ghc_muzero_minmax_update(mm, 10)
  expect_equal(mm$lo, 2)
  expect_equal(mm$hi, 10)
  # and then it is the linear map onto the unit interval
  expect_equal(.ghc_muzero_minmax_norm(mm, 2), 0)
  expect_equal(.ghc_muzero_minmax_norm(mm, 10), 1)
  expect_equal(.ghc_muzero_minmax_norm(mm, 6), 0.5)
  # the bounds only ever widen
  mm <- .ghc_muzero_minmax_update(mm, 5)
  expect_equal(c(mm$lo, mm$hi), c(2, 10))
  mm <- .ghc_muzero_minmax_update(mm, -1)
  expect_equal(mm$lo, -1)
})

test_that("a fresh node is empty and its value is its mean return", {
  nd <- .ghc_muzero_node_new(0.25)
  expect_equal(nd$visits, 0L)
  expect_equal(nd$value_sum, 0)
  expect_equal(nd$prior, 0.25)
  expect_length(nd$children, 0L)
  expect_null(nd$state)
  expect_equal(nd$reward, 0)
  expect_false(nd$expanded)
  # an unvisited node has no value to report
  expect_equal(.ghc_muzero_node_value(nd), 0)
  nd$visits <- 4L
  nd$value_sum <- 10
  expect_equal(.ghc_muzero_node_value(nd), 2.5)
})

test_that("expanding a node gives it one child per action", {
  nd <- .ghc_muzero_node_new(0)
  A <- as.list(c("a", "b", "c"))
  out <- .ghc_muzero_node_expand(nd, state = list(x = 1),
                                 prior = c(0.2, 0.3, 0.5), A)
  expect_true(out$expanded)
  expect_equal(out$state, list(x = 1))
  expect_length(out$children, 3L)
  expect_named(out$children, c("a", "b", "c"))
  # each child carries its own prior
  expect_equal(out$children[["a"]]$prior, 0.2)
  expect_equal(out$children[["b"]]$prior, 0.3)
  expect_equal(out$children[["c"]]$prior, 0.5)
  for (k in c("a", "b", "c")) {
    expect_equal(out$children[[k]]$visits, 0L)
    expect_false(out$children[[k]]$expanded)
  }
})

test_that("the backup writes the discounted return into the tree", {
  # A regression test. The tree used to be nested lists, which R copies on
  # assignment, so the visit counts and value sums went into throwaway
  # objects and the search ended at its own "no simulations reached the
  # root's children".
  n1 <- .ghc_muzero_node_new(0.5)
  n1$reward <- 1
  n2 <- .ghc_muzero_node_new(0.3)
  n2$reward <- 2
  mm <- .ghc_muzero_backup(list(n1, n2), value = 10, gamma = 0.9,
                           mm = list(lo = NULL, hi = NULL))$mm
  # both nodes on the path were visited once
  expect_equal(n1$visits, 1L)
  expect_equal(n2$visits, 1L)
  # the leaf receives the predicted value; its parent receives that value
  # discounted plus the reward on the transition into the leaf
  expect_equal(n2$value_sum, 10)
  expect_equal(n1$value_sum, 2 + 0.9 * 10)
  # and the tracker saw both node values
  expect_equal(mm$lo, 10)
  expect_equal(mm$hi, 11)
  # a second backup accumulates rather than replacing
  .ghc_muzero_backup(list(n1, n2), value = 0, gamma = 0.9,
                     mm = list(lo = NULL, hi = NULL))
  expect_equal(n2$visits, 2L)
  expect_equal(n2$value_sum, 10)
  expect_equal(n1$visits, 2L)
  expect_equal(n1$value_sum, 11 + 2)
})

test_that("selection maximises the PUCT score", {
  nd <- .ghc_muzero_node_new(0)
  A <- as.list(c("x", "y"))
  nd <- .ghc_muzero_node_expand(nd, list(), c(0.5, 0.5), A)
  c1 <- 1.25
  c2 <- 19652
  # with no visits anywhere the exploration term is zero for both and the
  # first action wins the tie
  expect_equal(.ghc_muzero_select(nd, A, list(lo = NULL, hi = NULL), c1, c2),
               "x")
  # give y a visit and a good value, x a visit and a bad one
  nd$children[["x"]]$visits <- 1L
  nd$children[["x"]]$value_sum <- 0
  nd$children[["y"]]$visits <- 1L
  nd$children[["y"]]$value_sum <- 1
  mm <- list(lo = 0, hi = 1)
  # recompute the score by hand: normalised Q plus the exploration bonus
  total <- 2
  bonus <- function(p, n) p * sqrt(total) / (1 + n) * (c1 + log((total + c2 + 1) / c2))
  sx <- 0 + bonus(0.5, 1)
  sy <- 1 + bonus(0.5, 1)
  expect_true(sy > sx)
  expect_equal(.ghc_muzero_select(nd, A, mm, c1, c2), "y")
  # a large enough prior on the unvisited-looking side can outweigh a small
  # value gap, which is what the bonus is for
  nd$children[["x"]]$prior <- 1
  nd$children[["y"]]$prior <- 1e-6
  nd$children[["y"]]$value_sum <- 0.01
  expect_equal(.ghc_muzero_select(nd, A, list(lo = 0, hi = 1), c1, c2), "x")
})

test_that("root noise mixes the prior without leaving the simplex", {
  prior <- c(0.2, 0.3, 0.5)
  # a zero fraction leaves the prior alone
  expect_equal(.ghc_muzero_add_noise(prior, alpha = 0.3, frac = 0, seed = 1L),
               prior)
  mixed <- .ghc_muzero_add_noise(prior, alpha = 0.3, frac = 0.25, seed = 1L)
  # the mixture of two distributions is a distribution
  expect_equal(sum(mixed), 1)
  expect_true(all(mixed >= 0))
  expect_length(mixed, 3L)
  expect_false(isTRUE(all.equal(mixed, prior)))
  # a full fraction discards the prior entirely, leaving pure noise
  pure <- .ghc_muzero_add_noise(prior, alpha = 0.3, frac = 1, seed = 1L)
  expect_equal(sum(pure), 1)
  # the draw is deterministic given the seed
  expect_equal(.ghc_muzero_add_noise(prior, 0.3, 0.25, 1L), mixed)
  expect_false(isTRUE(all.equal(.ghc_muzero_add_noise(prior, 0.3, 0.25, 9L),
                                mixed)))
  expect_error(.ghc_muzero_add_noise(prior, alpha = 0, frac = 0.25, seed = 1L),
               "dirichlet_alpha must be > 0")
  expect_error(.ghc_muzero_add_noise(prior, 0.3, frac = -0.1, seed = 1L),
               "exploration_fraction must lie in")
  expect_error(.ghc_muzero_add_noise(prior, 0.3, frac = 1.1, seed = 1L),
               "exploration_fraction must lie in")
})

test_that("the search concentrates on the rewarding action", {
  # a deterministic bandit: one action pays, the other does not
  repr <- function(o) list(s = 0)
  dyn <- function(s, a) list(if (identical(a, "1")) 1 else 0, list(s = 1))
  prd <- function(s) list(p = c(0.5, 0.5), v = 0.0)
  r <- morie_muzero(observation = list(0), actions = c("0", "1"),
                    representation = repr, dynamics = dyn, prediction = prd,
                    simulations = 200L)
  v <- unlist(r$visits)
  # every simulation reaches a root child, so the visits account for all
  expect_equal(sum(v), 200L)
  expect_equal(r$simulations, 200L)
  # the paying action attracts the visits and is the one returned
  expect_true(v[["1"]] > v[["0"]])
  expect_equal(r$action, "1")
  # its action value is the higher of the two
  expect_true(unlist(r$Q)[["1"]] > unlist(r$Q)[["0"]])
  # the policy is the visit distribution, and a distribution
  expect_equal(sum(unlist(r$policy)), 1)
  expect_equal(r$estimate, r$policy)
  expect_true(unlist(r$policy)[2] > unlist(r$policy)[1])
  # the model was queried once per simulation, plus once for the root
  expect_equal(r$n_dynamics_calls, 200L)
  expect_equal(r$n_prediction_calls, 201L)
  expect_true(is.finite(r$value))
  expect_named(r$prior, c("0", "1"))
  expect_match(r$method, "Schrittwieser")
})

test_that("a zero temperature is greedy and a high one flattens", {
  repr <- function(o) list(s = 0)
  dyn <- function(s, a) list(if (identical(a, "1")) 1 else 0, list(s = 1))
  prd <- function(s) list(p = c(0.5, 0.5), v = 0.0)
  base <- list(observation = list(0), actions = c("0", "1"),
               representation = repr, dynamics = dyn, prediction = prd,
               simulations = 200L)
  greedy <- do.call(morie_muzero, c(base, list(temperature = 0)))
  # all the mass on the most-visited action
  expect_equal(sum(unlist(greedy$policy)), 1)
  expect_equal(max(unlist(greedy$policy)), 1)
  expect_equal(greedy$action, "1")
  # a high temperature spreads the policy back out
  hot <- do.call(morie_muzero, c(base, list(temperature = 100)))
  expect_equal(sum(unlist(hot$policy)), 1)
  expect_true(max(unlist(hot$policy)) < max(unlist(greedy$policy)))
})

test_that("muzero validates its arguments", {
  repr <- function(o) list(s = 0)
  dyn <- function(s, a) list(0, list(s = 1))
  prd <- function(s) list(p = c(0.5, 0.5), v = 0)
  ok <- list(observation = list(0), actions = c("0", "1"),
             representation = repr, dynamics = dyn, prediction = prd,
             simulations = 4L)
  expect_error(morie_muzero(list(0), character(0), repr, dyn, prd),
               "actions must be non-empty")
  expect_error(do.call(morie_muzero, modifyList(ok, list(simulations = 0L))),
               "simulations must be >= 1")
  expect_error(do.call(morie_muzero, modifyList(ok, list(c2 = 0))),
               "c2 must be > 0")
  expect_error(do.call(morie_muzero, modifyList(ok, list(dynamics = "no"))),
               "dynamics must be callable")
  # a prediction returning the wrong number of priors is caught
  bad <- function(s) list(p = c(1, 1, 1), v = 0)
  expect_error(do.call(morie_muzero, modifyList(ok, list(prediction = bad))),
               "priors for 2 actions")
  # and one returning no mass at all
  zero <- function(s) list(p = c(0, 0), v = 0)
  expect_error(do.call(morie_muzero, modifyList(ok, list(prediction = zero))),
               "prior must have positive mass")
  # the alias points at the same routine
  expect_equal(names(formals(morie_muzero_mcts_search)),
               names(formals(morie_muzero)))
})
