# Anchors for reward machines and QRM (Toro Icarte et al. 2018, ICML).
#
# A reward machine is a deterministic object, so its semantics are
# checkable exactly rather than statistically: the formula compiler has a
# truth table, the transition takes the first matching edge, and a
# terminal state absorbs. The learner is then held to the one thing the
# paper guarantees on a problem this small, that it finds the optimum.

props <- c("a", "b", "c")
all_subsets <- lapply(0:7, function(m) props[as.logical(bitwAnd(m, c(1, 2, 4)))])

test_that("the formula compiler has the truth table it should", {
  # a conjunction of positives with negatives excluded
  f <- .rmrl_compile(list(c("a", "b"), "c"))
  for (s in all_subsets) {
    expect_identical(f(s), all(c("a", "b") %in% s) && !("c" %in% s))
  }
  # a bare proposition name
  g <- .rmrl_compile("a")
  for (s in all_subsets) expect_identical(g(s), "a" %in% s)
  # "true" holds under every assignment, including the empty one
  t_ <- .rmrl_compile("true")
  for (s in all_subsets) expect_true(t_(s))
  expect_true(t_(character(0)))
  # as does NULL
  expect_true(.rmrl_compile(NULL)(character(0)))
  # case does not matter for the constant
  expect_true(.rmrl_compile("TRUE")(character(0)))
  # negation only
  n <- .rmrl_compile(list(character(0), "c"))
  for (s in all_subsets) expect_identical(n(s), !("c" %in% s))
})

test_that("a malformed formula is refused", {
  expect_error(.rmrl_compile(list("a", "b", "c")), "formula must be")
  expect_error(.rmrl_compile(42), "formula must be")
})

test_that("machine construction collects the states it mentions", {
  m <- morie_rmrl_reward_machine(
    edges = list(list(0, "a", 1, 0), list(1, "b", 2, 1)),
    u0 = 0, terminal = c(2))
  expect_s3_class(m, "morie_reward_machine")
  expect_identical(m$u0, 0)
  expect_identical(m$terminal, "2")
  expect_setequal(m$states, c("0", "1", "2"))
  # one edge list per source state, in the order given
  expect_length(m$edges[["0"]], 1L)
  expect_length(m$edges[["1"]], 1L)
  expect_error(
    morie_rmrl_reward_machine(edges = list(list(0, "a", 1))),
    "each edge must be")
})

test_that("a step takes the first matching edge", {
  # two edges out of state 0 that both match on {a}: the first wins
  m <- morie_rmrl_reward_machine(
    edges = list(list(0, "a", 1, 5), list(0, "true", 9, -1)),
    u0 = 0)
  st <- morie_rmrl_machine_step(m, 0, "a")
  expect_identical(st$u, 1)
  expect_identical(st$reward, 5)
  # with no proposition true, the "true" edge is the one that matches
  st2 <- morie_rmrl_machine_step(m, 0, character(0))
  expect_identical(st2$u, 9)
  expect_identical(st2$reward, -1)
})

test_that("a state with no matching edge stays put and pays nothing", {
  m <- morie_rmrl_reward_machine(edges = list(list(0, "a", 1, 1)), u0 = 0)
  st <- morie_rmrl_machine_step(m, 0, "z")
  expect_identical(st$u, 0)
  expect_identical(st$reward, 0.0)
})

test_that("a terminal state absorbs", {
  m <- morie_rmrl_reward_machine(
    edges = list(list(0, "a", 1, 1), list(1, "a", 0, 7)),
    u0 = 0, terminal = c(1))
  st <- morie_rmrl_machine_step(m, 1, "a")
  expect_identical(st$u, 1)          # it does not leave
  expect_identical(st$reward, 0.0)   # and it pays nothing more
})

test_that("a run emits the state and reward sequence the machine dictates", {
  # reach a, then b; only the b pays
  m <- morie_rmrl_reward_machine(
    edges = list(list(0, "a", 1, 0), list(1, "b", 2, 1)),
    u0 = 0, terminal = c(2))
  r <- morie_rmrl_reward_machine_run(m, list("a", character(0), "b"))
  expect_identical(unlist(r$states), c(0, 1, 1, 2))
  expect_identical(r$rewards, c(0, 0, 1))
  expect_identical(r$total_reward, 1)
  expect_identical(r$final_state, 2)
  expect_true(r$accepted)
  expect_identical(r$estimate, r$states)
  expect_match(r$method, "Icarte")

  # the order is the whole point: b before a earns nothing
  r2 <- morie_rmrl_reward_machine_run(m, list("b", "b"))
  expect_identical(unlist(r2$states), c(0, 0, 0))
  expect_identical(r2$total_reward, 0)
  expect_false(r2$accepted)

  # once terminal, further labels add nothing
  r3 <- morie_rmrl_reward_machine_run(m, list("a", "b", "a", "b"))
  expect_identical(r3$total_reward, 1)
  expect_identical(r3$final_state, 2)

  # an empty label sequence leaves the machine where it started
  r4 <- morie_rmrl_reward_machine_run(m, list())
  expect_identical(unlist(r4$states), 0)
  expect_length(r4$rewards, 0L)
  expect_identical(r4$total_reward, 0)
})

# a corridor of five cells: "a" holds at the first, "b" at the last
corridor <- function() {
  list(states = 1:5, actions = c("L", "R"),
       step = function(s, a) max(1L, min(5L, s + if (identical(a, "R")) 1L else -1L)),
       label = function(s) if (s == 1L) "a" else if (s == 5L) "b" else character(0),
       machine = morie_rmrl_reward_machine(
         edges = list(list(0, "a", 1, 0), list(1, "b", 2, 1)),
         u0 = 0, terminal = c(2)))
}

test_that("QRM finds the optimal return on the corridor", {
  e <- corridor()
  q <- morie_rmrl(e$machine, e$states, e$actions, e$step, e$label,
                  episodes = 300L, horizon = 40L,
                  start = function() 1L, seed = 1L)
  expect_equal(q$mean_return_last, 1, tolerance = 1e-9)
  expect_length(q$returns, 300L)
  expect_identical(q$episodes, 300L)
  expect_match(q$method, "reward machine|QRM|Icarte")
  # one q-function per non-terminal machine state, the decomposition
  expect_gte(q$n_qfunctions, 1L)
  # the policy is defined over (task, machine state, environment state)
  expect_true(length(q$policy) > 0L)
  expect_true(all(unlist(q$policy) %in% e$actions))
})

test_that("the flat baseline also reaches the optimum, on the product state", {
  e <- corridor()
  f <- morie_rmrl_qlearn_flat(e$machine, e$states, e$actions, e$step, e$label,
                              episodes = 300L, horizon = 40L,
                              start = function() 1L, seed = 1L)
  expect_equal(f$mean_return_last, 1, tolerance = 1e-9)
  # both routes agree on what the task is worth, which is the point of the
  # comparison the paper draws
  q <- morie_rmrl(e$machine, e$states, e$actions, e$step, e$label,
                  episodes = 300L, horizon = 40L,
                  start = function() 1L, seed = 1L)
  expect_equal(f$mean_return_last, q$mean_return_last, tolerance = 1e-9)
})

test_that("a machine that cannot be satisfied earns nothing", {
  e <- corridor()
  # require a proposition no cell ever labels
  impossible <- morie_rmrl_reward_machine(
    edges = list(list(0, "z", 1, 1)), u0 = 0, terminal = c(1))
  q <- morie_rmrl(impossible, e$states, e$actions, e$step, e$label,
                  episodes = 60L, horizon = 20L,
                  start = function() 1L, seed = 1L)
  expect_equal(q$mean_return_last, 0, tolerance = 1e-12)
})

test_that("a learning run is reproducible from its seed", {
  e <- corridor()
  a <- morie_rmrl(e$machine, e$states, e$actions, e$step, e$label,
                  episodes = 80L, horizon = 20L, start = function() 1L, seed = 3L)
  b <- morie_rmrl(e$machine, e$states, e$actions, e$step, e$label,
                  episodes = 80L, horizon = 20L, start = function() 1L, seed = 3L)
  expect_equal(unlist(a$returns), unlist(b$returns), tolerance = 1e-15)
  d <- morie_rmrl(e$machine, e$states, e$actions, e$step, e$label,
                  episodes = 80L, horizon = 20L, start = function() 1L, seed = 4L)
  # On this corridor every episode reaches the goal inside the horizon, so
  # the return sequence is all ones whatever the seed. The exploration it
  # drives still differs, which shows in the learned values.
  expect_true(all(unlist(a$returns) == 1))
  expect_false(isTRUE(all.equal(a$q, d$q)))
  # and where the horizon is tight enough for the walk to fail, the
  # returns separate too
  tight <- function(sd) morie_rmrl(e$machine, e$states, e$actions, e$step,
                                   e$label, episodes = 60L, horizon = 5L,
                                   start = function() 1L, epsilon = 0.5,
                                   seed = sd)
  t3 <- tight(3L); t4 <- tight(4L)
  expect_false(isTRUE(all.equal(unlist(t3$returns), unlist(t4$returns))))
  expect_equal(unlist(tight(3L)$returns), unlist(t3$returns), tolerance = 1e-15)
})
