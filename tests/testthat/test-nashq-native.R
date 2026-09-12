# Nash Q-learning for general-sum stochastic games (Hu & Wellman 2003).
#
# Anchors outside the module: the equilibria of classic two-by-two games,
# which are known exactly. The Prisoner's Dilemma has the single pure
# equilibrium at mutual defection, Matching Pennies the single mixed one at
# a half each, and Battle of the Sexes three -- both pure profiles and the
# mixed profile that makes each player indifferent, at (2/3, 1/3) and
# (1/3, 2/3). A solver that gets those three right is not guessing.

# rows are player 1's actions, columns player 2's
PD_A <- matrix(c(3, 0, 5, 1), 2, byrow = TRUE)
PD_B <- matrix(c(3, 5, 0, 1), 2, byrow = TRUE)
MP_A <- matrix(c(1, -1, -1, 1), 2, byrow = TRUE)
MP_B <- -MP_A
BS_A <- matrix(c(2, 0, 0, 1), 2, byrow = TRUE)
BS_B <- matrix(c(1, 0, 0, 2), 2, byrow = TRUE)

test_that("the Prisoner's Dilemma has one equilibrium, mutual defection", {
  e <- nash_equilibria_bimatrix(PD_A, PD_B)
  expect_length(e, 1L)
  expect_equal(e[[1]]$p, c(0, 1))
  expect_equal(e[[1]]$q, c(0, 1))
  # defection strictly dominates, so cooperation is not an equilibrium
  expect_false(.nashq_is_equilibrium(PD_A, PD_B, c(1, 0), c(1, 0), 1e-9))
  expect_true(.nashq_is_equilibrium(PD_A, PD_B, c(0, 1), c(0, 1), 1e-9))
})

test_that("Matching Pennies has one equilibrium, and it is mixed", {
  e <- nash_equilibria_bimatrix(MP_A, MP_B)
  expect_length(e, 1L)
  expect_equal(e[[1]]$p, c(0.5, 0.5))
  expect_equal(e[[1]]$q, c(0.5, 0.5))
  # no pure profile can be an equilibrium in a game with no pure one
  for (i in 1:2) {
    for (j in 1:2) {
      p <- c(0, 0)
      p[i] <- 1
      q <- c(0, 0)
      q[j] <- 1
      expect_false(.nashq_is_equilibrium(MP_A, MP_B, p, q, 1e-9))
    }
  }
  expect_true(.nashq_is_equilibrium(MP_A, MP_B, c(0.5, 0.5), c(0.5, 0.5),
                                    1e-9))
  # the value of the game is zero at equilibrium
  expect_equal(.nashq_payoff(MP_A, c(0.5, 0.5), c(0.5, 0.5)), 0)
  expect_equal(.nashq_payoff(MP_B, c(0.5, 0.5), c(0.5, 0.5)), 0)
})

test_that("Battle of the Sexes has both pure profiles and the mixed one", {
  e <- nash_equilibria_bimatrix(BS_A, BS_B)
  expect_length(e, 3L)
  ps <- lapply(e, function(z) z$p)
  qs <- lapply(e, function(z) z$q)
  # the two pure coordinated profiles
  expect_true(any(vapply(seq_along(e), function(k)
    isTRUE(all.equal(ps[[k]], c(1, 0))) &&
    isTRUE(all.equal(qs[[k]], c(1, 0))), logical(1))))
  expect_true(any(vapply(seq_along(e), function(k)
    isTRUE(all.equal(ps[[k]], c(0, 1))) &&
    isTRUE(all.equal(qs[[k]], c(0, 1))), logical(1))))
  # and the mixed one, where each player makes the other indifferent:
  # player one plays 2/3 so that player two's two columns pay the same,
  # and player two plays 1/3 so that player one's two rows pay the same
  expect_true(any(vapply(seq_along(e), function(k)
    isTRUE(all.equal(ps[[k]], c(2 / 3, 1 / 3))) &&
    isTRUE(all.equal(qs[[k]], c(1 / 3, 2 / 3))), logical(1))))
  # check the indifference directly on the mixed profile
  p <- c(2 / 3, 1 / 3)
  q <- c(1 / 3, 2 / 3)
  expect_equal(sum(p * BS_B[, 1]), sum(p * BS_B[, 2]))
  expect_equal(sum(q * BS_A[1, ]), sum(q * BS_A[2, ]))
  expect_true(.nashq_is_equilibrium(BS_A, BS_B, p, q, 1e-9))
})

test_that("expected payoffs are the bilinear form", {
  p <- c(0.3, 0.7)
  q <- c(0.6, 0.4)
  expect_equal(.nashq_payoff(PD_A, p, q),
               as.numeric(t(p) %*% PD_A %*% q))
  # a pure profile reads one cell
  expect_equal(.nashq_payoff(PD_A, c(1, 0), c(0, 1)), PD_A[1, 2])
  expect_equal(.nashq_payoff(PD_A, c(0, 1), c(1, 0)), PD_A[2, 1])
  # the form is linear in each argument separately
  expect_equal(.nashq_payoff(PD_A, c(0.5, 0.5), q),
               0.5 * .nashq_payoff(PD_A, c(1, 0), q) +
                 0.5 * .nashq_payoff(PD_A, c(0, 1), q))
})

test_that("the linear solve agrees with base R", {
  set.seed(3)
  for (n in c(1, 2, 4)) {
    A <- crossprod(matrix(rnorm(n * n), n)) + diag(n)
    b <- rnorm(n)
    expect_equal(unname(.nashq_solve(A, b)), as.numeric(solve(A, b)),
                 tolerance = 1e-9)
  }
})

test_that("payoff matrices are coerced to double", {
  expect_equal(.nashq_mat(matrix(1:4, 2), "A"), matrix(as.double(1:4), 2))
  expect_equal(storage.mode(.nashq_mat(matrix(1:4, 2), "A")), "double")
  # a one-by-one game is a degenerate but legal payoff matrix
  expect_equal(.nashq_mat(matrix(7L, 1), "A"), matrix(7, 1))
})

test_that("the stage game is classified by its equilibria", {
  # the Prisoner's Dilemma equilibrium is a saddle: neither player can gain
  # by deviating and the opponent cannot be made worse off unilaterally
  pd <- stage_game_type(PD_A, PD_B)
  expect_equal(pd$n_equilibria, 1L)
  expect_equal(pd$estimate, 1)
  expect_true(pd$has_saddle)
  expect_length(pd$saddle, 1L)
  expect_equal(pd$saddle[[1]]$p, c(0, 1))
  expect_false(pd$has_global_optimal)
  expect_length(pd$global_optimal, 0L)
  expect_match(pd$method, "Hu & Wellman")

  # a zero-sum game's equilibrium is a saddle by construction
  mp <- stage_game_type(MP_A, MP_B)
  expect_equal(mp$n_equilibria, 1L)
  expect_true(mp$has_saddle)
  expect_equal(mp$saddle[[1]]$p, c(0.5, 0.5))

  # Battle of the Sexes has three equilibria and no single profile that is
  # best for both players at once
  bs <- stage_game_type(BS_A, BS_B)
  expect_equal(bs$n_equilibria, 3L)
  expect_equal(bs$estimate, 3)
  expect_false(bs$has_global_optimal)

  # a game of pure common interest does have a globally optimal profile
  CO <- matrix(c(5, 0, 0, 1), 2, byrow = TRUE)
  co <- stage_game_type(CO, CO)
  expect_true(co$has_global_optimal)
  expect_true(length(co$global_optimal) >= 1L)
  # and that profile is the one paying both players the most
  go <- co$global_optimal[[1]]
  expect_equal(.nashq_payoff(CO, go$p, go$q), 5)
})

test_that("saddle and global-optimal tests agree with their definitions", {
  expect_true(.nashq_is_saddle(MP_A, MP_B, c(0.5, 0.5), c(0.5, 0.5), 1e-9))
  expect_true(.nashq_is_saddle(PD_A, PD_B, c(0, 1), c(0, 1), 1e-9))
  # a profile that is not an equilibrium cannot be a saddle
  expect_false(.nashq_is_saddle(PD_A, PD_B, c(1, 0), c(1, 0), 1e-9))
})

test_that("Nash Q-learning on a repeated game finds the stage equilibrium", {
  # one state, so the stochastic game is the Prisoner's Dilemma played
  # repeatedly and the Nash Q values must rank mutual defection first
  step <- function(s, a1, a2) "s"
  rewards <- function(s, a1, a2, s1) {
    i <- if (identical(a1, "C")) 1L else 2L
    j <- if (identical(a2, "C")) 1L else 2L
    c(PD_A[i, j], PD_B[i, j])
  }
  r <- morie_nashq(states = list("s"), actions = list(list("C", "D"),
                                                      list("C", "D")),
                   step = step, rewards = rewards, gamma = 0.5, alpha = 0.5,
                   epsilon = 0.3, episodes = 60L, horizon = 8L, seed = 1L)
  expect_true(is.list(r))
  expect_true(all(c("estimate", "method") %in% names(r)))
  expect_match(r$method, "Nash Q")
  # the run is reproducible from its seed
  again <- morie_nashq(states = list("s"),
                       actions = list(list("C", "D"), list("C", "D")),
                       step = step, rewards = rewards, gamma = 0.5,
                       alpha = 0.5, epsilon = 0.3, episodes = 60L,
                       horizon = 8L, seed = 1L)
  expect_equal(r$estimate, again$estimate)
  # a different seed explores differently
  other <- morie_nashq(states = list("s"),
                       actions = list(list("C", "D"), list("C", "D")),
                       step = step, rewards = rewards, gamma = 0.5,
                       alpha = 0.5, epsilon = 0.3, episodes = 60L,
                       horizon = 8L, seed = 7L)
  expect_true(is.list(other))
})

test_that("nashq validates its arguments", {
  step <- function(s, a1, a2) "s"
  rewards <- function(s, a1, a2, s1) c(0, 0)
  ok <- list(states = list("s"),
             actions = list(list("C", "D"), list("C", "D")),
             step = step, rewards = rewards, episodes = 2L, horizon = 2L)
  expect_error(do.call(morie_nashq, c(ok, list(selection = "bogus"))),
               "selection must be one of")
  expect_error(morie_nashq(states = list("s"), actions = list(list("C")),
                           step = step, rewards = rewards),
               "covers two players")
  expect_error(morie_nashq(states = list(),
                           actions = list(list("C"), list("D")),
                           step = step, rewards = rewards),
               "must be non-empty")
  expect_error(morie_nashq(states = list("s"),
                           actions = list(list(), list("D")),
                           step = step, rewards = rewards),
               "must be non-empty")
  expect_error(morie_nashq(states = list("s"),
                           actions = list(list("C"), list("D")),
                           step = "not a function", rewards = rewards),
               "must be callable")
  expect_error(morie_nashq(states = list("s"),
                           actions = list(list("C"), list("D")),
                           step = step, rewards = "not a function"),
               "must be callable")
  expect_type(.nashq_cheatsheet(), "character")
})
