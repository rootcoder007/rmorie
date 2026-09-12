# Conservative Q-Learning for offline RL (Kumar et al. 2020, arXiv:2006.04779).
#
# Anchors independent of the module: with alpha = 0 the objective is ordinary
# fitted Q-iteration, so its fixed point must equal the value-iteration
# solution of the underlying MDP (computed here by a separate loop); with
# backup = "pi" it must equal the policy-evaluation solution of the linear
# system (I - gamma P_pi) Q = r, solved with base::solve; and Theorem 3.1
# requires the learned Q to lower-bound the true Q, with the gap widening in
# alpha. The penalty and objective are recomputed from the returned Q rather
# than trusted.

# A deterministic MDP: states A,B and actions L,R.
#   (A,L) -> A r=0    (A,R) -> B r=1
#   (B,L) -> A r=0    (B,R) -> B r=2
SS <- c("A", "B")
AA <- c("L", "R")
GAM <- 0.9
NXT <- list("A\rL" = "A", "A\rR" = "B", "B\rL" = "A", "B\rR" = "B")
REW <- list("A\rL" = 0, "A\rR" = 1, "B\rL" = 0, "B\rR" = 2)
KS <- names(NXT)

# independent value iteration for the optimal action values
q_star <- function(gamma = GAM) {
  Q <- stats::setNames(rep(0, 4), KS)
  for (it in seq_len(20000)) {
    V <- vapply(SS, function(s) max(Q[paste0(s, "\r", AA)]), numeric(1))
    Qn <- vapply(KS, function(k) REW[[k]] + gamma * V[[NXT[[k]]]], numeric(1))
    if (max(abs(Qn - Q)) < 1e-15) return(Qn)
    Q <- Qn
  }
  Q
}

# independent policy evaluation: (I - gamma P_pi) Q = r
q_pi <- function(pol, gamma = GAM) {
  M <- matrix(0, 4, 4, dimnames = list(KS, KS))
  for (k in KS) {
    for (b in AA) {
      M[k, paste0(NXT[[k]], "\r", b)] <- pol[[paste0(NXT[[k]], "\r", b)]]
    }
  }
  as.numeric(solve(diag(4) - gamma * M, unlist(REW)[KS]))
}

make_data <- function(reps = list("A\rL" = 10, "A\rR" = 30,
                                  "B\rL" = 10, "B\rR" = 25)) {
  d <- list()
  for (k in KS) {
    s <- substr(k, 1, 1)
    a <- substr(k, 3, 3)
    for (j in seq_len(reps[[k]])) {
      d[[length(d) + 1L]] <- list(s, a, REW[[k]], NXT[[k]])
    }
  }
  d
}

test_that("log-sum-exp and softmax match their closed forms", {
  v <- c(-2, 0.5, 1, 3)
  expect_equal(.offlrl_logsumexp(v), log(sum(exp(v))))
  # the shifted form survives arguments that would overflow
  expect_equal(.offlrl_logsumexp(c(800, 801)), 801 + log(1 + exp(-1)))
  expect_true(is.finite(.offlrl_logsumexp(c(800, 801))))
  expect_equal(.offlrl_logsumexp(4), 4)
  # log-sum-exp dominates the maximum, which dominates any weighted average
  expect_true(.offlrl_logsumexp(v) > max(v))

  p <- .offlrl_softmax(v)
  expect_equal(sum(p), 1)
  expect_equal(p, exp(v) / sum(exp(v)))
  expect_true(all(p > 0))
  # softmax is invariant to a constant shift and is the gradient of logsumexp
  expect_equal(.offlrl_softmax(v + 100), p)
  expect_equal(p[which.max(v)], max(p))
  expect_equal(.offlrl_softmax(rep(2, 5)), rep(0.2, 5))
})

test_that("conditional distributions are validated per state", {
  Sl <- as.list(SS)
  Al <- as.list(AA)
  expect_null(.offlrl_as_dist(NULL, Sl, Al, "policy"))
  # a function of (s, a)
  d <- .offlrl_as_dist(function(s, a) if (a == "L") 0.25 else 0.75,
                       Sl, Al, "policy")
  expect_equal(d[["A\rL"]], 0.25)
  expect_equal(d[["B\rR"]], 0.75)
  expect_length(d, 4L)
  # a list keyed the same way, with absent entries read as zero
  d2 <- .offlrl_as_dist(list("A\rL" = 1, "B\rR" = 1), Sl, Al, "policy")
  expect_equal(d2[["A\rL"]], 1)
  expect_equal(d2[["A\rR"]], 0)
  # rows that do not sum to one are rejected, and the message names the state
  expect_error(.offlrl_as_dist(function(s, a) 0.3, Sl, Al, "mu"),
               "sums to")
  expect_error(.offlrl_as_dist(list("A\rL" = 1, "A\rR" = 0), Sl, Al, "policy"),
               "B")
  expect_error(.offlrl_as_dist(42, Sl, Al, "policy"),
               "must be a function or a list")
})

test_that("alpha = 0 reproduces the value-iteration fixed point", {
  qs <- q_star()
  # sanity: the independent solution is the hand-computable one
  expect_equal(as.numeric(qs[KS]), c(17.1, 19, 17.1, 20), tolerance = 1e-9)
  r <- offlrl(make_data(), states = SS, actions = AA, alpha = 0, gamma = GAM,
              lr = 0.5, iters = 200000, tol = 1e-14)
  got <- unlist(r$q)[KS]
  expect_equal(as.numeric(got), as.numeric(qs[KS]), tolerance = 1e-8)
  # at the fitted-Q fixed point the Bellman residual is zero
  expect_equal(r$bellman_error, 0, tolerance = 1e-12)
  # with alpha zero the penalty does not enter the objective
  expect_equal(r$objective, r$bellman_error)
  expect_equal(r$alpha, 0)
  # the greedy policy and state values follow from Q
  expect_equal(unlist(r$value)[["A"]], 19, tolerance = 1e-8)
  expect_equal(unlist(r$value)[["B"]], 20, tolerance = 1e-8)
  expect_equal(unlist(r$greedy)[["A"]], "R")
  expect_equal(unlist(r$greedy)[["B"]], "R")
  expect_equal(r$n_transitions, 75L)
  expect_match(r$method, "Kumar")
  # estimate and q are the same object
  expect_equal(r$estimate, r$q)
})

test_that("a different discount moves the fixed point as value iteration does", {
  for (g in c(0.5, 0.8, 0.95)) {
    qs <- q_star(g)
    r <- offlrl(make_data(), states = SS, actions = AA, alpha = 0, gamma = g,
                lr = 0.5, iters = 200000, tol = 1e-14)
    expect_equal(as.numeric(unlist(r$q)[KS]), as.numeric(qs[KS]),
                 tolerance = 1e-8)
  }
})

test_that("backup = 'pi' solves the policy-evaluation system", {
  pol <- list("A\rL" = 0.25, "A\rR" = 0.75, "B\rL" = 0.25, "B\rR" = 0.75)
  want <- q_pi(pol)
  r <- offlrl(make_data(), states = SS, actions = AA, alpha = 0, gamma = GAM,
              backup = "pi", policy = pol, lr = 0.5, iters = 300000,
              tol = 1e-14)
  expect_equal(as.numeric(unlist(r$q)[KS]), want, tolerance = 1e-8)
  expect_equal(r$backup, "pi")
  # evaluating a policy gives smaller values than acting optimally
  expect_true(all(unlist(r$q)[KS] <= q_star()[KS] + 1e-8))
  # a policy that always takes R is the optimal one here, so it recovers Q*
  polR <- list("A\rL" = 0, "A\rR" = 1, "B\rL" = 0, "B\rR" = 1)
  r2 <- offlrl(make_data(), states = SS, actions = AA, alpha = 0, gamma = GAM,
               backup = "pi", policy = polR, lr = 0.5, iters = 300000,
               tol = 1e-14)
  expect_equal(as.numeric(unlist(r2$q)[KS]), as.numeric(q_star()[KS]),
               tolerance = 1e-8)
  expect_equal(as.numeric(unlist(r2$q)[KS]), q_pi(polR), tolerance = 1e-8)
})

test_that("the penalty lower-bounds Q and grows the conservatism gap", {
  qs <- q_star()
  prev <- NULL
  pens <- numeric(0)
  for (al in c(0, 0.05, 0.2, 1, 5)) {
    r <- offlrl(make_data(), states = SS, actions = AA, alpha = al,
                gamma = GAM, lr = 0.5, iters = 100000, tol = 1e-14)
    g <- unlist(r$q)[KS]
    # Theorem 3.1: the learned values never exceed the true ones
    expect_true(all(g <= qs[KS] + 1e-8))
    # CQL(H) pushes down monotonically in alpha
    if (!is.null(prev)) expect_true(all(g <= prev + 1e-8))
    prev <- g
    # the CQL(H) penalty is non-negative, since logsumexp dominates any
    # average under the behaviour policy
    expect_true(r$penalty >= 0)
    pens <- c(pens, r$penalty)
    expect_equal(r$objective, al * r$penalty + r$bellman_error)
    # and the penalty is what recomputing it from the returned Q gives
    n_s <- table(vapply(make_data(), function(x) x[[1]], character(1)))
    want <- 0
    for (s in SS) {
      qsv <- vapply(AA, function(b) r$q[[paste0(s, "\r", b)]], numeric(1))
      bsum <- sum(vapply(AA, function(b) r$behavior[[paste0(s, "\r", b)]] *
                           r$q[[paste0(s, "\r", b)]], numeric(1)))
      want <- want + (n_s[[s]] / 75) * (.offlrl_logsumexp(qsv) - bsum)
    }
    expect_equal(r$penalty, want)
  }
  # a bigger alpha buys a bigger Bellman residual: the two terms trade off
  expect_true(pens[1] >= pens[length(pens)])
})

test_that("terminal transitions stop the backup", {
  # (A,R) ends the episode, so its value is the immediate reward alone
  d <- list(list("A", "R", 5, "B", TRUE), list("A", "L", 0, "A", FALSE),
            list("B", "L", 1, "A", FALSE), list("B", "R", 0, "B", FALSE))
  r <- offlrl(d, states = SS, actions = AA, alpha = 0, gamma = GAM,
              lr = 0.5, iters = 300000, tol = 1e-14)
  expect_equal(r$q[["A\rR"]], 5, tolerance = 1e-8)
  # and the rest of the system still satisfies its own backups
  expect_equal(r$q[["B\rL"]], 1 + GAM * max(r$q[["A\rL"]], r$q[["A\rR"]]),
               tolerance = 1e-8)
  expect_equal(r$bellman_error, 0, tolerance = 1e-12)
  # without the terminal flag the same transition bootstraps instead
  d2 <- d
  d2[[1]] <- list("A", "R", 5, "B", FALSE)
  r2 <- offlrl(d2, states = SS, actions = AA, alpha = 0, gamma = GAM,
               lr = 0.5, iters = 300000, tol = 1e-14)
  expect_true(r2$q[["A\rR"]] > r$q[["A\rR"]])
  # a four-element transition is treated as non-terminal
  d3 <- lapply(d2, function(x) x[1:4])
  r3 <- offlrl(d3, states = SS, actions = AA, alpha = 0, gamma = GAM,
               lr = 0.5, iters = 300000, tol = 1e-14)
  expect_equal(unlist(r3$q)[KS], unlist(r2$q)[KS], tolerance = 1e-8)
})

test_that("the behaviour policy is the empirical action frequency", {
  r <- offlrl(make_data(), states = SS, actions = AA, alpha = 0, gamma = GAM,
              iters = 10)
  expect_equal(r$behavior[["A\rL"]], 10 / 40)
  expect_equal(r$behavior[["A\rR"]], 30 / 40)
  expect_equal(r$behavior[["B\rL"]], 10 / 35)
  expect_equal(r$behavior[["B\rR"]], 25 / 35)
  # each state's behaviour distribution sums to one
  for (s in SS) {
    expect_equal(sum(vapply(AA, function(b) r$behavior[[paste0(s, "\r", b)]],
                            numeric(1))), 1)
  }
  expect_equal(r$counts[["A\rR"]], 30L)
  expect_equal(r$counts[["B\rR"]], 25L)
})

test_that("the mu variant applies the Eq. 2 push-down/push-up asymmetry", {
  # The general CQL penalty is E_mu[Q] - E_pi_beta[Q]. Its sign, and so the
  # direction Q moves, is decided by how mu compares with the behaviour
  # policy, and the Theorem 3.1 lower bound only follows when mu favours the
  # high-value actions at least as much as the data does.
  qs <- q_star()
  beh <- list("A\rL" = 10 / 40, "A\rR" = 30 / 40,
              "B\rL" = 10 / 35, "B\rR" = 25 / 35)
  good <- list("A\rL" = 0, "A\rR" = 1, "B\rL" = 0, "B\rR" = 1)
  bad <- list("A\rL" = 1, "A\rR" = 0, "B\rL" = 1, "B\rR" = 0)
  base <- offlrl(make_data(), states = SS, actions = AA, alpha = 0,
                 gamma = GAM, lr = 0.5, iters = 200000, tol = 1e-14)

  # mu equal to the behaviour policy makes the penalty vanish identically, so
  # alpha cannot move the solution at all
  for (al in c(0.5, 1, 20)) {
    r <- offlrl(make_data(), states = SS, actions = AA, alpha = al,
                gamma = GAM, variant = "mu", mu = beh, lr = 0.5,
                iters = 200000, tol = 1e-14)
    expect_equal(r$penalty, 0)
    expect_equal(unlist(r$q)[KS], unlist(base$q)[KS], tolerance = 1e-8)
    expect_equal(r$variant, "mu")
  }

  # mu on the better action gives a positive penalty and a conservative Q
  rg <- offlrl(make_data(), states = SS, actions = AA, alpha = 1, gamma = GAM,
               variant = "mu", mu = good, lr = 0.5, iters = 200000,
               tol = 1e-14)
  expect_true(rg$penalty > 0)
  expect_true(all(unlist(rg$q)[KS] <= qs[KS] + 1e-8))
  expect_true(all(unlist(rg$q)[KS] < unlist(base$q)[KS]))

  # mu on the worse action reverses the asymmetry: the penalty is negative and
  # the values are pushed above the truth, which is what makes the choice of
  # mu the whole substance of the method
  rb <- offlrl(make_data(), states = SS, actions = AA, alpha = 1, gamma = GAM,
               variant = "mu", mu = bad, lr = 0.5, iters = 200000,
               tol = 1e-14)
  expect_true(rb$penalty < 0)
  expect_true(all(unlist(rb$q)[KS] > qs[KS]))
  expect_equal(rb$objective, rb$penalty + rb$bellman_error)
  # the penalty is reproducible from the returned Q and behaviour policy
  n_s <- c(A = 40, B = 35)
  want <- 0
  for (s in SS) {
    first <- sum(vapply(AA, function(b) bad[[paste0(s, "\r", b)]] *
                          rb$q[[paste0(s, "\r", b)]], numeric(1)))
    bsum <- sum(vapply(AA, function(b) rb$behavior[[paste0(s, "\r", b)]] *
                         rb$q[[paste0(s, "\r", b)]], numeric(1)))
    want <- want + (n_s[[s]] / 75) * (first - bsum)
  }
  expect_equal(rb$penalty, want)
})

test_that("the rho variant reduces to CQL(H) and to mu at its extremes", {
  unif <- list("A\rL" = 0.5, "A\rR" = 0.5, "B\rL" = 0.5, "B\rR" = 0.5)
  bad <- list("A\rL" = 1, "A\rR" = 0, "B\rL" = 1, "B\rR" = 0)
  # with a uniform rho the pi-weighted soft maximum is the plain softmax, so
  # the rho variant must agree with the closed-form CQL(H) route exactly
  ru <- offlrl(make_data(), states = SS, actions = AA, alpha = 0.5,
               gamma = GAM, variant = "rho", policy = unif, lr = 0.5,
               iters = 200000, tol = 1e-14)
  rh <- offlrl(make_data(), states = SS, actions = AA, alpha = 0.5,
               gamma = GAM, variant = "H", lr = 0.5, iters = 200000,
               tol = 1e-14)
  expect_equal(unlist(ru$q)[KS], unlist(rh$q)[KS], tolerance = 1e-8)
  # the two reported penalties differ only by the normalising constant that
  # Eq. 4 drops: rho keeps log(1/|A|), the closed-form H route does not. A
  # constant cannot change the gradient, which is why the values above match.
  expect_equal(ru$penalty, rh$penalty + log(1 / length(AA)))
  # with rho degenerate on one action the soft maximum collapses onto that
  # action's value, which is what the mu variant computes directly
  rr <- offlrl(make_data(), states = SS, actions = AA, alpha = 1, gamma = GAM,
               variant = "rho", policy = bad, lr = 0.5, iters = 200000,
               tol = 1e-14)
  rm <- offlrl(make_data(), states = SS, actions = AA, alpha = 1, gamma = GAM,
               variant = "mu", mu = bad, lr = 0.5, iters = 200000,
               tol = 1e-14)
  expect_equal(unlist(rr$q)[KS], unlist(rm$q)[KS], tolerance = 1e-8)
  expect_equal(rr$penalty, rm$penalty, tolerance = 1e-8)
  expect_match(rr$method, "eq. 4")
  expect_match(rm$method, "eq. 2")
})

test_that("labels need not be character", {
  # numeric labels index name-keyed tallies and must not be taken positionally
  d <- list(list(1, 10, 0, 1), list(1, 20, 1, 2),
            list(2, 10, 0, 1), list(2, 20, 2, 2))
  r <- offlrl(d, alpha = 0, gamma = GAM, lr = 0.5, iters = 300000,
              tol = 1e-14)
  # the same MDP as above with A=1, B=2 and L=10, R=20
  expect_equal(r$q[["1\r10"]], 17.1, tolerance = 1e-7)
  expect_equal(r$q[["1\r20"]], 19, tolerance = 1e-7)
  expect_equal(r$q[["2\r20"]], 20, tolerance = 1e-7)
  expect_equal(r$n_transitions, 4L)
  # each state was seen once per action, so the behaviour policy is uniform
  expect_equal(r$behavior[["1\r10"]], 0.5)
  # explicit numeric level sets agree with the inferred ones
  r2 <- offlrl(d, states = c(1, 2), actions = c(10, 20), alpha = 0,
               gamma = GAM, lr = 0.5, iters = 300000, tol = 1e-14)
  expect_equal(unlist(r2$q), unlist(r$q), tolerance = 1e-8)
})

test_that("unobserved state-action pairs are pushed down, not left alone", {
  # B,L never appears in the data, so conservatism must not credit it
  d <- list(list("A", "L", 0, "A"), list("A", "R", 1, "B"),
            list("B", "R", 2, "B"))
  r <- offlrl(d, states = SS, actions = AA, alpha = 2, gamma = GAM,
              lr = 0.5, iters = 100000, tol = 1e-14)
  expect_equal(r$behavior[["B\rL"]], 0)
  # its value sits below the best in-data action at the same state
  expect_true(r$q[["B\rL"]] < r$q[["B\rR"]])
  expect_equal(r$greedy[["B"]], "R")
  # with no penalty at all the same pair is simply never updated
  r0 <- offlrl(d, states = SS, actions = AA, alpha = 0, gamma = GAM,
               lr = 0.5, iters = 1000)
  expect_equal(r0$q[["B\rL"]], 0)
})

test_that("offlrl validates its arguments", {
  d <- make_data()
  expect_error(offlrl(d, variant = "Z"), "variant must be one of")
  expect_error(offlrl(d, backup = "Z"), "backup must be")
  expect_error(offlrl(d, alpha = -1), "alpha must be >= 0")
  expect_error(offlrl(list()), "must be non-empty")
  expect_error(offlrl(list(list("A", "L", 0))), "each transition must be")
  expect_error(offlrl(d, variant = "mu"), "needs mu")
  expect_error(offlrl(d, backup = "pi"), "needs policy")
  expect_error(offlrl(d, variant = "rho"), "pi\\^\\{k-1\\}")
  expect_error(offlrl(d, states = character(0), actions = AA),
               "must be non-empty")
})

test_that("the aliases and cheatsheet are intact", {
  d <- make_data()
  a <- offlrl(d, states = SS, actions = AA, alpha = 0.3, iters = 500)
  for (f in list(offline_rl_cql, offlinerlcql, conservative_q_learning)) {
    expect_equal(f(d, states = SS, actions = AA, alpha = 0.3, iters = 500), a)
  }
  expect_type(.offlrl_cheatsheet(), "character")
  expect_match(.offlrl_cheatsheet(), "CQL")
})
