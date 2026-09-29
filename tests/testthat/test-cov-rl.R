# Coverage for bandit / RL / adaptive-testing files (exp3.R,
# exp3_native.R, epsg.R, explor_native.R, acrt.R, ddqn.R, ddqn_native.R,
# bcq_native.R, catnxt.R, catstop_native.R): the learners are replayed
# step by step on the SplitMix64 / van der Corput streams they document,
# and item information is recomputed from the 4PL model.

vdc_b <- function(i, b) {
  f <- 1
  r <- 0
  k <- i + 1
  while (k > 0) {
    f <- f / b
    r <- r + f * (k %% b)
    k <- k %/% b
  }
  r
}

test_that("Exp3 and morie_exp3 replay the exponential-weight updates", {
  x <- rbind(c(0.2, 0.9, 0.1), c(0.5, 0.3, 0.7), c(0.1, 1, 0.4), c(0.6, 0.2, 0.8),
             c(0.3, 0.7, 0.2))
  r <- Exp3(x, gamma_ = 0.3, seed = 2)
  e <- .ghc_rng(2)
  w <- rep(1, 3)
  acts <- numeric(5)
  for (t in 1:5) {
    p <- 0.7 * w / sum(w) + 0.1
    i <- which(.ghc_unif(e, 1L) <= cumsum(p))[1]
    if (is.na(i)) i <- 3
    w[i] <- w[i] * exp(0.3 * x[t, i] / p[i] / 3)
    acts[t] <- i - 1
  }
  expect_equal(r$actions, acts)
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_equal(r$total_reward, sum(x[cbind(1:5, acts + 1)]), tolerance = 1e-12)
  n <- morie_exp3(x, gamma_ = 0.3, seed = 2)
  expect_equal(n$weights, r$weights, tolerance = 1e-12)
  expect_equal(Exp3(x, 0.3, T = 3, seed = 2)$actions, acts[1:3])
  expect_same_function(exp3_bandit, morie_exp3)
  expect_error(Exp3(x, 0.3, T = 9), "only 5 rows")
  expect_error(Exp3(x, 0), "\\(0, 1\\]")
  expect_error(morie_exp3(x, 0.3, T = 9), "only 5 rows")
  expect_error(morie_exp3(x, 1.5), "\\(0, 1\\]")
})

test_that("Epsgreedy uses van der Corput draws for exploration", {
  mu <- c(0.2, 0.5, 0.35)
  r <- Epsgreedy(mu, epsilon = 0.3, T = 12)
  q <- numeric(3)
  cnt <- numeric(3)
  tot <- 0
  for (t in 0:11) {
    if (vdc_b(t, 2) < 0.3) {
      a <- min(floor(vdc_b(t, 3) * 3) + 1, 3)
    } else {
      a <- which.max(q)
    }
    cnt[a] <- cnt[a] + 1
    q[a] <- q[a] + (mu[a] - q[a]) / cnt[a]
    tot <- tot + mu[a]
  }
  expect_equal(r$counts, cnt)
  expect_equal(r$q, q, tolerance = 1e-12)
  expect_equal(r$regret, 12 * 0.5 - tot, tolerance = 1e-12)
  expect_equal(r$p_greedy, 0.7 + 0.1, tolerance = 1e-12)
  rw <- matrix(c(1, 0, 0.5), 12, 3, byrow = TRUE)
  expect_equal(Epsgreedy(mu, 0, T = 12, rewards = rw)$total_reward, 12)
})

test_that("explor (ICM, identity features) replays the inverse/forward SGD", {
  S <- rbind(c(0.1, 0.5), c(0.4, -0.2), c(-0.3, 0.8), c(0.6, 0.1))
  S1 <- rbind(S[-1, ], c(0.2, 0.2))
  A <- c(0, 1, 1, 0)
  r <- explor(S, A, S1, n_actions = 2, features = "identity", eta = 2, beta = 0.3, lr = 0.1)
  Winv <- matrix(0, 4, 2)
  Wf <- matrix(0, 4, 2)
  rew <- numeric(4)
  li <- lf <- 0
  for (t in 1:4) {
    av <- c(0, 0)
    av[A[t] + 1] <- 1
    xi <- c(S[t, ], S1[t, ])
    z <- as.numeric(xi %*% Winv)
    pr <- exp(z - max(z)) / sum(exp(z - max(z)))
    li <- li - log(pr[A[t] + 1])
    xf <- c(S[t, ], av)
    ef <- as.numeric(xf %*% Wf) - S1[t, ]
    lf <- lf + 0.5 * sum(ef^2)
    rew[t] <- 2 * 0.5 * sum(ef^2)
    Winv <- Winv - 0.1 * 0.7 * outer(xi, pr - av)
    Wf <- Wf - 0.1 * 0.3 * outer(xf, ef)
  }
  expect_equal(r$intrinsic_reward, rew, tolerance = 1e-12)
  expect_equal(r$inverse_loss, li / 4, tolerance = 1e-12)
  expect_equal(r$objective, (0.7 * li + 0.3 * lf) / 4, tolerance = 1e-12)
  expect_equal(morie_explor(S, A, S1, n_actions = 2, features = "identity", eta = 2, beta = 0.3,
                            lr = 0.1)$intrinsic_reward, rew, tolerance = 1e-12)
  inv <- explor(S, A, S1, n_features = 3, seed = 1)
  expect_length(inv$phi[[1]], 3L)
  expect_length(inv$intrinsic_reward, 4L)
  ct <- explor(S, S[, 1, drop = FALSE], S1, features = "identity", discrete = FALSE)
  expect_equal(ct$intrinsic_reward[1], 0.5 * sum(S1[1, ]^2), tolerance = 1e-12)
  expect_error(explor(S, A, S1, features = "x"), "features must be")
  expect_error(explor(S, A, S1, eta = 0), "eta must be")
  expect_error(explor(S, A, S1, beta = 2), "beta must lie")
  expect_error(explor(S, A, S1[-1, ]), "same length")
  expect_error(explor(S, A, S1[, 1, drop = FALSE]), "same width")
  expect_error(explor(S, A[-1], S1), "actions for")
  expect_error(explor(S, rep(0, 4), S1), "at least 2")
  expect_error(explor(S, c(0, 1, 5, 0), S1, n_actions = 2), "out of range")
  expect_error(explor(S, A, S1, n_features = 0), ">= 1")
  expect_error(explor(matrix(numeric(0), 0, 2), A, S1), "non-empty")
})

test_that("Actorcrit applies the one-step TD actor-critic updates", {
  R <- c(1, 0, 2, -1)
  V <- c(0.5, 0.3, 0.8, 0.1)
  G <- rbind(c(1, 0), c(0.5, -0.5), c(0, 1), c(-1, 0.2))
  r <- Actorcrit(R, values = V, grad_logpi = G, alpha_theta = 0.2, alpha_w = 0.3, gamma = 0.9)
  d <- R + 0.9 * c(V[-1], 0) - V
  expect_equal(r$deltas, d, tolerance = 1e-12)
  expect_equal(r$theta, as.numeric(0.2 * colSums(0.9^(0:3) * d * G)), tolerance = 1e-12)
  expect_equal(r$w, 0.3 * sum(d), tolerance = 1e-12)
  nd <- Actorcrit(R, values = V, grad_logpi = G, alpha_theta = 0.2, gamma = 0.9,
                  discount_actor = FALSE)
  expect_equal(nd$theta, as.numeric(0.2 * colSums(d * G)), tolerance = 1e-12)
})

test_that("Ddqn and morie_ddqn replay tabular double Q-learning", {
  P <- list(rbind(c(0.2, 0.8, 0), c(0, 0.3, 0.7), c(0, 0, 1)),
            rbind(c(0.9, 0.1, 0), c(0.5, 0, 0.5), c(0, 0, 1)))
  R <- rbind(c(0, 1), c(2, 0.5), c(0, 0))
  r <- Ddqn(P, R, gamma = 0.9, alpha = 0.5, epsilon = 0.2, n_episodes = 3, terminal = 2,
            max_steps = 20, seed = 4)
  e <- .ghc_rng(4)
  Q1 <- Q2 <- matrix(0, 3, 2)
  gr <- function(q) which(q == max(q))[1]
  for (ep in 1:3) {
    s <- 1
    for (k in 1:20) {
      if (s == 3) break
      if (.ghc_unif(e, 1L) < 0.2) a <- min(floor(.ghc_unif(e, 1L) * 2), 1) + 1 else a <- gr(Q1[s, ] + Q2[s, ])
      s2 <- which(.ghc_unif(e, 1L) <= cumsum(P[[a]][s, ]))[1]
      if (.ghc_unif(e, 1L) < 0.5) {
        nx <- if (s2 == 3) 0 else Q2[s2, gr(Q1[s2, ])]
        Q1[s, a] <- Q1[s, a] + 0.5 * (R[s, a] + 0.9 * nx - Q1[s, a])
      } else {
        nx <- if (s2 == 3) 0 else Q1[s2, gr(Q2[s2, ])]
        Q2[s, a] <- Q2[s, a] + 0.5 * (R[s, a] + 0.9 * nx - Q2[s, a])
      }
      s <- s2
    }
  }
  expect_equal(r$q1, Q1, tolerance = 1e-12)
  expect_equal(r$q2, Q2, tolerance = 1e-12)
  expect_equal(r$policy, apply((Q1 + Q2) / 2, 1, gr) - 1)
  n <- morie_ddqn(P, R, gamma = 0.9, alpha = 0.5, epsilon = 0.2, n_episodes = 3, terminal = 2,
                  max_steps = 20, seed = 4)
  expect_equal(n$estimate, r$estimate, tolerance = 1e-12)
  expect_error(Ddqn(P, R, 0.9, start = 5), "start out of range")
  expect_error(morie_ddqn(P, R, 0.9, start = -1), "start out of range")
})

test_that("morie_bcq restricts the Bellman backup to behaviour-supported actions", {
  D <- list(list(0, 0, 1, 1), list(0, 0, 1, 1), list(0, 1, 5, 2, TRUE), list(1, 0, 0, 2),
            list(1, 1, 2, 2), list(1, 1, 2, 2), list(1, 1, 2, 2), list(2, 0, 0, 2, TRUE))
  r <- morie_bcq(D, tau = 0.4, gamma = 0.9, lr = 1, loss = "squared", iters = 200)
  # G(a|s): s0 -> (2/3, 1/3), s1 -> (1/4, 3/4), s2 -> (1, 0); tau = 0.4 keeps a/max > 0.4
  expect_equal(unname(unlist(r$allowed[c("0", "1", "2")])), c(0, 1, 1, 0))
  q20 <- 0
  q11 <- 2 + 0.9 * q20
  q10 <- 0 + 0.9 * q20
  q00 <- 1 + 0.9 * q11
  q01 <- 5
  expect_equal(r$q[["1|1"]], q11, tolerance = 1e-12)
  expect_equal(r$q[["0|0"]], q00, tolerance = 1e-12)
  expect_equal(r$q[["0|1"]], q01, tolerance = 1e-12)
  expect_equal(r$policy[["0"]], 1)
  expect_equal(r$n_eliminated, 2L)
  expect_equal(r$bellman_error, 0, tolerance = 1e-12)
  expect_error(morie_bcq(D, tau = 2), "tau must lie")
  expect_error(morie_bcq(D, loss = "x"), "loss must be")
  expect_error(morie_bcq(D, huber_c = 0), "huber_c")
  expect_error(morie_bcq(list(list(1, 2))), "each transition")
  expect_error(morie_bcq(list()), "non-empty")
})

test_that("Catnext and morie_catstop use 4PL Fisher information", {
  it <- rbind(c(1.2, -0.5, 0.1, 0.95), c(0.8, 0.3, 0.2, 1), c(1.8, 0.1, 0, 0.9), c(1, 1.5, 0.15, 1))
  info4 <- function(th, a, b, c, d) {
    e <- exp(a * (th - b))
    p <- c + (d - c) * e / (1 + e)
    dp <- a * e * (d - c) / (1 + e)^2
    dp^2 / (p * (1 - p))
  }
  inf <- info4(0.2, it[, 1], it[, 2], it[, 3], it[, 4])
  r <- Catnext(it, 0.2, administered = 3, exposure = c(1, 1, 1, 0.5))
  expect_equal(r$information, inf, tolerance = 1e-12)
  wt <- inf * c(1, 1, 1, 0.5)
  expect_equal(r$next_item, c(1, 2, 4)[which.max(wt[c(1, 2, 4)])])
  expect_equal(Catnext(it, 0.2, D = 1.7)$information, info4(0.2, 1.7 * it[, 1], it[, 2], it[, 3], it[, 4]),
               tolerance = 1e-12)
  expect_error(Catnext(it[0, ], 0), "non-empty")
  expect_error(Catnext(it[, 1:3], 0), "\\(a, b, c, d\\)")
  expect_error(Catnext(it, 0, administered = 9), "1..J")
  expect_error(Catnext(it, 0, exposure = 1), "one entry per item")
  expect_error(Catnext(it, 0, exposure = c(2, 1, 1, 1)), "\\[0, 1\\]")
  expect_error(Catnext(it, 0, administered = 1:4), "every item")
  s <- morie_catstop(it, 0.2, se_target = 0.9)
  expect_equal(s$information, sum(inf), tolerance = 1e-12)
  expect_equal(s$se, 1 / sqrt(sum(inf)), tolerance = 1e-12)
  expect_equal(s$stop, 1 / sqrt(sum(inf)) <= 0.9)
  bm <- morie_catstop(it, 0.2, 0.5, estimator = "BM", prior_var = 2)
  expect_equal(bm$se, 1 / sqrt(0.5 + sum(inf)), tolerance = 1e-12)
  expect_error(morie_catstop(it, 0, 0), "positive")
  expect_error(morie_catstop(it, 0, 1, estimator = "EAP"), "'ML' or 'BM'")
  expect_error(morie_catstop(rbind(c(1, 0, 0.5, 0.4)), 0, 1), "0 <= c < d <= 1")
  expect_error(morie_catstop(it, 0, 1, estimator = "BM", prior_var = 0), "prior_var")
})
