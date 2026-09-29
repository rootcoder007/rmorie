# Coverage for AIRL (Fu, Luo & Levine 2018): the discriminator
# D = sigmoid(f - log pi) with f = g + gamma h(s') - h(s), trained by
# full-batch gradient ascent on the expert/policy log-likelihood
# (regenerated here with plain vectors), the recovered reward
# log D - log(1 - D), and soft value iteration (log-sum-exp Bellman
# backups and the Boltzmann policy). Both the morie_ entry points and the
# short-name copies are exercised.

.traj <- function() {
  list(es = c(0, 1, 2, 0, 1), ea = c(1, 1, 0, 1, 1), en = c(1, 2, 2, 1, 2), el = log(c(0.8, 0.8, 0.6, 0.8, 0.8)),
       ps = c(0, 1, 0, 2, 1), pa = c(0, 0, 1, 0, 1), pn = c(0, 1, 1, 2, 2), pl = log(c(0.5, 0.5, 0.5, 0.5, 0.5)))
}

.ref <- function(d, gamma, lr, epochs, state_only) {
  S <- sort(unique(c(d$es, d$en, d$ps, d$pn)))
  key <- function(s, a) if (state_only) match(s, S) else match(paste(s, a), sort(unique(c(paste(d$es, d$ea), paste(d$ps, d$pa)))))
  ng <- if (state_only) length(S) else length(unique(c(paste(d$es, d$ea), paste(d$ps, d$pa))))
  g <- numeric(ng)
  h <- numeric(length(S))
  f <- function(s, a, s1) g[key(s, a)] + gamma * h[match(s1, S)] - h[match(s, S)]
  D <- function(s, a, s1, lp) stats::plogis(f(s, a, s1) - lp)
  for (it in seq_len(epochs)) {
    dg <- numeric(ng)
    dh <- numeric(length(S))
    for (k in seq_along(d$es)) {
      c <- (1 - D(d$es[k], d$ea[k], d$en[k], d$el[k])) / length(d$es)
      i <- key(d$es[k], d$ea[k])
      dg[i] <- dg[i] + c
      dh[match(d$en[k], S)] <- dh[match(d$en[k], S)] + gamma * c
      dh[match(d$es[k], S)] <- dh[match(d$es[k], S)] - c
    }
    for (k in seq_along(d$ps)) {
      c <- -D(d$ps[k], d$pa[k], d$pn[k], d$pl[k]) / length(d$ps)
      i <- key(d$ps[k], d$pa[k])
      dg[i] <- dg[i] + c
      dh[match(d$pn[k], S)] <- dh[match(d$pn[k], S)] + gamma * c
      dh[match(d$ps[k], S)] <- dh[match(d$ps[k], S)] - c
    }
    g <- g + lr * dg
    h <- h + lr * dh
  }
  dp <- vapply(seq_along(d$ps), function(k) D(d$ps[k], d$pa[k], d$pn[k], d$pl[k]), 1)
  de <- vapply(seq_along(d$es), function(k) D(d$es[k], d$ea[k], d$en[k], d$el[k]), 1)
  list(dp = dp, de = de, h = h, reward = log(dp) - log(1 - dp))
}

test_that("the AIRL discriminator is trained by full-batch ascent", {
  d <- .traj()
  for (so in c(TRUE, FALSE)) {
    r <- morie_airl(d$es, d$ea, d$en, d$el, d$ps, d$pa, d$pn, d$pl, gamma = 0.9, lr = 0.5, epochs = 40, state_only = so)
    ref <- .ref(d, 0.9, 0.5, 40, so)
    expect_equal(r$D_policy, ref$dp, tolerance = 1e-10)
    expect_equal(r$D_expert, ref$de, tolerance = 1e-10)
    expect_equal(r$reward, ref$reward, tolerance = 1e-10)
    expect_equal(unname(r$h), ref$h, tolerance = 1e-10)
    expect_equal(r$log_likelihood, mean(log(ref$de)) + mean(log(1 - ref$dp)), tolerance = 1e-10)
    expect_equal(r$accuracy, (sum(ref$de > 0.5) + sum(ref$dp <= 0.5)) / 10)
    a <- airl(d$es, d$ea, d$en, d$el, d$ps, d$pa, d$pn, d$pl, gamma = 0.9, lr = 0.5, epochs = 40, state_only = so)
    expect_equal(a$D_policy, ref$dp, tolerance = 1e-10)
    expect_equal(a$reward, ref$reward, tolerance = 1e-10)
  }
  expect_error(morie_airl(d$es, d$ea[-1], d$en, d$el, d$ps, d$pa, d$pn, d$pl), "same length")
  expect_error(airl(d$es, d$ea[-1], d$en, d$el, d$ps, d$pa, d$pn, d$pl), "same length")
})

test_that("vector-valued states are keyed consistently", {
  es <- list(c(0, 1), c(1, 1))
  en <- list(c(1, 1), c(1, 2))
  ps <- list(c(0, 1), c(0, 0))
  pn <- list(c(0, 0), c(1, 1))
  r <- morie_airl(es, c(1, 1), en, log(c(0.7, 0.7)), ps, c(0, 0), pn, log(c(0.5, 0.5)), epochs = 5)
  a <- airl(es, c(1, 1), en, log(c(0.7, 0.7)), ps, c(0, 0), pn, log(c(0.5, 0.5)), epochs = 5)
  expect_equal(a$D_policy, r$D_policy, tolerance = 1e-10)
  expect_length(r$h, 4L)
})

test_that("soft value iteration solves V(s) = logsumexp_a(r(s) + gamma V(s'))", {
  step <- function(s, a) min(max(s + a, 0), 3)
  rew <- function(s) if (s == 3) 1 else 0
  v <- morie_soft_value_iteration(0:3, c(-1, 1), step, rew, gamma = 0.8)
  V <- unname(v$V)
  bell <- vapply(0:3, function(s) {
    q <- rew(s) + 0.8 * V[c(step(s, -1), step(s, 1)) + 1]
    log(sum(exp(q)))
  }, 1)
  expect_equal(V, bell, tolerance = 1e-12)
  pi <- unname(v$pi)
  expect_equal(pi[1] + pi[2], 1, tolerance = 1e-12)
  expect_equal(pi[2], exp(rew(0) + 0.8 * V[2] - V[1]), tolerance = 1e-12)
  s <- soft_value_iteration(0:3, c(-1, 1), step, rew, gamma = 0.8)
  expect_equal(unname(unlist(s$V)), V, tolerance = 1e-12)
  expect_equal(unname(unlist(s$pi)), pi, tolerance = 1e-12)
})
