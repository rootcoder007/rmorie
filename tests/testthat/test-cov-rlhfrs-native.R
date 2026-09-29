# Session-MDP recommendation: ranking metrics (Jarvelin & Kekalainen
# 2002), discounted rollouts, and IPS / SNIPS / doubly robust off-policy
# values (Horvitz & Thompson 1952; Swaminathan & Joachims 2015; Dudik et
# al. 2011), recomputed in vector form.

rl_env <- list(
  transition = list(list(c(0, 1), c(1, 0)), list(c(1, 0), c(0, 1))),
  reward = list(c(1, 0), c(0.5, 2))
)
rl_log <- list(c(0, 0, 1), c(0, 1, 0), c(1, 1, 2), c(1, 0, 0.5), c(0, 0, 0.8))
rl_beh <- c(0.5, 0.5, 0.4, 0.6, 0.5)
rl_pol <- rbind(c(0.8, 0.2), c(0.3, 0.7))

test_that("precision, nDCG and MRR at k", {
  rel <- c(0, 3, 1, 0, 2)
  expect_equal(morie_rlhfRS_precision(rel, 3), 2 / 3)
  expect_equal(morie_rlhfRS_precision(rel, 8), 3 / 8)
  dcg <- function(v, k) sum(v[1:k] / log2(1:k + 1))
  expect_equal(morie_rlhfRS_ndcg(rel, 4), dcg(rel, 4) / dcg(sort(rel, TRUE), 4), tolerance = 1e-15)
  expect_identical(morie_rlhfRS_ndcg(c(0, 0), 2), 0)
  expect_equal(morie_rlhfRS_mrr(rel), 1 / 2)
  expect_identical(morie_rlhfRS_mrr(c(0, 0)), 0)
  expect_error(morie_rlhfRS_precision(rel, 0), "below one")
  expect_error(morie_rlhfRS_ndcg(rel, 0), "below one")
})

test_that("rollouts accumulate discounted rewards along sampled paths", {
  # deterministic policy and transitions: 0 -a0-> 1 -a1-> 1 ...
  pol <- rbind(c(1, 0), c(0, 1))
  r <- morie_rlhfRS_rollout(rl_env, pol, n_episodes = 3, horizon = 4, gamma = 0.9)
  g <- 1 + 0.9 * 2 + 0.81 * 2 + 0.729 * 2
  expect_equal(r$returns, rep(g, 3), tolerance = 1e-14)
  expect_equal(r$visits, matrix(c(3L, 0L, 0L, 9L), 2))
  expect_equal(r$se, 0)
  # stochastic policy: replay the uniforms by hand
  rs <- morie_rlhfRS_rollout(rl_env, rl_pol, n_episodes = 2, horizon = 3, gamma = 0.5, seed = 8)
  e <- .ghc_rng(8)
  ret <- numeric(2)
  for (ep in 1:2) {
    s <- 0
    d <- 1
    for (h in 1:3) {
      a <- if (.ghc_unif(e, 1L) < rl_pol[s + 1, 1]) 0 else 1
      ret[ep] <- ret[ep] + d * rl_env$reward[[s + 1]][a + 1]
      d <- d * 0.5
      .ghc_unif(e, 1L)
      s <- which(rl_env$transition[[s + 1]][[a + 1]] == 1) - 1
    }
  }
  expect_equal(rs$returns, ret, tolerance = 1e-15)
  expect_equal(rs$se, stats::sd(ret) / sqrt(2), tolerance = 1e-14)
  expect_error(morie_rlhfRS_rollout(rl_env, rl_pol[, 1, drop = FALSE]), "distribution")
})

test_that("IPS, SNIPS and doubly robust off-policy values", {
  s <- vapply(rl_log, `[`, 1, 1)
  a <- vapply(rl_log, `[`, 1, 2)
  r <- vapply(rl_log, `[`, 1, 3)
  w <- rl_pol[cbind(s + 1, a + 1)] / rl_beh
  ips <- morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh)
  expect_equal(ips$weights, w, tolerance = 1e-15)
  expect_equal(ips$estimate, mean(w * r), tolerance = 1e-15)
  expect_equal(ips$ess, sum(w)^2 / sum(w^2), tolerance = 1e-14)
  sn <- morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, "snips")
  expect_equal(sn$estimate, sum(w * r) / sum(w), tolerance = 1e-15)
  Q <- rbind(c(0.9, 0.1), c(0.4, 1.8))
  dr <- morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, "dr", reward_model = Q)
  base <- rowSums(rl_pol * Q)[s + 1]
  expect_equal(dr$estimate, mean(base + w * (r - Q[cbind(s + 1, a + 1)])), tolerance = 1e-15)
  cl <- morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, clip = 1.5)
  expect_equal(cl$weights, pmin(w, 1.5), tolerance = 1e-15)
  expect_identical(cl$n_clipped, sum(w > 1.5))
  expect_error(morie_rlhfRS_offpolicy(rl_log, rl_pol, c(0, rl_beh[-1])), "never could")
  expect_error(morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, "dm"), "ips, snips or dr")
  expect_error(morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, "dr"), "reward model")
  expect_error(morie_rlhfRS_offpolicy(list(), rl_pol, numeric(0)), "needs a log")
  expect_error(morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh[-1]), "one propensity")
})

test_that("morie_rlhfRS combines rollout, off-policy value and ranking metrics", {
  env <- c(rl_env, list(log = rl_log, behaviour = rl_beh))
  r <- morie_rlhfRS(env, rl_pol, n_episodes = 5, horizon = 3, estimator = "snips",
                    relevance = c(1, 0, 2), k = 2)
  ro <- morie_rlhfRS_rollout(env, rl_pol, 5, 3)
  expect_equal(r$value, ro$mean, tolerance = 1e-15)
  expect_equal(r$ci_lower, ro$mean - stats::qnorm(0.975) * ro$se, tolerance = 1e-12)
  expect_equal(r$off_policy, morie_rlhfRS_offpolicy(rl_log, rl_pol, rl_beh, "snips")$estimate)
  expect_equal(r$precision_at_k, 0.5)
  expect_equal(r$mrr, 1)
  lo <- morie_rlhfRS(list(log = rl_log, behaviour = rl_beh), rl_pol)
  expect_false(lo$has_simulator)
  expect_null(lo$value)
  expect_error(morie_rlhfRS(list(), rl_pol), "either a transition")
  expect_match(morie_rlhfRS_cheatsheet(), "doubly robust", fixed = TRUE)
})
