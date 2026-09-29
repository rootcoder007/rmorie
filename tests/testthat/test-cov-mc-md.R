# Coverage for the exhaustive MCD (covariance and regression), MCTS
# selection rules, UCT search and the two finite-MDP solvers (four
# entry points); recomputed by exhaustive enumeration in the test.

test_that("Mcdv and Mcdcv enumerate every h-subset", {
  X <- rbind(c(1, 2), c(2, 1.8), c(1.5, 2.4), c(2.2, 2.9), c(9, -3), c(1.8, 2.2), c(2.5, 2.6))
  r <- Mcdv(X)
  h <- (7 + 2 + 1) %/% 2
  dets <- vapply(combn(7, h, simplify = FALSE), function(s) det(cov(X[s, ])), 0)
  best <- combn(7, h, simplify = FALSE)[[which.min(dets)]]
  expect_equal(r$estimate, min(dets), tolerance = 1e-12)
  expect_equal(r$subset, best - 1)
  expect_equal(r$center, colMeans(X[best, ]), tolerance = 1e-12)
  c0 <- (h / 7) / pchisq(qchisq(h / 7, 2), 4)
  expect_equal(r$cov, cov(X[best, ]) * c0, tolerance = 1e-9)
  expect_error(Mcdv(X, h = 2), "h must exceed p")
  y <- c(1.1, 2.0, 1.4, 2.3, 10, 1.9, 2.4)
  x <- X[, 1]
  rc <- Mcdcv(y, x)
  Z <- cbind(x, y)
  dz <- vapply(combn(7, h, simplify = FALSE), function(s) det(cov(Z[s, ])), 0)
  bz <- combn(7, h, simplify = FALSE)[[which.min(dz)]]
  S <- cov(Z[bz, ])
  expect_equal(rc$coef, S[1, 2] / S[1, 1], tolerance = 1e-12)
  expect_equal(rc$intercept, mean(y[bz]) - S[1, 2] / S[1, 1] * mean(x[bz]), tolerance = 1e-12)
  expect_error(Mcdcv(y, x, h = 8), "cannot exceed")
})

test_that("Puctsel scores PUCT, MuZero and UCT", {
  Q <- c(0.2, 0.5, 0.1)
  N <- c(3, 10, 1)
  P <- c(0.5, 0.3, 0.2)
  r <- Puctsel(Q, N, P, c = 1.5)
  u <- 1.5 * P * sqrt(14) / (1 + N)
  expect_equal(r$scores, Q + u, tolerance = 1e-12)
  expect_equal(r$action, which.max(Q + u) - 1L)
  m <- Puctsel(Q, N, P, c = 1.25, rule = "muzero", c2 = 100)
  expect_equal(m$u, P * sqrt(14) / (1 + N) * (1.25 + log((14 + 101) / 100)), tolerance = 1e-12)
  ut <- Puctsel(Q, c(3, 0, 1), P, c = 1, rule = "uct")
  expect_equal(ut$u, c(sqrt(log(4) / 3), Inf, sqrt(log(4) / 1)), tolerance = 1e-12)
  expect_equal(ut$action, 1)
})

test_that("morie_mctsr visits the best move most on a one-step game", {
  actions <- function(s) if (s == "root") list("a", "b", "c") else list()
  step <- function(s, a) a
  reward <- function(s) c(a = 0.2, b = 1, c = 0.5)[[s]]
  term <- function(s) s != "root"
  r <- morie_mctsr("root", actions, step, reward, term, n_iter = 60, seed = 1)
  expect_equal(r$root_visits, 60)
  expect_equal(sum(r$child_visits), 60L)
  expect_equal(unname(r$child_values), c(0.2, 1, 0.5), tolerance = 1e-12)
  expect_equal(r$action, "b")
  mx <- morie_mctsr("root", actions, step, reward, term, n_iter = 5, final = "max")
  expect_equal(mx$action, "b")
  expect_error(morie_mctsr("root", actions, step, reward, term, backup = "avg"), "backup")
})

test_that("the MDP solvers reach the optimum found by enumerating policies", {
  P1 <- rbind(c(0.7, 0.3, 0), c(0.1, 0.8, 0.1), c(0, 0.2, 0.8))
  P2 <- rbind(c(0.2, 0.8, 0), c(0, 0.3, 0.7), c(0.5, 0, 0.5))
  R <- rbind(c(1, 0), c(0, 2), c(0.5, 3))
  g <- 0.9
  pols <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  Pl <- list(P1, P2)
  Vs <- t(apply(pols, 1, function(p) {
    Pp <- t(vapply(1:3, function(s) Pl[[p[s] + 1]][s, ], numeric(3)))
    Rp <- R[cbind(1:3, p + 1)]
    as.numeric(solve(diag(3) - g * Pp, Rp))
  }))
  best <- which.max(rowSums(Vs))
  Vstar <- Vs[best, ]
  expect_true(all(sweep(Vs, 2, Vstar) <= 1e-9))
  for (res in list(Mdpval(Pl, R, g), morie_mdpval(Pl, R, g))) {
    expect_equal(res$estimate, Vstar, tolerance = 1e-8)
    expect_equal(res$policy, as.numeric(pols[best, ]))
    expect_true(res$converged)
  }
  for (res in list(Mdppol(Pl, R, g), morie_mdppol(Pl, R, g, pi0 = c(1, 0, 0)))) {
    expect_equal(res$estimate, Vstar, tolerance = 1e-9)
    expect_equal(res$policy, as.numeric(pols[best, ]))
    expect_true(res$policy_stable)
  }
  Rl <- list(matrix(1, 3, 3), matrix(2, 3, 3))
  expect_equal(Mdpval(Pl, Rl, 0.5)$q[, 2], rep(2 + 0.5 * 4, 3), tolerance = 1e-8)
  expect_error(Mdpval(list(P1 * 2), R[, 1, drop = FALSE], g), "does not sum to 1")
  expect_error(Mdppol(Pl, R, g, pi0 = c(2, 0, 0)), "out of range")
})
