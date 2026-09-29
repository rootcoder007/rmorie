# Coverage for the AlphaZero / MuZero building blocks (ag*.R, alf*.R,
# alphas.R, alpz.R): each expected value is recomputed from the paper's
# formula in base R.

rk_digest <- function(text) {
  h <- 0
  for (cp in utf8ToInt(text)) h <- (131 * h + cp) %% 2147483647
  h
}
softmax_ref <- function(z) exp(z - max(z)) / sum(exp(z - max(z)))

test_that("Mctsbackup, Mctsnode, Mctsexpand and Mctsq", {
  r <- Mctsbackup("leaf", 0.6, path = 1:3, N = c(2, 1, 0), W = c(0.5, -0.2, 0),
                  rewards = c(0.1, 0, 0.2), gamma = 0.9)
  g3 <- 0.2 + 0.9 * -0.6
  g2 <- 0 + 0.9 * -g3
  g1 <- 0.1 + 0.9 * -g2
  expect_equal(r$g, c(g1, g2, g3), tolerance = 1e-12)
  expect_equal(r$q, (c(0.5, -0.2, 0) + c(g1, g2, g3)) / c(3, 2, 1), tolerance = 1e-12)
  expect_equal(Mctsbackup("l", 1, path = 1:2, alternate = FALSE)$g, c(1, 1))
  expect_true(is.nan(Mctsbackup("l", 1, path = integer(0))$estimate))
  nd <- Mctsnode(c(2, 1, 1), action_space = 5)
  expect_equal(nd$p, c(0.5, 0.25, 0.25, 0, 0), tolerance = 1e-12)
  expect_equal(Mctsnode(c(3, 1, 1), action_space = 2)$p, c(0.75, 0.25), tolerance = 1e-12)
  expect_equal(Mctsnode(c(0, 0))$prior_sum, 0)
  ex <- Mctsexpand("s", function(s) list(c(1, 2, 3), 0.4), legal = c(TRUE, FALSE, TRUE))
  expect_equal(ex$p, c(0.25, 0, 0.75), tolerance = 1e-12)
  expect_equal(ex$value, 0.4)
  lg <- Mctsexpand("s", c(0.5, 1.5, -1), logits = TRUE)
  expect_equal(lg$p, softmax_ref(c(0.5, 1.5, -1)), tolerance = 1e-12)
  expect_equal(Mctsexpand("s", c(0, 0, 1), legal = c(1, 1, 0))$p, c(0.5, 0.5, 0))
  expect_equal(Mctsexpand("s", c(0, 0), legal = c(0, 0))$p, c(0, 0))
  q <- Mctsq(c(2, 0, 4), c(1, 5, -2), unvisited = -1)
  expect_equal(q$q, c(0.5, -1, -0.5), tolerance = 1e-12)
  M <- rbind(c(1, 2), c(0, 0))
  qm <- Mctsq(M, c(0.3, -0.6))
  expect_equal(qm$q, c((0.3 - 1.2) / 3, 0), tolerance = 1e-12)
})

test_that("Elorating and Elomatch invert the logistic Elo curve", {
  r <- Elorating(c(6, 2, 2), ladder = c(100, -50), anchor = 20)
  s <- 0.7
  rt <- 20 + 400 * log(s / (1 - s))
  expect_equal(r$rating, rt, tolerance = 1e-12)
  expect_equal(r$expected, 1 / (1 + exp((c(100, -50) - rt) / 400)), tolerance = 1e-12)
  b10 <- Elorating(0.7, base = 10)
  expect_equal(b10$rating, 400 * log10(0.7 / 0.3), tolerance = 1e-12)
  expect_equal(Elorating(c(0, 0, 3))$rating, -Inf)
  expect_equal(Elorating(1)$rating, Inf)
  G <- rbind(c(6, 2, 2), c(1, 1, 8), c(0, 0, 4))
  m <- Elomatch(G, c(0, 100, 200))
  rr <- c(400 * log(0.7 / 0.3), 100 + 400 * log(0.15 / 0.85))
  expect_equal(m$per_rung, c(rr, -Inf), tolerance = 1e-12)
  expect_equal(m$rating, sum(c(10, 10) * rr) / 20, tolerance = 1e-12)
  expect_equal(c(m$wins, m$draws, m$losses), c(7, 3, 14))
  expect_equal(Elomatch(G[1, , drop = FALSE], 0, base = 10)$rating,
               400 * log10(0.7 / 0.3), tolerance = 1e-12)
})

test_that("Bnrunstat updates batch-norm running statistics", {
  x <- c(1.5, -0.3, 2.2, 0.8, 1.1)
  r <- Bnrunstat(x, runmean = 0.2, runvar = 1.4, momentum = 0.2, gamma = 1.5, beta = 0.1)
  vb <- mean((x - mean(x))^2)
  rm <- 0.8 * 0.2 + 0.2 * mean(x)
  rv <- 0.8 * 1.4 + 0.2 * var(x)
  expect_equal(c(r$runmean, r$runvar), c(rm, rv), tolerance = 1e-12)
  expect_equal(r$normalized, 1.5 * (x - rm) / sqrt(rv + 1e-5) + 0.1, tolerance = 1e-12)
  expect_equal(r$trainnorm, 1.5 * (x - mean(x)) / sqrt(vb + 1e-5) + 0.1, tolerance = 1e-12)
  expect_error(Bnrunstat(1), "two activations")
  expect_error(Bnrunstat(x, momentum = 2), "\\[0, 1\\]")
})

test_that("optimiser steps: SGD momentum, cosine/step LR, gradient clipping", {
  r <- Sgdmomstep(c(1, -2), c(0.5, 0.1), momentum = 0.9, weight_decay = 0.01,
                  lr = 0.1, buf = c(0.2, -0.3))
  nb <- 0.9 * c(0.2, -0.3) + c(0.5, 0.1) + 0.01 * c(1, -2)
  expect_equal(r$buf, nb, tolerance = 1e-12)
  expect_equal(r$theta_new, c(1, -2) - 0.1 * nb, tolerance = 1e-12)
  expect_equal(r$step_norm, 0.1 * sqrt(sum(nb^2)), tolerance = 1e-12)
  expect_true(is.nan(Sgdmomstep(numeric(0), numeric(0))$estimate))
  expect_equal(Coslrate(30, 100, lr_0 = 0.2, floor = 0.01)$lr,
               0.01 + 0.19 * 0.5 * (1 + cos(0.3 * pi)), tolerance = 1e-12)
  expect_equal(Coslrate(60, 100, kind = "step")$lr, 0.2 * 0.002 / 0.2, tolerance = 1e-12)
  expect_equal(Coslrate(150, 100, kind = "step")$lr, 0.2 * 0.0002 / 0.2, tolerance = 1e-12)
  expect_equal(Coslrate(-5, 100)$frac, 0)
  expect_equal(Coslrate(5, 0)$lr, 0.2, tolerance = 1e-12)
  g <- Gradclip(c(3, 4), max_norm = 2)
  expect_equal(g$clipped, c(3, 4) * 2 / 5, tolerance = 1e-12)
  expect_true(g$was_clipped)
  expect_false(Gradclip(c(0.3, 0.4))$was_clipped)
  expect_equal(Gradclip(c(0.3, 0.4))$estimate, 0.5, tolerance = 1e-12)
})

test_that("Agdproj solves the box-constrained least squares (KKT)", {
  X <- cbind(c(1, 2, 0.5, -1, 1.5, 0.3), c(0.2, -0.4, 1, 1.3, 0.7, -0.8),
             c(1, 1, 1, 1, 1, 1))
  y <- c(2.1, 0.5, 1.9, 1.2, 2.8, -0.4)
  r <- Agdproj(X, y, lower = c(-Inf, 0, 0.5), upper = c(0.4, Inf, Inf), steps = 5000)
  g <- as.numeric(crossprod(X, X %*% r$beta - y))
  lo <- c(-Inf, 0, 0.5)
  hi <- c(0.4, Inf, Inf)
  free <- r$beta > lo + 1e-9 & r$beta < hi - 1e-9
  expect_lt(max(abs(g[free])), 1e-8)
  expect_true(all(g[abs(r$beta - lo) <= 1e-9] >= -1e-8))
  expect_true(all(g[abs(r$beta - hi) <= 1e-9] <= 1e-8))
  expect_equal(r$objective, 0.5 * sum((X %*% r$beta - y)^2), tolerance = 1e-12)
  expect_equal(r$lipschitz, max(eigen(crossprod(X))$values), tolerance = 1e-9)
  un <- Agdproj(X, y, steps = 5000)
  expect_equal(un$beta, as.numeric(qr.solve(X, y)), tolerance = 1e-9)
  expect_error(Agdproj(X, y[-1]), "one row")
  expect_error(Agdproj(X, y, lower = c(1, 2)), "scalar or of length p")
  expect_error(Agdproj(X, y, lower = 1, upper = 0), "must not exceed")
})

test_that("Rootnoise and Noisealpha", {
  p <- c(0.5, 0.3, 0.2)
  r <- Rootnoise(p, alpha = 0.3, eps = 0.25)
  raw <- qgamma(c(0.5, 0.25, 0.75), shape = 0.3)
  et <- raw / sum(raw)
  expect_equal(r$eta, et, tolerance = 1e-9)
  expect_equal(r$p_noisy, 0.75 * p + 0.25 * et, tolerance = 1e-9)
  expect_equal(r$entropy, -sum(r$p_noisy * log(r$p_noisy)), tolerance = 1e-12)
  r2 <- Rootnoise(p, eps = 0.5, eta = c(1, 1, 2))
  expect_equal(r2$p_noisy, 0.5 * p + 0.5 * c(0.25, 0.25, 0.5), tolerance = 1e-12)
  big <- Rootnoise(c(0.5, 0.5), alpha = 3)
  expect_equal(big$eta, qgamma(c(0.5, 0.25), 3) / sum(qgamma(c(0.5, 0.25), 3)),
               tolerance = 1e-9)
  n <- Noisealpha(250)
  expect_equal(n$alpha, 10 / 250, tolerance = 1e-12)
  expect_equal(n$published_alpha, 0.03)
  expect_true(is.nan(Noisealpha(0)$alpha))
  expect_true(is.nan(Noisealpha(40)$published_alpha))
})

test_that("Replaypack and Gamelog digest a canonical text encoding", {
  buf <- rbind(c(1, 0.25, -1), c(2, 0.75, 1))
  txt <- paste(sprintf("%.17g,%.17g,%.17g", buf[, 1], buf[, 2], buf[, 3]), collapse = "\n")
  r <- Replaypack(buf)
  expect_equal(r$digest, rk_digest(txt))
  expect_equal(r$text_len, nchar(txt))
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  expect_true(Replaypack(buf, path = f)$written)
  expect_identical(readChar(f, 1000), txt)
  acts <- c(2, 0, 2, 1)
  vals <- c(0.1, -0.2, 0.3, 0.5)
  g <- Gamelog(acts, values = vals, visits = c(10, 8, 6, 4))
  gt <- paste(sprintf("%d,%.17g,%.17g,%.17g", 0:3, acts, c(10, 8, 6, 4), vals), collapse = "\n")
  expect_equal(g$digest, rk_digest(gt))
  expect_equal(g$action_entropy, -sum(c(0.25, 0.25, 0.5) * log(c(0.25, 0.25, 0.5))),
               tolerance = 1e-12)
  expect_equal(g$mean_value, mean(vals), tolerance = 1e-12)
  f2 <- tempfile()
  on.exit(unlink(f2), add = TRUE)
  expect_true(Gamelog(acts, path = f2)$written)
})

test_that("Distilkl is the temperature-scaled distillation loss", {
  t <- c(2, 0.5, -1)
  s <- c(1.5, 1, -0.5)
  r <- Distilkl(t, s, temperature = 3, label = 2, alpha = 0.3)
  p <- softmax_ref(t / 3)
  q <- softmax_ref(s / 3)
  ce <- -sum(p * log(q))
  expect_equal(r$softce, ce, tolerance = 1e-12)
  expect_equal(r$kl, sum(p * log(p / q)), tolerance = 1e-12)
  expect_equal(r$total, 0.3 * 9 * ce + 0.7 * -log(softmax_ref(s)[2]), tolerance = 1e-12)
  expect_equal(Distilkl(t, s)$total, 4 * -sum(softmax_ref(t / 2) * log(softmax_ref(s / 2))),
               tolerance = 1e-12)
  expect_error(Distilkl(t, s[-1]), "equal length")
  expect_error(Distilkl(t, s, temperature = 0), "strictly positive")
  expect_error(Distilkl(t, s, alpha = 2), "\\[0, 1\\]")
  expect_error(Distilkl(t, s, label = 4), "out of range")
})

test_that("MuZero pUCT, n-step target, value head, reanalyse", {
  Q <- c(0.2, -0.1, 0.5)
  N <- c(3, 0, 1)
  P <- c(0.3, 0.5, 0.2)
  r <- Mzpuct(Q, N, P)
  u <- 1.25 + log((4 + 19652 + 1) / 19652)
  sc <- (Q + 0.1) / 0.6 + P * 2 / (1 + N) * u
  expect_equal(r$score, sc, tolerance = 1e-12)
  expect_equal(r$best, which.max(sc) - 1L)
  expect_equal(Mzpuct(c(1, 1), c(1, 1), c(0.5, 0.5))$qbar, c(0, 0))
  expect_equal(Mzpuct(Q, N, P, qmin = -1, qmax = 1)$qbar, (Q + 1) / 2, tolerance = 1e-12)
  expect_error(Mzpuct(Q, N[-1], P), "same length")
  expect_error(Mzpuct(Q, c(-1, 0, 0), P), "non-negative")
  rw <- c(1, 0, 2, 1)
  vv <- c(0.5, 0.4, 0.3, 0.2)
  z <- Mznstep(rw, vv, n = 2, gamma = 0.9)
  ref <- c(1 + 0.9 * 0 + 0.81 * 0.3, 0 + 0.9 * 2 + 0.81 * 0.2, 2 + 0.9 * 1, 1)
  expect_equal(z$target, ref, tolerance = 1e-12)
  expect_error(Mznstep(rw, vv[-1]), "same length")
  expect_error(Mznstep(rw, vv, n = 0), "at least 1")
  lg <- c(0.1, 0.5, -0.2, 0.3, 0)
  mv <- Mzvalue(lg, support = 2, epsilon = 0.01)
  y <- sum(softmax_ref(lg) * (-2:2))
  a <- (sqrt(1 + 0.04 * (abs(y) + 1.01)) - 1) / 0.02
  expect_equal(mv$expected, y, tolerance = 1e-12)
  expect_equal(mv$value, sign(y) * (a^2 - 1), tolerance = 1e-12)
  hx <- function(x) sign(x) * (sqrt(abs(x) + 1) - 1) + 0.01 * x
  expect_equal(hx(mv$value), y, tolerance = 1e-9)
  expect_error(Mzvalue(1:4, support = 2), "2\\*support")
  expect_error(Mzvalue(lg, support = 2, epsilon = 0), "strictly positive")
  vis <- rbind(c(3, 1), c(0, 0), c(2, 2), c(1, 0))
  ra <- Mzreanal(rw, vv, vis, n = 2, gamma = 0.9, alpha = 2, beta = 0.5,
                 oldvalues = c(0, 1, 2, 3))
  pr <- abs(c(0, 1, 2, 3) - ref)
  prob <- pr^2 / sum(pr^2)
  expect_equal(ra$target, ref, tolerance = 1e-12)
  expect_equal(ra$policy, rbind(c(0.75, 0.25), c(0, 0), c(0.5, 0.5), c(1, 0)))
  expect_equal(ra$weight, (1 / (4 * prob))^0.5, tolerance = 1e-12)
  expect_error(Mzreanal(rw, vv[-1], vis), "same length")
  expect_error(Mzreanal(rw, vv, vis[-1, ]), "one row")
  expect_error(Mzreanal(rw, vv, vis, n = 0), "at least 1")
  expect_error(Mzreanal(rw, vv, vis, oldvalues = 1), "length T")
})

test_that("MuZero recurrent inference and world-model rollout", {
  dyn <- function(s, a) list(s * 0.1 + a, s + a)
  pred <- function(s) list(c(s, 1 - s) / 1, s / 10)
  r <- Mzrecur(2, 3, dyn, pred)
  expect_equal(c(r$reward, r$state, r$value), c(3.2, 5, 0.5), tolerance = 1e-12)
  expect_null(Mzrecur(2, 3, dyn)$value)
  w <- Mzworld(1, list(1, 2, 3), function(o) o * 2, dyn)
  expect_equal(w$rewards, c(0.2 + 1, 0.3 + 2, 0.5 + 3), tolerance = 1e-12)
  expect_equal(unlist(w$states), c(2, 3, 5, 8))
  expect_equal(w$K, 3L)
})

test_that("Resblock, Policyhead and Valuehead", {
  x <- c(0.5, -1, 2, 0.3, 1.2)
  bn <- function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2) + 1e-5)
  r <- Resblock(x)
  h1 <- pmax(bn(x), 0)
  expect_equal(r$h1, h1, tolerance = 1e-12)
  expect_equal(r$y, pmax(x + bn(h1), 0), tolerance = 1e-12)
  k <- c(0.25, 0.5, 0.25)
  conv <- function(v) stats::filter(c(0, v, 0), k, sides = 2)[2:6]
  rk <- Resblock(x, filters = list(k))
  hk <- pmax(bn(conv(x)), 0)
  expect_equal(rk$y, pmax(x + bn(conv(hk)), 0), tolerance = 1e-12)
  f <- c(0.2, -0.4, 1.1)
  W <- rbind(c(1, 0, 0.5), c(0, 1, 1), c(0.3, 0.3, 0.3), c(-1, 1, 0))
  ph <- Policyhead(f, W = W, legal = c(1, 0, 1, 1))
  lo <- as.numeric(W %*% f)
  pp <- exp(lo) * c(1, 0, 1, 1)
  expect_equal(ph$p, pp / sum(pp), tolerance = 1e-12)
  expect_equal(ph$entropy, -sum((pp / sum(pp))[-2] * log((pp / sum(pp))[-2])),
               tolerance = 1e-12)
  expect_equal(Policyhead(f, action_space = 4)$p, softmax_ref(c(f, 0)), tolerance = 1e-12)
  expect_equal(Policyhead(f, action_space = 2)$logits, f[1:2])
  v <- Valuehead(f, W = c(1, 2, -1), b = 0.3, scale = 0.5)
  expect_equal(v$v, tanh(0.5 * (0.3 + sum(c(1, 2, -1) * f))), tolerance = 1e-12)
  expect_equal(Valuehead(f)$v, tanh(mean(f)), tolerance = 1e-12)
})

test_that("Azloss, Pertarget, Virtloss, Tempdecay, Evalgate, Searchhoriz", {
  a <- Azloss(z = 1, v = 0.4, pi = c(0.7, 0.3, 0), p = c(0.5, 0.4, 0.1),
              theta = c(1, -2), c = 0.01)
  expect_equal(a$estimate, 0.36 - 0.7 * log(0.5) - 0.3 * log(0.4) + 0.05, tolerance = 1e-12)
  expect_equal(Azloss(0, 0, 1, 0)$policy_loss, -log(1e-300), tolerance = 1e-12)
  pt <- Pertarget(NULL, priorities = c(0.5, 0.1, 0.9), alpha = 0.5, beta = 0.4, eps = 0)
  pr <- sqrt(c(0.5, 0.1, 0.9)) / sum(sqrt(c(0.5, 0.1, 0.9)))
  w <- (3 * pr)^-0.4
  expect_equal(pt$prob, pr, tolerance = 1e-12)
  expect_equal(pt$weight, w / max(w), tolerance = 1e-12)
  rk <- Pertarget(NULL, z = c(1, 0, -1), v = c(0.5, 0.4, 0.2), variant = "rank", alpha = 1)
  expect_equal(rk$priority, c(1 / 2, 1 / 3, 1))
  rb <- Pertarget(rbind(c(1, 0.5), c(0, 0.5)), eps = 0, alpha = 1)
  expect_equal(rb$prob, c(0.5, 0.5))
  vl <- Virtloss(W = c(2, -1, 0), N = c(4, 2, 0), pending = c(1, 0, 2), nvl = 3)
  expect_equal(vl$Q, c(-1 / 7, -0.5, -1), tolerance = 1e-12)
  expect_equal(vl$Qclean, c(0.5, -0.5, 0), tolerance = 1e-12)
  expect_error(Virtloss(1, 1:2, 1), "same length")
  expect_error(Virtloss(1, -1, 0), "non-negative")
  expect_equal(Tempdecay(10, N = c(2, 6, 2))$pi, c(0.2, 0.6, 0.2), tolerance = 1e-12)
  expect_equal(Tempdecay(40, N = c(2, 6, 6))$pi, c(0, 1, 0))
  expect_equal(Tempdecay(40)$tau, 0)
  eg <- Evalgate(new_net = 7, old_net = 3, draws = 2)
  expect_equal(eg$score, 8 / 12, tolerance = 1e-12)
  expect_equal(eg$p_value, stats::pbinom(6, 10, 0.5, lower.tail = FALSE), tolerance = 1e-12)
  expect_true(eg$replace)
  eg2 <- Evalgate(NULL, wins = 50, draws = 10, n_games = 100)
  expect_equal(eg2$losses, 40)
  expect_equal(eg2$p_value, stats::pbinom(49, 90, 0.5, lower.tail = FALSE), tolerance = 1e-12)
  sh <- Searchhoriz(3, "s", rewards = c(1, 2, 3, 4), values = c(0, 0, 0, 5, 9), gamma = 0.5)
  expect_equal(sh$estimate, 1 + 0.5 * 2 + 0.25 * 3 + 0.125 * 5, tolerance = 1e-12)
  sk <- Searchhoriz(3, "s", rewards = c(1, 2, 3), values = c(4, 5), k_start = 1, gamma = 0.5)
  expect_equal(sk$estimate, 2 + 0.5 * 3 + 0.25 * 5, tolerance = 1e-12)
})

test_that("Selfconsis, Agpuct, Agestd", {
  P <- rbind(c(2, 1, 1), c(1, 1, 2))
  s <- Selfconsis(P)
  n1 <- c(0.5, 0.25, 0.25)
  n2 <- c(0.25, 0.25, 0.5)
  H <- function(p) -sum(p * log(p))
  expect_equal(s$jsd, H((n1 + n2) / 2) - (H(n1) + H(n2)) / 2, tolerance = 1e-12)
  f <- Selfconsis(function(k) c(k, 1), seeds = list(1, 3))
  expect_equal(f$entropies, c(H(c(0.5, 0.5)), H(c(0.75, 0.25))), tolerance = 1e-12)
  expect_true(is.nan(Selfconsis(function(k) 1)$jsd))
  a <- Agpuct(P = c(0.5, 0.3, 0.2), N = c(2, 1, 0), Q = c(0.1, 0.4, 0), c_puct = 1.5)
  U <- 1.5 * c(0.5, 0.3, 0.2) * sqrt(3) / (1 + c(2, 1, 0))
  expect_equal(a$score, c(0.1, 0.4, 0) + U, tolerance = 1e-12)
  expect_equal(a$action, which.max(c(0.1, 0.4, 0) + U) - 1L)
  expect_error(Agpuct(numeric(0), numeric(0), numeric(0)), "no actions")
  expect_error(Agpuct(1, 1:2, 1), "same length")
  expect_error(Agpuct(-1, 1, 1), "priors")
  expect_error(Agpuct(1, -1, 1), "visit counts")
  expect_error(Agpuct(1, 1, 1, c_puct = -1), "c_puct")
  rt <- c(0.001, 0.004, 0.02)
  sp <- c(30, 50, 20)
  ag <- Agestd(rt, sp, person_time = c(1000, 2000, 500))
  expect_equal(ag$asr, sum(rt * sp) / 100, tolerance = 1e-12)
  v <- sum(sp^2 * rt / c(1000, 2000, 500)) / 100^2
  expect_equal(ag$se, sqrt(v), tolerance = 1e-12)
  expect_equal(ag$ci_lower, ag$asr - qnorm(0.975) * sqrt(v), tolerance = 1e-12)
  expect_true(is.na(Agestd(rt, sp)$se))
  expect_error(Agestd(rt, sp[-1]), "same length")
  expect_error(Agestd(rt, c(0, 0, 0)), "positive total")
  expect_error(Agestd(rt, sp, c(1, 0, 1)), "positive")
})

test_that("Rolloutmc mixes the value net with the rollout outcome", {
  net <- function(s) c(0.2, 0.3, 0.5)
  step <- function(s, a) s + a
  r <- Rolloutmc(0, net, horizon = 3, step = step, outcome = function(s) s / 10,
                 value_net = function(s) 0.4, lam = 0.25)
  # the default stream is the base-2 radical inverse 0.5, 0.25, 0.75
  acts <- vapply(c(0.5, 0.25, 0.75), function(u) which(u < cumsum(c(0.2, 0.3, 0.5)))[1] - 1, 0)
  expect_equal(r$actions, as.integer(acts))
  expect_equal(r$z, sum(acts) / 10, tolerance = 1e-12)
  expect_equal(r$estimate, 0.75 * 0.4 + 0.25 * sum(acts) / 10, tolerance = 1e-12)
  st <- Rolloutmc(0, net, horizon = 2, step = step, stream = c(0.1, 0.99))
  expect_equal(st$actions, c(0L, 2L))
  one <- Rolloutmc(5, net)
  expect_equal(one$plies, 0L)
  expect_length(one$actions, 1L)
  tm <- Rolloutmc(0, net, horizon = 5, step = step, terminal = function(s) s >= 1)
  expect_true(tail(unlist(tm$trajectory), 1) >= 1)
})

test_that("Azsearch runs PUCT simulations and Azselfplay samples from pi", {
  net <- function(s) list(c(0.6, 0.3, 0.1), 0.2)
  r <- Azsearch("root", net, num_sim = 6, c_puct = 1)
  P <- c(0.6, 0.3, 0.1)
  puct_counts <- function(v, cp) {
    N <- c(0, 0, 0)
    W <- c(0, 0, 0)
    for (k in 1:6) {
      q <- ifelse(N > 0, W / pmax(N, 1), 0)
      a <- which.max(q + cp * P * sqrt(sum(N)) / (1 + N))
      N[a] <- N[a] + 1
      W[a] <- W[a] - v
    }
    list(N = N, W = W)
  }
  N <- puct_counts(0.2, 1)$N
  W <- puct_counts(0.2, 1)$W
  expect_equal(r$n, N)
  expect_equal(r$pi, N / 6, tolerance = 1e-12)
  expect_equal(r$value, sum(W) / 6, tolerance = 1e-12)
  rn <- Azsearch("root", net, num_sim = 0, root_noise = c(1, 1, 2), eps = 0.5)
  expect_equal(rn$p, 0.5 * P + 0.5 * c(0.25, 0.25, 0.5), tolerance = 1e-12)
  deep <- Azsearch(0, function(s) list(c(0.5, 0.5), s / 10), num_sim = 4,
                   step = function(s, a) if (s < 2) s + a + 1 else NULL)
  expect_equal(sum(deep$n), 4)
  expect_gt(deep$n_nodes, 1L)
  sp <- Azselfplay("root", function(s) c(0.6, 0.3, 0.1), mcts_iter = 6)
  n0 <- puct_counts(0, 1.25)$N
  expect_equal(sp$pis[[1]], n0 / 6, tolerance = 1e-12)
  expect_equal(sp$actions, which(0.5 < cumsum(n0 / 6))[1] - 1L)
  gm <- Azselfplay(0, function(s) c(0.5, 0.5), value = function(s) 0, mcts_iter = 2,
                   step = function(s, a) s + 1, terminal = function(s) s >= 2,
                   outcome = function(s) 1, temp_threshold = 1)
  expect_equal(gm$moves, 2L)
  expect_equal(gm$zs, c(1, -1))
})
