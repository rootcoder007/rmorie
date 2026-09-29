# Coverage for sacc .. SampR exports. Every expectation is recomputed in
# the test body.

cov_s1_mdp <- function() {
  P1 <- rbind(c(0.8, 0.2, 0), c(0.1, 0.6, 0.3), c(0, 0.3, 0.7))
  P2 <- rbind(c(0.2, 0.5, 0.3), c(0.4, 0.4, 0.2), c(0.5, 0, 0.5))
  R <- rbind(c(1, 0), c(0, 2), c(0.5, 1.5))
  list(P = list(P1, P2), R = R)
}

test_that("soft policy iteration reaches the soft Bellman optimum", {
  m <- cov_s1_mdp()
  for (fn in list(morie_sacc, Sacc)) {
    r <- fn(m$P, m$R, gamma = 0.8, temp = 0.5)
    Q <- r$q
    V <- 0.5 * log(rowSums(exp(Q / 0.5)))
    expect_true(r$converged)
    expect_equal(r$estimate, V, tolerance = 1e-9)
    expect_equal(Q, m$R + 0.8 * cbind(m$P[[1]] %*% V, m$P[[2]] %*% V), tolerance = 1e-9)
    expect_equal(r$policy, exp((Q - V) / 0.5), tolerance = 1e-9)
    expect_equal(r$entropy, -rowSums(r$policy * log(r$policy)), tolerance = 1e-9)
  }
  # soft value iteration from scratch gives the same fixed point
  V <- numeric(3)
  for (i in 1:2000) {
    Q <- m$R + 0.8 * cbind(m$P[[1]] %*% V, m$P[[2]] %*% V)
    V <- 0.5 * log(rowSums(exp(Q / 0.5)))
  }
  expect_equal(morie_sacc(m$P, m$R, 0.8, temp = 0.5)$estimate, as.numeric(V), tolerance = 1e-9)
  expect_error(morie_sacc(m$P, m$R, 0.8, temp = 0), "temp must be positive")
  expect_error(Sacc(m$P, m$R, 0.8, temp = -1), "temp must be positive")
})

test_that("Sacmod is the SARAR model with one weight matrix", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  n <- 16
  xy <- expand.grid(1:4, 1:4)
  D <- as.matrix(stats::dist(xy))
  W <- (D > 0 & D <= 1) * 1
  W <- W / rowSums(W)
  x <- sin(1:n) + 0.1 * (1:n)
  e <- cos(3 * (1:n)) * 0.3
  y <- solve(diag(n) - 0.3 * W, 1 + 0.8 * x + solve(diag(n) - 0.2 * W, e))
  r <- Sacmod(y, cbind(1, x), W)
  expect_same_function(morie_spatial_combined, Sacmod)
  lw <- spdep::mat2listw(W, style = "W")
  f <- spatialreg::sacsarlm(y ~ x, data = data.frame(y = y, x = x), listw = lw,
                            control = list(pre_eig1 = NULL))
  # both maximise the same concentrated likelihood, each to its own
  # optimiser tolerance
  expect_equal(r$loglik, as.numeric(f$LL), tolerance = 1e-6)
  expect_equal(c(r$rho, r$lambda), unname(c(f$rho, f$lambda)), tolerance = 1e-3)
  A <- diag(n) - r$rho * W
  B <- diag(n) - r$lambda * W
  Xs <- B %*% cbind(1, x)
  b <- solve(crossprod(Xs), crossprod(Xs, B %*% A %*% y))
  expect_equal(r$estimate, as.numeric(b), tolerance = 1e-10)
})

test_that("corpus BLEU with clipped precisions and the closest-reference brevity penalty", {
  cand <- c("the cat sat on the mat", "a quick brown fox")
  refs <- list(c("the cat is on the mat", "there is a cat on the mat"), c("the quick brown fox jumps"))
  tk <- function(s) strsplit(s, " ")[[1]]
  ng <- function(t, n) if (length(t) >= n) vapply(seq_len(length(t) - n + 1), function(i) paste(t[i:(i + n - 1)], collapse = " "), "") else character(0)
  num <- den <- numeric(2)
  for (i in 1:2) {
    ct <- tk(cand[i])
    for (n in 1:2) {
      cc <- table(ng(ct, n))
      best <- sapply(names(cc), function(g) max(vapply(refs[[i]], function(r) sum(ng(tk(r), n) == g), 0)))
      num[n] <- num[n] + sum(pmin(cc, best))
      den[n] <- den[n] + sum(cc)
    }
  }
  p <- num / den
  c_len <- 6 + 4
  r_len <- 6 + 5
  bp <- exp(1 - r_len / c_len)
  r <- morie_sacrb_bleu(cand, refs, max_n = 2)
  expect_equal(r$precisions, p, tolerance = 1e-12)
  expect_equal(r$bp, bp, tolerance = 1e-12)
  expect_equal(r$bleu, bp * exp(mean(log(p))), tolerance = 1e-12)
  expect_equal(r$reference_length, r_len)
  expect_match(r$signature, "nrefs:2\\|case:mixed\\|tok:13a\\|ngram:2")
  expect_equal(morie_sacrb("same words here", list("same words here"), max_n = 3)$bleu, 1)
  # punctuation is split off by 13a
  expect_equal(morie_sacrb("hi, there", list("hi , there"), max_n = 1)$bleu, 1)
  expect_equal(morie_sacrb("Hello", list("hello"), max_n = 1, lowercase = TRUE)$bleu, 1)
  expect_equal(morie_sacrb("no overlap", list("totally different"))$bleu, 0)
  expect_error(morie_sacrb(cand, refs[1]), "reference sets")
  expect_error(morie_sacrb("a", list("a"), weights = c(0.5, 0.2, 0.2, 0.2)), "sum to 1")
  expect_error(morie_sacrb("a", list("a"), tokenizer = "moses"), "tokenizer must be one of")
})

test_that("CPO steps satisfy the trust region and the linearised constraints", {
  g <- c(1, 0.5, -0.2)
  H <- rbind(c(2, 0.3, 0), c(0.3, 1.5, 0.2), c(0, 0.2, 1))
  Hi <- solve(H)
  u <- morie_safrl(g, H, delta = 0.02)
  expect_equal(u$step, sqrt(2 * 0.02 / sum(g * Hi %*% g)) * as.numeric(Hi %*% g), tolerance = 1e-12)
  expect_equal(u$kl, 0.02, tolerance = 1e-12)
  B <- cbind(c(0.5, 1, 0.3))
  cc <- -0.01
  r <- morie_safrl(g, H, B = B, c = cc, delta = 0.02)
  expect_true(r$feasible)
  expect_equal(r$kl, 0.02, tolerance = 1e-10)
  # the violation is ~0 at the optimum, so compare absolutely
  expect_lt(abs(r$predicted_violation - (cc + sum(B * r$step))), 1e-14)
  # KKT: step proportional to H^-1 (g - B nu), nu >= 0, complementary slackness
  expect_gte(r$nu, 0)
  expect_equal(r$step, as.numeric(Hi %*% (g - B * r$nu)) / r$lambda_, tolerance = 1e-8)
  expect_lt(abs(r$nu * r$predicted_violation), 1e-8)
  expect_lte(r$predicted_violation, 1e-8)
  rec <- morie_safrl(g, H, B = B, c = 1, delta = 0.02)
  expect_false(rec$feasible)
  expect_true(rec$recovery)
  b <- as.numeric(B)
  expect_equal(rec$step, -sqrt(2 * 0.02 / sum(b * Hi %*% b)) * as.numeric(Hi %*% b), tolerance = 1e-12)
  expect_error(morie_safrl(g, H, delta = 0), "delta must be > 0")
  expect_error(morie_safrl(g, H[1:2, 1:2]), "matching g")
  expect_error(morie_safrl(g, H, B = B, c = c(1, 2)), "one entry per constraint")
  expect_same_function(morie_cpo_step, morie_safrl)
})

test_that("CMDP returns and the CPO worst-case violation bound", {
  S <- 0:2
  A <- c("l", "r")
  pol <- function(s, a) if (a == "r") 0.7 else 0.3
  stp <- function(s, a) if (a == "r") min(s + 1, 2) else max(s - 1, 0)
  rew <- function(s, a, s1) as.numeric(s1 == 2)
  cst <- function(s, a, s1) as.numeric(a == "r") * 0.5
  r <- morie_safrl_cmdp_returns(pol, S, A, stp, rew, list(cst), gamma = 0.9, start = 0)
  P <- matrix(0, 3, 3)
  rr <- cc <- numeric(3)
  for (s in 0:2) {
    P[s + 1, stp(s, "r") + 1] <- P[s + 1, stp(s, "r") + 1] + 0.7
    P[s + 1, stp(s, "l") + 1] <- P[s + 1, stp(s, "l") + 1] + 0.3
    rr[s + 1] <- 0.7 * rew(s, "r", stp(s, "r")) + 0.3 * rew(s, "l", stp(s, "l"))
    cc[s + 1] <- 0.7 * 0.5
  }
  V <- solve(diag(3) - 0.9 * P, rr)
  Vc <- solve(diag(3) - 0.9 * P, cc)
  expect_equal(r$J, V[1], tolerance = 1e-10)
  expect_equal(r$J_C[[1]], Vc[1], tolerance = 1e-10)
  expect_equal(morie_safrl_cmdp_returns(pol, S, A, stp, rew, list(cst), gamma = 0.9)$J, mean(V), tolerance = 1e-10)
  expect_equal(morie_safrl_worst_case_violation(0.01, 0.9, 0.5), sqrt(0.02) * 0.9 * 0.5 / 0.01, tolerance = 1e-12)
  expect_error(morie_safrl_worst_case_violation(-1, 0.9, 1), "delta must be >= 0")
  expect_error(morie_safrl_worst_case_violation(0.1, 1, 1), "gamma must lie")
})

test_that("GraphSAGE layers aggregate, concatenate, rectify and normalise", {
  G <- rbind(c(0, 1, 1, 0), c(1, 0, 0, 1), c(1, 0, 0, 1), c(0, 1, 1, 0))
  X <- rbind(c(1, 0), c(0.5, 2), c(-1, 1), c(2, -0.5))
  W <- matrix(c(0.3, -0.2, 0.5, 0.1, 0.4, 0.2, -0.1, 0.6), 4)
  nrm <- function(M) M / ifelse(sqrt(rowSums(M^2)) > 0, sqrt(rowSums(M^2)), 1)
  agg <- (G %*% X) / rowSums(G)
  r <- sage(G, X, W = W)
  expect_equal(r$Z, nrm(pmax(cbind(X, agg) %*% W, 0)), tolerance = 1e-12)
  mx <- t(apply(G, 1, function(g) apply(X[g != 0, , drop = FALSE], 2, max)))
  rm <- sage(G, X, W = W, aggregator = "max")
  expect_equal(rm$Z, nrm(pmax(cbind(X, mx) %*% W, 0)), tolerance = 1e-12)
  Wc <- matrix(c(1, -0.5, 0.2, 0.8), 2)
  rc <- sage(G, X, W = Wc, convolutional = TRUE)
  cm <- ((G + diag(4)) %*% X) / (rowSums(G) + 1)
  expect_equal(rc$Z, nrm(pmax(cm %*% Wc, 0)), tolerance = 1e-12)
  two <- sage(G, X, W = W, K = 2)
  Z1 <- nrm(pmax(cbind(X, agg) %*% W, 0))
  expect_equal(two$Z, nrm(pmax(cbind(Z1, (G %*% Z1) / rowSums(G)) %*% W, 0)), tolerance = 1e-12)
  expect_same_function(morie_graphsage, sage)
  expect_error(sage(G, X, aggregator = "median"), "aggregator must be")
})

test_that("SAM multi-mask helpers", {
  m1 <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 0), 3)
  m2 <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0), 3)
  m3 <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0), 3)
  a <- average_of_valid_masks(list(m1, m2))
  avg <- (as.numeric(m1) + as.numeric(m2)) / 2
  expect_equal(a$mask, avg)
  expect_equal(a$ambiguous_fraction, mean(avg > 0.05 & avg < 0.95))
  loss <- function(p, t) sum((as.numeric(p) - as.numeric(t))^2)
  ml <- min_loss_over_masks(list(m1, m2, m3), m2, loss)
  ls <- c(loss(m1, m2), 0, loss(m3, m2))
  expect_equal(ml$losses, ls)
  expect_equal(ml$index, 1L)
  expect_equal(ml$gap, mean(ls), tolerance = 1e-12)
  w <- whole_part_subpart(list(m1, m2, m3))
  expect_equal(unlist(w$assignment), c(whole = 1L, part = 0L, subpart = 2L))
  expect_true(w$nested)
  iouf <- function(a, b) sum(a & b) / sum(a | b)
  rk <- rank_masks(list(m1, m2, m3), c(0.4, 0.9, 0.1), target = m1)
  tru <- c(1, iouf(m2 > 0.5, m1 > 0.5), iouf(m3 > 0.5, m1 > 0.5))
  expect_equal(rk$order, c(1L, 0L, 2L))
  expect_equal(rk$true_iou, tru, tolerance = 1e-12)
  expect_equal(rk$regret, 1 - tru[2], tolerance = 1e-12)
  expect_equal(rk$calibration_error, mean(abs(c(0.4, 0.9, 0.1) - tru)), tolerance = 1e-12)
  expect_false(rk$correct)
  expect_error(whole_part_subpart(list(m1, m2)), "THREE outputs")
  expect_error(rank_masks(list(m1, m2), 0.5), "predicted IoUs")
  expect_error(average_of_valid_masks(list()), "no masks")
})

test_that("effective sample size and sample moments", {
  w <- c(1, 2, 0.5, 3, NA, 0, 1.5)
  v <- c(1, 2, 0.5, 3, 1.5)
  expect_equal(morie_effective_sample_size(w), sum(v)^2 / sum(v^2), tolerance = 1e-12)
  expect_equal(morie_effective_sample_size(rep(2, 6)), 6, tolerance = 1e-12)
  x <- c(2.1, 3.4, 1.8, 4.0)
  y <- c(1.0, 2.5, 0.9, 3.8)
  expect_equal(SampMean(x)$mean, mean(x))
  expect_error(SampMean(c(1, NA)), "non-empty")
  s <- SampR(x, y)
  expect_equal(s$r, stats::cor(x, y), tolerance = 1e-12)
  expect_equal(s$cov, stats::cov(x, y) * 3 / 4, tolerance = 1e-12)
  expect_error(SampR(x, y[-1]), "equal-length")
  expect_error(SampR(c(1, 1), c(1, 2)), "zero variance")
})
