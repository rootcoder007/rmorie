# Coverage for ABC rejection, SIR resampling, Meng-Wong bridge sampling,
# parallel tempering, max pooling, GloVe, IPW causal mediation, Fieller
# effective doses and nuclear-norm matrix completion; recomputed from
# replays of the uniform stream, glm(), MASS::dose.p() and svd() in the
# test body.

test_that("Abcrej accepts the draws within eps of the observed summaries", {
  sim <- function(th, e) c(th[1] + th[2], th[1] - th[2])
  obs <- c(1, 0.2)
  r <- Abcrej(sim, obs, 0.4, list(c(0, 1), c(-1, 1)), n_draws = 300, seed = 5)
  u <- .ghc_unif(.ghc_rng(5), 600)
  th <- cbind(u[c(TRUE, FALSE)], -1 + 2 * u[c(FALSE, TRUE)])
  d <- sqrt((th[, 1] + th[, 2] - 1)^2 + (th[, 1] - th[, 2] - 0.2)^2)
  ok <- d <= 0.4
  expect_equal(r$n_accepted, sum(ok))
  expect_equal(r$distances, d[ok], tolerance = 1e-12)
  expect_equal(r$posterior_mean, colMeans(th[ok, ]), tolerance = 1e-12)
  expect_equal(r$acceptance_rate, mean(ok), tolerance = 1e-12)
  none <- Abcrej(sim, c(50, 50), 0.1, list(c(0, 1), c(0, 1)), n_draws = 10)
  expect_true(all(is.nan(none$posterior_mean)))
  noisy <- Abcrej(function(th, e) th + .ghc_unif(e, 1L), 0.5, 1, list(c(0, 1)), n_draws = 4, seed = 2)
  v <- .ghc_unif(.ghc_rng(2), 8)
  expect_equal(unlist(noisy$samples), v[c(TRUE, FALSE)][abs(v[c(TRUE, FALSE)] + v[c(FALSE, TRUE)] - 0.5) <= 1])
  expect_error(Abcrej(sim, obs, 0, list(c(0, 1))), "eps must be positive")
  expect_error(Abcrej(sim, obs, 1, list(c(1, 0))), "low < high")
  expect_error(Abcrej(sim, 1, 1, list(c(0, 1), c(0, 1)), n_draws = 1), "matching obs")
})

test_that("Bayisr resamples by inverse CDF on the importance weights", {
  x <- c(-1, 0, 0.5, 1.2, 2)
  lt <- function(v) dnorm(v, 0.5, 1, log = TRUE)
  lq <- function(v) dnorm(v, 0, 2, log = TRUE)
  r <- Bayisr(x, lt, lq, m = 7, seed = 3)
  lw <- lt(x) - lq(x)
  w <- exp(lw - max(lw))
  expect_equal(r$weights, w / sum(w), tolerance = 1e-12)
  expect_equal(r$ess, 1 / sum((w / sum(w))^2), tolerance = 1e-12)
  u <- .ghc_unif(.ghc_rng(3), 7) * sum(w)
  idx <- vapply(u, function(v) min(c(which(v <= cumsum(w)), 5L)), 0L)
  expect_equal(r$indices, idx - 1L)
  expect_equal(unlist(r$resample), x[idx])
  expect_error(Bayisr(numeric(0), lt, lq, 1), "non-empty")
  expect_error(Bayisr(x, lt, lq, 0), "positive integer")
})

test_that("Bridgs reaches the Meng-Wong fixed point", {
  set.seed(10)
  d1 <- rnorm(400)
  d2 <- rnorm(500, 0, 2)
  q1 <- function(v) -v^2 / 2
  q2 <- function(v) -v^2 / 8
  r <- Bridgs(d1, d2, q1, q2)
  expect_true(r$converged)
  l1 <- q1(d1) - q2(d1)
  l2 <- q1(d2) - q2(d2)
  s1 <- 400 / 900
  s2 <- 500 / 900
  rr <- r$ratio
  num <- mean(exp(l2) / (s1 * exp(l2) + s2 * rr))
  den <- mean(1 / (s1 * exp(l1) + s2 * rr))
  expect_equal(num / den, rr, tolerance = 1e-10)
  # the exact ratio of normalising constants is sqrt(2 pi) / (2 sqrt(2 pi))
  expect_lt(abs(r$log_ratio - log(0.5)), 0.1)
  one <- Bridgs(d1, d2, q1, q2, max_iter = 1)
  expect_false(one$converged)
  expect_error(Bridgs(numeric(0), d2, q1, q2), "non-empty")
})

test_that("Ptmcmc replays Metropolis moves and adjacent swaps", {
  lp <- function(v) -abs(v)^1.5
  temps <- c(1, 2.5, 6)
  r <- Ptmcmc(lp, temps, 0.3, n_iter = 40, step = 0.8, seed = 4, swap_every = 2)
  u <- .ghc_unif(.ghc_rng(4), 2000)
  k <- 0
  nx <- function(m) {
    out <- u[k + seq_len(m)]
    k <<- k + m
    out
  }
  x <- rep(0.3, 3)
  l <- lp(x)
  acc <- c(0, 0, 0)
  sw <- c(0, 0)
  cold <- numeric(40)
  for (s in 1:40) {
    for (j in 1:3) {
      z <- nx(2)
      pr <- x[j] + 0.8 * sqrt(temps[j]) * sqrt(-2 * log(z[1])) * cos(2 * pi * z[2])
      if (log(nx(1)) < (lp(pr) - l[j]) / temps[j]) {
        x[j] <- pr
        l[j] <- lp(pr)
        acc[j] <- acc[j] + 1
      }
    }
    if (s %% 2 == 0) for (j in 1:2) {
      dl <- (1 / temps[j] - 1 / temps[j + 1]) * (l[j + 1] - l[j])
      if (log(nx(1)) < min(0, dl)) {
        x[j:(j + 1)] <- x[(j + 1):j]
        l[j:(j + 1)] <- l[(j + 1):j]
        sw[j] <- sw[j] + 1
      }
    }
    cold[s] <- x[1]
  }
  expect_equal(r$chain, cold, tolerance = 1e-12)
  expect_equal(r$accept_rate, acc / 40, tolerance = 1e-12)
  expect_equal(r$swap_accept_rate, sw / 20, tolerance = 1e-12)
  expect_error(Ptmcmc(lp, 1, 0), "at least two temperatures")
  expect_error(Ptmcmc(lp, c(2, 1), 0), "positive and ascending")
})

test_that("Maxpl takes windowed maxima with first-index ties", {
  x <- c(1, 5, 5, 2, 7, 3, 3, 9)
  r <- Maxpl(x, 3, 2)
  st <- seq(1, 6, by = 2)
  expect_equal(r$pooled, vapply(st, function(a) max(x[a:(a + 2)]), 0))
  expect_equal(r$argmax, vapply(st, function(a) a - 1 + which.max(x[a:(a + 2)]) - 1, 0))
  expect_equal(Maxpl(x, 1, 1)$pooled, x)
  expect_error(Maxpl(x, 9, 1), "wider than the input")
  expect_error(Maxpl(x, 0, 1), "kernel must be")
  expect_error(Maxpl(x, 2, 0), "stride must be")
  expect_error(Maxpl(numeric(0), 1, 1), "x is empty")
})

test_that("glove_weight, glove_loss and morie_glove follow Pennington et al.", {
  expect_equal(glove_weight(0), 0)
  expect_equal(glove_weight(150), 1)
  expect_equal(glove_weight(20, 50, 0.5), sqrt(0.4), tolerance = 1e-12)
  corpus <- list("a b c a", c("b", "c"))
  r <- morie_glove(corpus, dim = 2, window = 2, epochs = 2, lr = 0.1, seed = 3)
  expect_equal(r$vocab, c("a", "b", "c"))
  cooc <- function(harm) {
    C <- matrix(0, 3, 3)
    for (doc in list(c(1, 2, 3, 1), c(2, 3))) {
      for (p in seq_along(doc)) for (o in seq_len(p - 1)) {
        if (p - o <= 2) {
          v <- if (harm) 1 / (p - o) else 1
          C[doc[p], doc[o]] <- C[doc[p], doc[o]] + v
          C[doc[o], doc[p]] <- C[doc[o], doc[p]] + v
        }
      }
    }
    C
  }
  C <- cooc(TRUE)
  X <- r$cooccurrence
  expect_equal(X$count, C[cbind(X$i, X$j)], tolerance = 1e-12)
  expect_equal(nrow(X), sum(C > 0))
  u <- .ghc_unif(.ghc_rng(3), 18)
  W <- matrix((u[1:6] - 0.5) * 0.25, 3, byrow = TRUE)
  Wt <- matrix((u[7:12] - 0.5) * 0.25, 3, byrow = TRUE)
  b <- (u[13:15] - 0.5) * 0.25
  bt <- (u[16:18] - 0.5) * 0.25
  gW <- gWt <- matrix(1, 3, 2)
  gb <- gbt <- rep(1, 3)
  fw <- (X$count / 100)^0.75
  for (ep in 1:2) for (k in seq_len(nrow(X))) {
    i <- X$i[k]
    j <- X$j[k]
    g <- 2 * fw[k] * (sum(W[i, ] * Wt[j, ]) + b[i] + bt[j] - log(X$count[k]))
    gi <- g * Wt[j, ]
    gj <- g * W[i, ]
    W[i, ] <- W[i, ] - 0.1 * gi / sqrt(gW[i, ])
    Wt[j, ] <- Wt[j, ] - 0.1 * gj / sqrt(gWt[j, ])
    gW[i, ] <- gW[i, ] + gi^2
    gWt[j, ] <- gWt[j, ] + gj^2
    b[i] <- b[i] - 0.1 * g / sqrt(gb[i])
    bt[j] <- bt[j] - 0.1 * g / sqrt(gbt[j])
    gb[i] <- gb[i] + g^2
    gbt[j] <- gbt[j] + g^2
  }
  expect_equal(r$W, W, tolerance = 1e-12)
  expect_equal(r$W_tilde, Wt, tolerance = 1e-12)
  expect_equal(r$b_tilde, bt, tolerance = 1e-12)
  loss <- sum(fw * (rowSums(W[X$i, ] * Wt[X$j, ]) + b[X$i] + bt[X$j] - log(X$count))^2)
  expect_equal(r$final_loss, loss, tolerance = 1e-12)
  expect_equal(glove_loss(X, W, Wt, b, bt), loss, tolerance = 1e-12)
  expect_equal(glove_loss(X[0, ], W, Wt, b, bt), 0)
  expect_equal(r$vectors[[1]], W[1, ] + Wt[1, ], tolerance = 1e-12)
  cc <- morie_glove(corpus, dim = 2, window = 2, epochs = 1, seed = 3, combine = "concat", harmonic = FALSE)
  expect_equal(length(cc$vectors[[1]]), 4)
  X0 <- cc$cooccurrence
  expect_equal(X0$count, cooc(FALSE)[cbind(X0$i, X0$j)])
  expect_equal(morie_glove(corpus, dim = 2, epochs = 1, combine = "w")$vectors[[2]],
               morie_glove(corpus, dim = 2, epochs = 1)$W[2, ])
  expect_error(morie_glove(corpus, combine = "x"), "combine must be")
  expect_error(morie_glove(corpus, dim = 0), "dim must be at least 1")
  expect_error(morie_glove(list("a a a")), "at least two")
  expect_error(morie_glove(list("a", "b")), "no co-occurrences")
  expect_error(morie_glove(NULL), "must not be None")
})

test_that("morie_causal_mediation reweights by probit propensities", {
  set.seed(11)
  n <- 300
  x <- rnorm(n)
  d <- rbinom(n, 1, pnorm(0.3 * x))
  m <- 0.6 * d + 0.4 * x + rnorm(n)
  y <- 1 + 0.5 * d + 0.8 * m + 0.3 * x + rnorm(n)
  r <- morie_causal_mediation(y, d, m, x)
  px <- fitted(glm(d ~ x, family = binomial("probit"), control = glm.control(epsilon = 1e-14)))
  pm <- fitted(glm(d ~ m + x, family = binomial("probit"), control = glm.control(epsilon = 1e-14)))
  wm <- function(w) sum(y * w) / sum(w)
  y11 <- wm(d / px)
  y01 <- wm((1 - d) * pm / ((1 - pm) * px))
  y10 <- wm(d * (1 - pm) / (pm * (1 - px)))
  y00 <- wm((1 - d) / (1 - px))
  expect_equal(c(r$y11, r$y01, r$y10, r$y00), c(y11, y01, y10, y00), tolerance = 1e-8)
  expect_equal(r$total_effect, y11 - y00, tolerance = 1e-8)
  expect_equal(r$direct_treated, y11 - y01, tolerance = 1e-8)
  expect_equal(r$indirect_control, (y11 - y00) - (y11 - y01), tolerance = 1e-8)
  expect_true(r$decomposition_holds)
  lg <- morie_causal_mediation(y, d, m, x, link = "logit", trim = 0.2)
  pxl <- fitted(glm(d ~ x, family = binomial, control = glm.control(epsilon = 1e-14)))
  pml <- fitted(glm(d ~ m + x, family = binomial, control = glm.control(epsilon = 1e-14)))
  expect_equal(lg$n_trimmed, sum(!(pxl > 0.2 & pxl < 0.8 & pml > 0.2 & pml < 0.8)))
  bs <- morie_causal_mediation(y, d, m, x, boot = 5, seed = 1)
  expect_equal(names(bs$se)[1], "total_effect")
  expect_true(all(bs$se > 0))
  expect_error(morie_causal_mediation(y, d + 1, m, x), "binary")
  expect_error(morie_causal_mediation(y, d, m, x, trim = 0.5), "trim must lie")
  expect_error(morie_causal_mediation(y[-1], d, m, x), "same")
  expect_error(morie_causal_mediation(replace(y, 1, NA), d, m, x), "finite")
})

test_that("morie_effective_dose gives the delta SE and Fieller roots", {
  set.seed(12)
  dose <- rep(c(1, 2, 4, 8, 16), each = 20)
  dead <- rbinom(100, 1, pnorm(-2 + 1.2 * log(dose)))
  f <- glm(dead ~ log(dose), family = binomial("probit"))
  V <- vcov(f)
  a <- coef(f)[[1]]
  b <- coef(f)[[2]]
  r <- morie_effective_dose(a, b, V, level = 0.5)
  dp <- MASS::dose.p(f, p = 0.5)
  expect_equal(r$ed, as.numeric(dp), tolerance = 1e-12)
  expect_equal(r$se_delta, as.numeric(attr(dp, "SE")), tolerance = 1e-10)
  t2 <- qnorm(0.975)^2
  fl <- function(x) (a + b * x)^2 - t2 * (V[1, 1] + 2 * x * V[1, 2] + x^2 * V[2, 2])
  expect_equal(fl(r$lower), 0, tolerance = 1e-9)
  expect_equal(fl(r$upper), 0, tolerance = 1e-9)
  expect_lt(r$lower, r$ed)
  expect_equal(r$fieller_g, t2 * V[2, 2] / b^2, tolerance = 1e-12)
  expect_equal(r$ed_dose, exp(r$ed), tolerance = 1e-12)
  lg <- morie_effective_dose(a, b, V, level = 0.9, link = "logit", log_scale = FALSE)
  expect_equal(lg$ed, (log(9) - a) / b, tolerance = 1e-12)
  expect_null(lg$ed_dose)
  ub <- morie_effective_dose(a, b, V * 1e4)
  expect_false(ub$bounded)
  expect_equal(c(ub$lower, ub$upper), c(-Inf, Inf))
  expect_true(is.na(morie_effective_dose(a, 0, V)$ed))
  expect_error(morie_effective_dose(a, b, diag(3)), "must be 2x2")
  expect_error(morie_effective_dose(a, b, V, level = 1), "level must lie")
})

test_that("nuclear_norm, sample_bound, svt and the morie_meglt dispatcher", {
  M <- outer(1:4, c(1, -1, 2)) + 0.1 * diag(4)[, 1:3]
  expect_equal(nuclear_norm(M), sum(svd(M)$d), tolerance = 1e-12)
  expect_equal(morie_meglt("nuclear_norm", M)$nuclear_norm, sum(svd(M)$d), tolerance = 1e-12)
  sb <- sample_bound(50, 2, C = 1.5)
  expect_equal(sb$m, 1.5 * 50^1.2 * 2 * log(50), tolerance = 1e-12)
  expect_equal(sb$fraction, sb$m / 2500, tolerance = 1e-12)
  expect_equal(morie_meglt("sample_bound", 50, 2, exponent = 1.25)$m, 50^1.25 * 2 * log(50), tolerance = 1e-12)
  expect_error(sample_bound(50, 2, exponent = 2), "exponent must be")
  expect_error(sample_bound(1, 1), "need n >= 2")
  obs <- list(c(0, 0), c(0, 1), c(1, 0), c(1, 2), c(2, 1), c(3, 0), c(3, 2), c(2, 2), c(0, 0))
  r <- svt(M, obs, tau = 2, step = 1.2, iters = 30, tol = 0)
  o <- unique(do.call(rbind, obs)) + 1
  Y <- matrix(0, 4, 3)
  h <- numeric(0)
  for (it in 1:30) {
    s <- svd(Y)
    X <- s$u %*% diag(pmax(s$d - 2, 0)) %*% t(s$v)
    dd <- M[o] - X[o]
    Y[o] <- Y[o] + 1.2 * dd
    h <- c(h, sqrt(sum(dd^2)))
  }
  expect_equal(r$X, X, tolerance = 1e-9)
  expect_equal(r$residual_history, h, tolerance = 1e-9)
  expect_equal(r$n_observed, 8)
  expect_equal(r$nuclear_norm, sum(svd(X)$d), tolerance = 1e-9)
  expect_equal(svt(M, obs, iters = 1)$tau, 5 * sqrt(12), tolerance = 1e-12)
  stop_early <- svt(M, obs, tau = 2, iters = 500, tol = 1e-3)
  expect_lt(stop_early$final_residual, 1e-3)
  expect_lt(length(stop_early$residual_history), 500)
  expect_equal(morie_meglt("svt", M, obs, tau = 2, step = 1.2, iters = 30, tol = 0)$X, r$X)
  rel <- morie_meglt("relative_error", lapply(1:4, function(i) M[i, ] + 0.1), M)$relative_error
  expect_equal(rel, sqrt(12 * 0.01) / sqrt(sum(M^2)), tolerance = 1e-12)
  co <- morie_meglt("coherence", M, rank = 1)
  expect_equal(co$mu_row, 4 * max(svd(M)$u[, 1]^2), tolerance = 1e-12)
  expect_match(morie_meglt("cheatsheet")$cheatsheet, "NUCLEAR NORM")
  expect_match(morie_mfovsm_cheatsheet(), "NUMERATOR")
  expect_error(morie_meglt("nope"), "unknown op")
  expect_error(morie_meglt(), "op must be one of")
  expect_error(svt(M, list()), "no entries were observed")
})
