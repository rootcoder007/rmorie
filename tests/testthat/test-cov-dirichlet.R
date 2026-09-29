# Coverage for the Dirichlet-process family (dp*.R, Dpld.R, Dpvar.R,
# crpcol.R, crpgib.R): closed forms are recomputed from the formulas and
# the Gibbs samplers are replayed on the package's SplitMix64 stream.

vdc2 <- function(i, base = 2) {
  f <- 1
  r <- 0
  k <- i + 1
  while (k > 0) {
    f <- f / base
    r <- r + f * (k %% base)
    k <- k %/% base
  }
  r
}
lnorm_pdf <- function(x, m, v) -0.5 * (log(2 * pi * v) + (x - m)^2 / v)
dp_y <- c(-2.1, -1.8, -2.4, 2.2, 1.9, 2.5, -2.0, 2.3, 0.1)

test_that("Crp seats customers by the Chinese restaurant rule", {
  u <- c(0.1, 0.7, 0.2, 0.9, 0.35, 0.6)
  r <- Crp(6, alpha = 1.5, u = u)
  cnt <- numeric(0)
  tab <- integer(6)
  for (i in 1:6) {
    cw <- cumsum(cnt / (i - 1 + 1.5))
    pk <- which(u[i] < cw)[1]
    if (is.na(pk)) {
      cnt <- c(cnt, 0)
      pk <- length(cnt)
    }
    cnt[pk] <- cnt[pk] + 1
    tab[i] <- pk - 1L
  }
  expect_equal(r$table, tab)
  expect_equal(r$counts, cnt)
  expect_equal(r$expected_tables, sum(1.5 / (1.5 + 0:5)), tolerance = 1e-12)
  s <- 3
  lu <- vapply(1:4, function(i) {
    s <<- (48271 * s) %% 2147483647
    s / 2147483647
  }, 0)
  expect_equal(Crp(4, alpha = 2, seed = 3)$table, Crp(4, alpha = 2, u = lu)$table)
  expect_error(Crp(3, alpha = 0), "positive")
  expect_error(Crp(0), "at least 1")
})

test_that("Dpedt is the Blackwell-MacQueen predictive rule and EPPF", {
  p <- c("a", "b", "a", "c", "a", "b")
  r <- Dpedt(p, alpha = 2)
  expect_equal(unname(r$probs), c(3, 2, 1) / 8, tolerance = 1e-12)
  expect_equal(r$p_new, 2 / 8, tolerance = 1e-12)
  expect_equal(r$log_eppf, 3 * log(2) + lgamma(3) + lgamma(2) + lgamma(1) + lgamma(2) - lgamma(8),
               tolerance = 1e-12)
  expect_equal(r$expected_K, sum(2 / (2 + 0:5)), tolerance = 1e-12)
  expect_error(Dpedt(p, alpha = 0), "strictly positive")
  expect_error(Dpedt(character(0)), "empty")
})

test_that("Stickw uses Beta(1, alpha) quantiles at van der Corput points", {
  r <- Stickw(alpha = 2, truncation = 5)
  u <- vapply(0:4, vdc2, 0)
  V <- qbeta(u, 1, 2)
  expect_equal(r$V, V, tolerance = 1e-12)
  expect_equal(r$pi, V * cumprod(c(1, 1 - V[-5])), tolerance = 1e-12)
  expect_equal(r$remainder, prod(1 - V), tolerance = 1e-12)
  expect_equal(Stickw(V = c(0.5, 0.5))$pi, c(0.5, 0.25))
  expect_equal(Stickw(alpha = 1, truncation = 3, base = 3)$V,
               vapply(0:2, vdc2, 0, base = 3), tolerance = 1e-12)
})

test_that("Dpgem and Dpparit draw on the SplitMix64 stream", {
  r <- Dpgem(alpha = 1.5, K = 4, seed = 9)
  e <- .ghc_rng(9)
  V <- vapply(1:4, function(k) .ghc_beta1(e, 1, 1.5), 0)
  expect_equal(r$V, V, tolerance = 1e-12)
  expect_equal(r$weights, V * cumprod(c(1, 1 - V[-4])), tolerance = 1e-12)
  expect_equal(r$expected_remaining, (1.5 / 2.5)^4, tolerance = 1e-12)
  expect_error(Dpgem(alpha = 0), "strictly positive")
  expect_error(Dpgem(K = 0), "at least 1")
  py <- Dpparit(n = 12, alpha = 1, sigma = 0.3, seed = 5)
  e <- .ghc_rng(5)
  cnt <- 1
  for (i in 1:11) {
    K <- length(cnt)
    w <- c((cnt - 0.3) / (i + 1), (1 + 0.3 * K) / (i + 1))
    pk <- which(.ghc_unif(e, 1L) <= cumsum(w))[1]
    if (is.na(pk) || pk == K + 1) {
      cnt <- c(cnt, 0)
      pk <- K + 1
    }
    cnt[pk] <- cnt[pk] + 1
  }
  expect_equal(py$counts, as.integer(cnt))
  expect_equal(py$p_new, (1 + 0.3 * length(cnt)) / 13, tolerance = 1e-12)
  expect_error(Dpparit(n = 0), "at least 1")
  expect_error(Dpparit(sigma = 1), "\\[0, 1\\)")
  expect_error(Dpparit(alpha = -0.6, sigma = 0.5), "exceed -sigma")
})

test_that("Dpsing gives the exact Poisson-binomial law of the cluster count", {
  r <- Dpsing(c(1, 1, 2, 3, 3, 3, 4, 5), alpha = 1.2)
  p <- 1.2 / (1.2 + 0:7)
  pmf <- 1
  for (q in p) pmf <- c(pmf * (1 - q), 0) + c(0, pmf * q)
  expect_equal(r$E_K, sum(p), tolerance = 1e-12)
  expect_equal(r$var_K, sum(p * (1 - p)), tolerance = 1e-12)
  expect_equal(r$p_value, sum(pmf[pmf <= pmf[6] + 1e-12]), tolerance = 1e-12)
  expect_equal(r$p_normal, 2 * pnorm(-abs((5 - sum(p)) / sqrt(sum(p * (1 - p))))), tolerance = 1e-12)
  expect_equal(Dpsing(5, alpha = 1.2, n = 8)$p_value, r$p_value)
  expect_error(Dpsing(1:3, alpha = 0), "positive")
  expect_error(Dpsing(9, alpha = 1, n = 8), "1..n")
})

test_that("Dpld measures l-diversity of the sensitive attribute", {
  qid <- cbind(c(1, 1, 1, 2, 2, 2, 2), c(0, 0, 0, 5, 5, 5, 5))
  sv <- c("x", "y", "x", "x", "y", "z", "z")
  r <- Dpld(1:7, qid, sv, l = 2, c = 3)
  e1 <- -sum(c(2, 1) / 3 * log(c(2, 1) / 3))
  e2 <- -sum(c(1, 1, 2) / 4 * log(c(1, 1, 2) / 4))
  expect_equal(r$distinct_l, 2)
  expect_equal(r$min_entropy, min(e1, e2), tolerance = 1e-12)
  expect_equal(unname(r$c_min), max(2 / 1, 2 / 2), tolerance = 1e-12)
  expect_equal(c(r$satisfies_distinct, r$satisfies_recursive), c(1, 1))
  expect_equal(Dpld(1:7, qid, sv, l = 3)$c_min, Inf)
  expect_error(Dpld(numeric(0), qid, sv, 2), "empty")
  expect_error(Dpld(1:7, qid[-1, ], sv, 2), "same length")
  expect_error(Dpld(1:7, qid, sv[-1], 2), "same length")
  expect_error(Dpld(1:7, qid, sv, 0), "at least 1")
  expect_error(Dpld(1:7, qid, sv, 2, c = 0), "strictly positive")
})

test_that("Dpvar adds Laplace noise at van der Corput points", {
  skip_if_not_installed("extraDistr")
  x <- c(0.2, 1.5, -0.4, 3.2, 0.9, 2.2, -1.5)
  r <- Dpvar(x, a = -1, b = 2, epsilon = 1, seed = 3)
  cl <- pmin(pmax(x, -1), 2)
  sm <- 3 / 7 / 0.5
  s2 <- (4 - 0) / 7 / 0.5
  md <- mean(cl) + extraDistr::qlaplace(vdc2(3, 2), 0, sm)
  m2 <- mean(cl^2) + extraDistr::qlaplace(vdc2(3, 3), 0, s2)
  expect_equal(c(r$mean_dp, r$m2_dp), c(md, m2), tolerance = 1e-12)
  expect_equal(r$var_dp, min(max(m2 - md^2, 0), 2.25), tolerance = 1e-12)
  expect_equal(r$n_clamped, sum(cl != x))
  expect_error(Dpvar(numeric(0), 0, 1, 1), "empty")
  expect_error(Dpvar(x, 2, 1, 1), "a < b")
  expect_error(Dpvar(x, 0, 1, 0), "strictly positive")
  expect_error(Dpvar(x, 0, 1, 1, seed = 0), "at least 1")
})

test_that("Dpgmm runs the truncated stick-breaking EM", {
  r <- Dpgmm(dp_y, alpha = 1, truncation = 3, max_iter = 2)
  prior <- Stickw(1, 3)$pi
  prior <- prior / sum(prior)
  K <- 3
  mu <- min(dp_y) + (max(dp_y) - min(dp_y)) * (0:2 + 0.5) / 3
  sd <- rep((max(dp_y) - min(dp_y)) / 3, 3)
  w <- prior
  for (it in 1:2) {
    lp <- sapply(1:K, function(k) log(w[k]) + dnorm(dp_y, mu[k], sd[k], log = TRUE))
    R <- exp(lp - apply(lp, 1, max))
    R <- R / rowSums(R)
    eff <- colSums(R) + prior
    mu <- (colSums(R * dp_y) + prior * 0) / eff
    sd <- sqrt((colSums(R * (outer(dp_y, mu, "-"))^2) + prior * 1) / eff)
    w <- eff / sum(eff)
  }
  expect_equal(r$mu, mu, tolerance = 1e-10)
  expect_equal(r$sigma, sd, tolerance = 1e-10)
  expect_equal(r$weights, w, tolerance = 1e-10)
})

collapsed_ref <- function(y, alpha, iters, mu0, tau2, s2, seed) {
  n <- length(y)
  z <- rep(1, n)
  cnt <- n
  sm <- sum(y)
  e <- .ghc_rng(seed)
  for (it in 1:iters) {
    for (i in 1:n) {
      k <- z[i]
      cnt[k] <- cnt[k] - 1
      sm[k] <- sm[k] - y[i]
      K <- length(cnt)
      prec <- 1 / tau2 + cnt / s2
      m <- (mu0 / tau2 + sm / s2) / prec
      w <- c(ifelse(cnt > 0, cnt * exp(lnorm_pdf(y[i], m, s2 + 1 / prec)), 0),
             alpha * exp(lnorm_pdf(y[i], mu0, s2 + tau2)))
      pk <- which(.ghc_unif(e, 1L) * sum(w) <= cumsum(w))[1]
      if (is.na(pk)) pk <- K + 1
      if (pk == K + 1) {
        cnt <- c(cnt, 0)
        sm <- c(sm, 0)
      }
      z[i] <- pk
      cnt[pk] <- cnt[pk] + 1
      sm[pk] <- sm[pk] + y[i]
    }
    keep <- which(cnt > 0)
    z <- match(z, keep)
    cnt <- cnt[keep]
    sm <- sm[keep]
  }
  list(z = z, cnt = cnt, sm = sm)
}

test_that("Crpcol and Dpmem replay the collapsed Gibbs sweep", {
  s <- collapsed_ref(dp_y, 1, 3, 0, 10, 1, 4)
  r <- Crpcol(dp_y, alpha = 1, n_iter = 3, seed = 4)
  expect_equal(r$z, as.integer(s$z - 1))
  expect_equal(r$counts, as.integer(s$cnt))
  prec <- 1 / 10 + s$cnt
  means <- (s$sm) / prec
  expect_equal(r$cluster_mean, means, tolerance = 1e-12)
  expect_equal(r$loglik, sum(lnorm_pdf(dp_y, means[s$z], 1)), tolerance = 1e-12)
  expect_error(Crpcol(numeric(0)), "empty")
  expect_error(Crpcol(dp_y, alpha = 0), "strictly positive")
  expect_error(Crpcol(dp_y, tau2 = 0), "strictly positive")
  expect_error(Crpcol(dp_y, n_iter = 0), "at least 1")
  d <- Dpmem(dp_y, alpha = 2, base_distribution = c(0.5, 4), n_iter = 3, sigma2 = 0.5, seed = 6)
  s2 <- collapsed_ref(dp_y, 2, 3, 0.5, 4, 0.5, 6)
  pr <- 1 / 4 + s2$cnt / 0.5
  mm <- (0.5 / 4 + s2$sm / 0.5) / pr
  w <- s2$cnt / 11
  expect_equal(d$cluster_mean, mm, tolerance = 1e-12)
  expect_equal(d$estimate, sum(w * mm) + 2 / 11 * 0.5, tolerance = 1e-12)
  expect_error(Dpmem(numeric(0)), "empty")
  expect_error(Dpmem(dp_y, alpha = 0), "strictly positive")
  expect_error(Dpmem(dp_y, base_distribution = 1), "\\(mu0, tau2\\)")
  expect_error(Dpmem(dp_y, sigma2 = 0), "strictly positive")
})

test_that("Crpgib replays Neal's algorithm 8", {
  y <- dp_y
  n <- 9
  e <- .ghc_rng(2)
  z <- rep(1, n)
  cnt <- n
  th <- mean(y)
  for (it in 1:2) {
    for (i in 1:n) {
      k <- z[i]
      cnt[k] <- cnt[k] - 1
      K <- length(cnt)
      aux <- vapply(1:3, function(j) .ghc_norm(e, 1L, 0, sqrt(10)), 0)
      if (cnt[k] == 0) aux[1] <- th[k]
      w <- c(ifelse(cnt > 0, cnt * exp(lnorm_pdf(y[i], th, 1)), 0),
             1 / 3 * exp(lnorm_pdf(y[i], aux, 1)))
      pk <- which(.ghc_unif(e, 1L) * sum(w) <= cumsum(w))[1]
      if (is.na(pk)) pk <- K + 3
      if (pk > K) {
        th <- c(th, aux[pk - K])
        cnt <- c(cnt, 0)
        pk <- length(cnt)
      }
      z[i] <- pk
      cnt[pk] <- cnt[pk] + 1
    }
    keep <- which(cnt > 0)
    z <- match(z, keep)
    cnt <- cnt[keep]
    th <- th[keep]
    for (c in seq_along(cnt)) {
      pr <- 1 / 10 + cnt[c]
      th[c] <- .ghc_norm(e, 1L, sum(y[z == c]) / pr, sqrt(1 / pr))
    }
  }
  r <- Crpgib(y, alpha = 1, n_iter = 2, m = 3, seed = 2)
  expect_equal(r$z, as.integer(z - 1))
  expect_equal(r$theta, th, tolerance = 1e-12)
  expect_equal(r$loglik, sum(lnorm_pdf(y, th[z], 1)), tolerance = 1e-12)
  expect_error(Crpgib(numeric(0)), "empty")
  expect_error(Crpgib(y, alpha = 0), "strictly positive")
  expect_error(Crpgib(y, m = 0), "at least 1")
  expect_error(Crpgib(y, sigma2 = 0), "strictly positive")
})

test_that("Dpsbm replays the DP stochastic-block Gibbs sweep", {
  A <- matrix(0, 6, 6)
  A[1:3, 1:3] <- 1
  A[4:6, 4:6] <- 1
  A[3, 4] <- A[4, 3] <- 1
  diag(A) <- 0
  lb <- function(a, b) lgamma(a) + lgamma(b) - lgamma(a + b)
  bll <- function(z, K) {
    s <- 0
    for (r in 1:K) for (c in 1:K) {
      pr <- outer(z == r, z == c) & !diag(6)
      t <- sum(pr)
      if (t > 0) s <- s + lb(1 + sum(A[pr]), 1 + t - sum(A[pr]))
    }
    s
  }
  e <- .ghc_rng(3)
  z <- rep(1, 6)
  K <- 1
  for (it in 1:2) for (i in 1:6) {
    cnt <- vapply(1:K, function(c) sum(z[-i] == c), 0)
    cand <- which(cnt > 0)
    lw <- vapply(cand, function(c) {
      zz <- z
      zz[i] <- c
      log(cnt[c]) + bll(zz, K)
    }, 0)
    zz <- z
    zz[i] <- K + 1
    lw <- c(lw, log(1.2) + bll(zz, K + 1))
    cand <- c(cand, K + 1)
    w <- exp(lw - max(lw))
    pk <- cand[which(.ghc_unif(e, 1L) * sum(w) <= cumsum(w))[1]]
    z[i] <- pk
    z <- match(z, sort(unique(z)))
    K <- max(z)
  }
  r <- Dpsbm(A, alpha = 1.2, n_iter = 2, seed = 3)
  expect_equal(r$z, as.integer(z - 1))
  expect_equal(r$log_likelihood, bll(z, K), tolerance = 1e-12)
  expect_error(Dpsbm(matrix(numeric(0), 0, 0)), "empty")
  expect_error(Dpsbm(matrix(0, 2, 3)), "square")
  expect_error(Dpsbm(A, alpha = 0), "strictly positive")
})

test_that("morie_dpgrf flags adjacent regions that rarely co-cluster", {
  W <- matrix(0, 4, 4)
  W[1, 2] <- W[2, 1] <- W[2, 3] <- W[3, 2] <- W[3, 4] <- W[4, 3] <- 1
  draws <- list(c(0, 0, 1, 1), c(0, 0, 0, 1), c(0, 1, 1, 1), c(0, 0, 1, 1))
  r <- morie_dpgrf(W, draws, threshold = 0.4)
  co <- Reduce(`+`, lapply(draws, function(d) outer(d, d, "=="))) / 4
  pd <- c(1 - co[1, 2], 1 - co[2, 3], 1 - co[3, 4])
  expect_equal(vapply(r$ranked, `[[`, 0, "p_difference"), sort(pd, decreasing = TRUE),
               tolerance = 1e-12)
  pairs <- list(c(1L, 2L), c(2L, 3L), c(3L, 4L))
  expect_equal(r$boundaries, pairs[pd > 0.4])
  expect_equal(r$n_adjacent, 3L)
  expect_error(morie_dpgrf(matrix(0, 2, 3), draws), "not square")
  expect_error(morie_dpgrf(W + upper.tri(W), draws), "symmetric")
  expect_error(morie_dpgrf(W, list()), "no label draws")
  expect_error(morie_dpgrf(W, list(1:4, 1:3)), "differ in length")
})

test_that("morie_dpoF is the DPO loss (Bradley-Terry and Plackett-Luce)", {
  pw <- c(-1.2, -0.8, -2.0)
  pl <- c(-1.5, -0.6, -2.4)
  rw <- c(-1.3, -1.0, -1.9)
  rl <- c(-1.4, -0.9, -2.2)
  r <- morie_dpoF(pw, pl, rw, rl, beta = 0.5)
  m <- 0.5 * (pw - rw) - 0.5 * (pl - rl)
  expect_equal(r$losses, -log(plogis(m)), tolerance = 1e-12)
  expect_equal(r$loss, mean(-log(plogis(m))), tolerance = 1e-12)
  expect_equal(r$grad_weight, plogis(-m), tolerance = 1e-12)
  expect_equal(r$accuracy, mean(m > 0))
  ls <- morie_dpoF(pw, pl, rw, rl, beta = 0.5, label_smoothing = 0.1)
  expect_equal(ls$losses, -0.9 * log(plogis(m)) - 0.1 * log(plogis(-m)), tolerance = 1e-12)
  P <- rbind(c(-1, -1.5, -2), c(-0.5, -0.7, -3))
  R <- rbind(c(-1.2, -1.3, -2.1), c(-0.6, -0.6, -2.5))
  plk <- morie_dpoF(logp = P, logp_ref = R, beta = 0.3, model = "plackett-luce")
  pll <- apply(0.3 * (P - R), 1, function(rh) -sum(vapply(1:3, function(k)
    rh[k] - log(sum(exp(rh[k:3]))), 0)))
  expect_equal(plk$losses, pll, tolerance = 1e-12)
  k2 <- morie_dpoF(logp = cbind(pw, pl), logp_ref = cbind(rw, rl), beta = 0.5,
                   model = "plackett-luce")
  expect_equal(k2$losses, r$losses, tolerance = 1e-12)
  expect_error(morie_dpoF(pw, pl, rw, rl, model = "x"), "model must be")
  expect_error(morie_dpoF(pw, pl, rw, rl, beta = 0), "beta must be")
  expect_error(morie_dpoF(pw, pl, rw, rl, label_smoothing = 0.5), "label_smoothing")
  expect_error(morie_dpoF(pw, pl[-1], rw, rl), "same length")
  expect_error(morie_dpoF(numeric(0), pl, rw, rl), "non-empty")
  expect_error(morie_dpoF(model = "plackett-luce"), "needs logp")
  expect_error(morie_dpoF(logp = P, logp_ref = R[1, , drop = FALSE], model = "plackett-luce"),
               "same number of rankings")
  expect_error(morie_dpoF(logp = P[, 1, drop = FALSE], logp_ref = R[, 1, drop = FALSE],
                          model = "plackett-luce"), "K >= 2")
  expect_error(morie_dpoF(logp = P, logp_ref = R[, 1:2], model = "plackett-luce"),
               "reference entries")
})
