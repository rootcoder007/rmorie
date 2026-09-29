# Coverage for REML heritability, HTMT, R0 under heterogeneous mixing,
# hexagon binning, Hoeffding, hybrid prediction, option-value backups,
# hindsight relabelling, HITS, hitting times, Hadamard-response LDP, HLM
# shrinkage, the hierarchical normal model, HMC, profile and tagging HMMs,
# the history-adjusted MSM, the hockey-stick identity, the HP filter, HOT
# SAX discords and Getis-Ord hot spots; recomputed in the test body.

test_that("Hertbg maximises the profiled REML in h2", {
  set.seed(2)
  n <- 12
  Z <- matrix(rbinom(n * 20, 2, 0.4), n)
  Zc <- scale(Z, scale = FALSE)
  K <- Zc %*% t(Zc) / 20 + diag(0.05, n)
  y <- as.numeric(t(chol(0.6 * K + 0.4 * diag(n))) %*% rnorm(n)) + 3
  r <- Hertbg(y, K)
  reml <- function(h) {
    V <- h * K + (1 - h) * diag(n)
    Vi <- solve(V)
    s11 <- sum(Vi)
    yPy <- sum(y * (Vi %*% y)) - sum(Vi %*% y)^2 / s11
    -0.5 * (as.numeric(determinant(V)$modulus) + log(s11) + (n - 1) * log(yPy))
  }
  expect_equal(r$grid_loglik, vapply(r$grid_h2, reml, 0), tolerance = 1e-9)
  expect_equal(r$loglik, reml(r$h2), tolerance = 1e-9)
  opt <- optimize(reml, c(1e-6, 1 - 1e-6), maximum = TRUE, tol = 1e-12)
  expect_gte(r$loglik, opt$objective - 1e-9)
  V <- r$h2 * K + (1 - r$h2) * diag(n)
  Vi <- solve(V)
  tot <- (sum(y * (Vi %*% y)) - sum(Vi %*% y)^2 / sum(Vi)) / (n - 1)
  expect_equal(r$total_var, tot, tolerance = 1e-9)
  expect_equal(r$var_g, r$h2 * tot, tolerance = 1e-9)
  expect_error(Hertbg(y, K[-1, ]), "n x n")
  expect_error(Hertbg(y, K, grid = 2), "three points")
})

test_that("Hetero is the HTMT ratio", {
  set.seed(4)
  f <- matrix(rnorm(60), 30)
  X <- cbind(f[, 1] + rnorm(30, sd = 0.5), f[, 1] + rnorm(30, sd = 0.5), f[, 1] + rnorm(30, sd = 0.6),
             f[, 2] + rnorm(30, sd = 0.5), f[, 2] + rnorm(30, sd = 0.4))
  r <- Hetero(X, c(1, 1, 1, 2, 2))
  R <- abs(cor(X))
  htmt <- mean(R[1:3, 4:5]) / sqrt(mean(R[1:3, 1:3][upper.tri(diag(3))]) * R[4, 5])
  expect_equal(r$htmt, htmt, tolerance = 1e-12)
  expect_equal(r$discriminant_validity, as.integer(htmt <= 0.85))
  expect_error(Hetero(X, c(1, 1, 1, 1, 2)), "at least two indicators")
  expect_error(Hetero(X, 1:4), "one construct label")
})

test_that("Hetmix returns the spectral radius of C diag(1/gamma)", {
  C <- matrix(c(2, 0.5, 0.2, 1, 1.5, 0.3, 0.4, 0.2, 0.8), 3, byrow = TRUE)
  g <- c(1, 2, 0.5)
  r <- Hetmix(C, g)
  K <- C %*% diag(1 / g)
  e <- eigen(K)
  expect_equal(r$R0, max(Mod(e$values)), tolerance = 1e-9)
  v <- abs(Re(e$vectors[, 1]))
  expect_equal(r$stable_distribution, v / sum(v), tolerance = 1e-8)
  expect_equal(r$epidemic, as.integer(r$R0 > 1))
  expect_equal(Hetmix(diag(2), 4)$R0, 0.25, tolerance = 1e-12)
  expect_error(Hetmix(C, c(1, 2)), "one rate per group")
  expect_error(Hetmix(-C, g), "non-negative")
})

test_that("Hexgrd and hexagonal_grid bin points into the nearest hexagon centre", {
  set.seed(9)
  P <- cbind(runif(40, 0, 5), runif(40, 0, 4))
  v <- rnorm(40)
  r <- Hexgrd(P, v, cell_size = 1.2)
  # every point lies in the hexagon whose centre is nearest in the scaled metric
  sx <- (P[, 1] - min(P[, 1])) / 1.2
  sy <- (P[, 2] - min(P[, 2])) / (1.2 * sqrt(3))
  cx <- (r$centers[, 1] - min(P[, 1])) / 1.2
  cy <- (r$centers[, 2] - min(P[, 2])) / (1.2 * sqrt(3))
  for (i in 1:40) {
    d <- (sx[i] - cx)^2 + 3 * (sy[i] - cy)^2
    expect_lte(d[r$cell_id[i]], min(d) + 1e-12)
  }
  expect_equal(r$counts, tabulate(r$cell_id, nrow(r$centers)))
  expect_equal(r$value_mean, as.numeric(tapply(v, factor(r$cell_id, seq_len(nrow(r$centers))), mean)), tolerance = 1e-12)
  expect_equal(r$xcm, as.numeric(tapply(P[, 1], factor(r$cell_id, seq_len(nrow(r$centers))), mean)), tolerance = 1e-12)
  expect_equal(hexagonal_grid(P, cell_size = 1.2)$cell_id, r$cell_id)
  expect_error(Hexgrd(P, cell_size = 0), "positive")
  expect_error(Hexgrd(P, v[-1]), "match")
})

test_that("Hffdsg is Hoeffding's two-sided bound", {
  r <- Hffdsg(0, 2, 50, 0.3)
  expect_equal(r$bound, 2 * exp(-2 * 50 * 0.09 / 4), tolerance = 1e-12)
  expect_equal(r$estimate, min(1, r$bound), tolerance = 1e-12)
  expect_equal(r$t_min, 2 * sqrt(log(2) / 100), tolerance = 1e-12)
  expect_equal(Hffdsg(0, 1, 1, 0)$estimate, 1)
  expect_error(Hffdsg(1, 1, 5, 0.1), "a < b")
})

test_that("morie_hibrid_hibrid_prediction reproduces the REML fit it reports", {
  set.seed(6)
  n <- 14
  m <- 10
  P1 <- matrix(sample(c(-1, 0, 1), n * m, TRUE), n)
  P2 <- matrix(sample(c(-1, 0, 1), n * m, TRUE), n)
  bm <- rnorm(m, 0, 0.6)
  y <- as.numeric(5 + P1 %*% bm + P2 %*% bm + rnorm(n))
  Kg <- (P1 %*% t(P1) + P2 %*% t(P2)) / m
  Ks <- (P1 %*% t(P1) / m) * (P2 %*% t(P2) / m)
  reml <- function(la, ls, KS) {
    V <- Kg / la + KS / ls + diag(n)
    V <- V + diag(1e-11 * max(sum(diag(V)) / n, 1), n)
    Vi <- solve(V)
    X <- matrix(1, n, 1)
    XtViX <- sum(Vi) + 1e-11 * max(sum(Vi), 1)
    b <- sum(Vi %*% y) / XtViX
    r <- y - b
    s2e <- sum(r * (Vi %*% r)) / (n - 1)
    list(ll = -0.5 * ((n - 1) * log(s2e) + as.numeric(determinant(V)$modulus) + log(XtViX) + n - 1),
         b = b, s2e = s2e, Vi = Vi)
  }
  a <- morie_hibrid_hibrid_prediction(y, P1, P2, sigma2_sca = 0)
  la <- a$sigma2_e / a$sigma2_gca
  f <- reml(la, 1e300, 0 * Ks)
  expect_equal(a$reml_loglik, f$ll, tolerance = 1e-8)
  expect_equal(a$coefficients, f$b, tolerance = 1e-8)
  expect_equal(a$gca_effect, as.numeric(a$sigma2_gca * Kg %*% f$Vi %*% (y - f$b)), tolerance = 1e-8)
  expect_equal(a$sca_effect, rep(0, n))
  expect_equal(a$h2, a$sigma2_gca / (a$sigma2_gca + a$sigma2_e), tolerance = 1e-12)
  expect_gt(log(la), -14 + 0.1)
  expect_lt(log(la), 14 - 0.1)
  expect_gte(a$reml_loglik, reml(la * 1.05, 1e300, 0 * Ks)$ll - 1e-9)
  expect_gte(a$reml_loglik, reml(la / 1.05, 1e300, 0 * Ks)$ll - 1e-9)
  b <- morie_hibrid_hibrid_prediction(y, P1, P2, max_iter = 20, p1_new = P1[1:2, ], p2_new = P2[1:2, ])
  f2 <- reml(b$sigma2_e / b$sigma2_gca, b$sigma2_e / b$sigma2_sca, Ks)
  expect_equal(b$reml_loglik, f2$ll, tolerance = 1e-8)
  w <- f2$Vi %*% (y - f2$b)
  expect_equal(b$prediction_new, as.numeric(f2$b + b$sigma2_gca * Kg[1:2, ] %*% w + b$sigma2_sca * Ks[1:2, ] %*% w),
               tolerance = 1e-8)
  expect_equal(b$prediction_new, b$fitted[1:2], tolerance = 1e-8)
  expect_error(morie_hibrid_hibrid_prediction(y[-1], P1, P2), "phenotypes")
  expect_error(morie_hibrid_hibrid_prediction(y, P1, P2[, -1]), "same")
})

test_that("Optionshrl performs the SMDP option backup", {
  R <- c(1, 0.5, 2, -1)
  r <- Optionshrl(R, gamma = 0.9, alpha = 0.2, Q = 1, q_next = c(0.5, 3), k_steps = 3)
  ro <- 1 + 0.9 * 0.5 + 0.81 * 2
  expect_equal(r$r_option, ro, tolerance = 1e-12)
  expect_equal(r$target, ro + 0.9^3 * 3, tolerance = 1e-12)
  expect_equal(r$estimate, 1 + 0.2 * (ro + 0.729 * 3 - 1), tolerance = 1e-12)
  vdc <- function(i) {
    k <- i + 1
    f <- 1
    s <- 0
    while (k > 0) {
      f <- f / 2
      s <- s + f * (k %% 2)
      k <- k %/% 2
    }
    s
  }
  beta <- c(0.1, 0.9, 0.9, 0.9)
  kk <- which(vapply(0:3, vdc, 0) < beta)[1]
  o <- Optionshrl(R, options = beta, gamma = 0.9)
  expect_equal(o$k, kk)
  expect_equal(o$r_option, sum(0.9^(0:(kk - 1)) * R[1:kk]), tolerance = 1e-12)
  expect_equal(Optionshrl(R, gamma = 0.5)$k, 4L)
})

test_that("hindsr relabels transitions with future, final, episode and random goals", {
  ep <- list(list(0, 1, 2, 3), list(5, 4, 4))
  f <- hindsr(ep, strategy = "final", k = 1)
  expect_equal(f$n_transitions, 2 * 5)
  expect_equal(f$n_relabelled, 5L)
  rw <- vapply(f$transitions, function(t) t$reward, 0)
  gl <- vapply(f$transitions, function(t) t$goal, 0)
  ns <- vapply(f$transitions, function(t) t$next_state, 0)
  expect_equal(rw, ifelse(abs(ns - gl) <= 1e-6, 0, -1))
  expect_true(all(gl == rep(c(3, 3, 3, 3, 3, 3, 4, 4, 4, 4), 1)))
  e <- .ghc_rng(3)
  fu <- hindsr(ep, strategy = "future", k = 2, seed = 3)
  goals <- list()
  for (i in 1:2) {
    T <- length(ep[[i]]) - 1
    for (t in 1:T) {
      idx <- (t + 1) + floor(.ghc_unif(e, 2) * (T + 1 - t))
      goals <- c(goals, list(unlist(ep[[i]])[idx]))
    }
  }
  rel <- Filter(function(t) t$relabelled, fu$transitions)
  expect_equal(vapply(rel, function(t) t$goal, 0), unlist(goals))
  expect_equal(fu$success_rate, mean(fu$rewards > -1 + 1e-12), tolerance = 1e-12)
  ra <- hindsr(ep, strategy = "random", k = 1, history = list(7, 8))
  expect_true(all(vapply(Filter(function(t) t$relabelled, ra$transitions), function(t) t$goal, 0) %in% c(7, 8)))
  expect_error(hindsr(ep, strategy = "best"), "strategy must be")
  expect_error(hindsr(list(list(1))), "at least s_0 and s_1")
  expect_error(hindsr(ep, actions = list(1:3)), "action sequences")
})

test_that("Hits and morie_hits are Kleinberg's normalised power iteration", {
  A <- matrix(0, 5, 5)
  A[1, c(2, 3)] <- 1
  A[2, 3] <- 1
  A[4, c(1, 2, 3, 5)] <- 1
  A[5, 3] <- 1
  e <- eigen(A %*% t(A), symmetric = TRUE)
  hub <- abs(e$vectors[, 1])
  auth <- abs(eigen(t(A) %*% A, symmetric = TRUE)$vectors[, 1])
  r <- Hits(A, iters = 200)
  expect_equal(r$hubs, hub, tolerance = 1e-9)
  expect_equal(r$authorities, auth, tolerance = 1e-9)
  expect_equal(r$top_hub, which.max(hub))
  m <- morie_hits(A, iters = 200)
  expect_equal(m$hubs, hub, tolerance = 1e-9)
  expect_equal(morie_hits(A, 1)$authorities, as.numeric(t(A) %*% rep(1, 5)) / sqrt(sum((t(A) %*% rep(1, 5))^2)),
               tolerance = 1e-12)
  expect_error(Hits(A[1:2, ]), "square")
  expect_error(morie_hits(A, iters = 0), "iters")
})

test_that("Hittime solves the first-step equations", {
  W <- matrix(c(0, 1, 1, 0,
                1, 0, 1, 1,
                1, 1, 0, 1,
                0, 1, 1, 0), 4, byrow = TRUE)
  r <- Hittime(W, start = 3, target = 0)
  P <- W / rowSums(W)
  Q <- P[2:4, 2:4]
  h <- solve(diag(3) - Q, rep(1, 3))
  expect_equal(r$hitting, c(0, h), tolerance = 1e-12)
  expect_equal(r$estimate, h[3], tolerance = 1e-12)
  W2 <- W
  W2[4, ] <- 0
  W2[2, 4] <- W2[3, 4] <- 0
  r2 <- Hittime(W2, target = 0)
  expect_equal(r2$hitting[4], Inf)
  expect_error(Hittime(W, target = 4), "target out of range")
  expect_error(Hittime(-W), "non-negative")
})

test_that("Ldphr rescales the Hadamard-response frequencies", {
  r <- Ldphr(c(30, 55, 15), epsilon = 1)
  sc <- 2 * (exp(1) + 1) / (exp(1) - 1)
  expect_equal(r$p, sc * (c(30, 55, 15) / 100 - 0.5), tolerance = 1e-12)
  expect_equal(Ldphr(c(3, 5), 2, n = 20)$p_set, c(0.15, 0.25), tolerance = 1e-12)
  expect_error(Ldphr(1, 0), "epsilon")
})

test_that("morie_hlmgr and Hiermodel shrink toward the grand mean", {
  B <- rbind(c(1, 0.5), c(2, 0.1), c(0.5, 0.9), c(1.8, 0.2), c(1.2, 0.6))
  V <- lapply(1:5, function(j) diag(c(0.1, 0.02) * j / 3))
  r <- morie_hlmgr(B, V)
  raw <- cov(B) - Reduce(`+`, V) / 5
  e <- eigen(raw, symmetric = TRUE)
  tau <- e$vectors %*% diag(pmax(e$values, 0)) %*% t(e$vectors)
  expect_equal(r$tau, tau, tolerance = 1e-12)
  lam3 <- tau %*% solve(tau + V[[3]])
  expect_equal(r$shrunken[3, ], as.numeric(lam3 %*% B[3, ] + (diag(2) - lam3) %*% colMeans(B)), tolerance = 1e-12)
  expect_error(morie_hlmgr(B[1:2, ]), "three groups")
  expect_error(morie_hlmgr(B, V[1:2]), "one V_j")

  y <- c(28, 8, -3, 7, -1, 1, 18, 12)
  s <- c(15, 10, 16, 11, 9, 11, 10, 18)
  h <- Hiermodel(y, s, tau = 5)
  w <- 1 / (s^2 + 25)
  mu <- sum(w * y) / sum(w)
  expect_equal(h$mu_hat, mu, tolerance = 1e-12)
  expect_equal(h$theta_hat, (y / s^2 + mu / 25) / (1 / s^2 + 1 / 25), tolerance = 1e-12)
  expect_equal(h$log_post_tau, 0.5 * log(1 / sum(w)) - 0.5 * sum(log(s^2 + 25) + (y - mu)^2 * w), tolerance = 1e-12)
  expect_equal(Hiermodel(y, s, 0)$theta_hat, rep(sum(y / s^2) / sum(1 / s^2), 8), tolerance = 1e-12)
  expect_error(Hiermodel(y, -s, 1), "positive")
})

test_that("Hmcsam runs leapfrog with van der Corput momenta and a base-3 acceptance draw", {
  vdc <- function(i, b) {
    k <- i + 1
    f <- 1
    s <- 0
    while (k > 0) {
      f <- f / b
      s <- s + f * (k %% b)
      k <- k %/% b
    }
    s
  }
  lp <- function(x) -0.5 * sum(x^2 / c(1, 4))
  gr <- function(x) -x / c(1, 4)
  r <- Hmcsam(lp, gr, c(1, -1), step_size = 0.3, L = 5, n_iter = 6)
  x <- c(1, -1)
  cnt <- 1
  D <- matrix(0, 6, 2)
  acc <- 0
  for (s in 1:6) {
    p <- qnorm(vapply(cnt + 0:1, vdc, 0, b = 2))
    cnt <- cnt + 2
    H0 <- -lp(x) + 0.5 * sum(p^2)
    xn <- x
    pn <- p
    for (l in 1:5) {
      pn <- pn + 0.15 * gr(xn)
      xn <- xn + 0.3 * pn
      pn <- pn + 0.15 * gr(xn)
    }
    H1 <- -lp(xn) + 0.5 * sum(pn^2)
    u <- vdc(cnt, 3)
    cnt <- cnt + 1
    if (H1 <= H0 || u < exp(H0 - H1)) {
      x <- xn
      acc <- acc + 1
    }
    D[s, ] <- x
  }
  expect_equal(r$draws, D, tolerance = 1e-12)
  expect_equal(r$accept_rate, acc / 6, tolerance = 1e-12)
  expect_error(Hmcsam(lp, gr, 1, step_size = 0), "step_size")
  expect_error(Hmcsam(lp, 1, 1), "callable")
})

test_that("Hmmprf and HmmTag agree with brute-force enumeration", {
  prof <- list(match = matrix(c(0.7, 0.3, 0.2, 0.8), 2, byrow = TRUE), insert = c(0.5, 0.5),
               trans = list(mm = 0.8, mi = 0.1, md = 0.1, im = 0.6, ii = 0.4, dm = 0.7, dd = 0.3))
  r <- Hmmprf(c(0, 1), prof)
  # the only paths emitting 2 residues through 2 positions: MM, MI..., enumerate the small lattice
  # forward is the log of the total path probability; with 2 residues and 2 match columns every
  # path ends in state (2, 2): enumerate by dynamic programming in probability space
  tr <- prof$trans
  fM <- fI <- fD <- matrix(0, 3, 3)
  fM[1, 1] <- 1
  for (i in 1:3) for (j in 1:3) {
    if (i == 1 && j == 1) next
    if (i > 1 && j > 1) fM[i, j] <- (fM[i - 1, j - 1] * tr$mm + fI[i - 1, j - 1] * tr$im + fD[i - 1, j - 1] * tr$dm) *
      prof$match[j - 1, c(0, 1)[i - 1] + 1]
    if (i > 1) fI[i, j] <- (fM[i - 1, j] * tr$mi + fI[i - 1, j] * tr$ii) * prof$insert[c(0, 1)[i - 1] + 1]
    if (j > 1) fD[i, j] <- fM[i, j - 1] * tr$md + fD[i, j - 1] * tr$dd
  }
  expect_equal(r$forward_logprob, log(fM[3, 3] + fI[3, 3] + fD[3, 3]), tolerance = 1e-12)
  expect_equal(r$log_odds, r$forward_logprob - 2 * log(0.5), tolerance = 1e-12)
  expect_lte(r$viterbi_logprob, r$forward_logprob)
  expect_equal(r$viterbi_logprob, log(0.8 * 0.7 * 0.8 * 0.8), tolerance = 1e-12)
  expect_error(Hmmprf(2, prof), "out of range")

  st <- c(0.6, 0.4)
  A <- matrix(c(0.7, 0.3, 0.4, 0.6), 2, byrow = TRUE)
  B <- matrix(c(0.5, 0.4, 0.1, 0.1, 0.3, 0.6), 2, byrow = TRUE)
  x <- c(0, 2, 1, 2)
  paths <- as.matrix(expand.grid(rep(list(1:2), 4)))
  lpp <- apply(paths, 1, function(p) log(st[p[1]]) + sum(log(A[cbind(p[-4], p[-1])])) + sum(log(B[cbind(p, x + 1)])))
  h <- HmmTag(x, c("N", "V"), st, A, B)
  expect_equal(h$logprob, max(lpp), tolerance = 1e-12)
  expect_equal(h$path, as.integer(paths[which.max(lpp), ]))
  expect_error(HmmTag(x, c("N", "V")), "must be supplied")
})

test_that("Hmstrn fits IP-weighted regime means over time", {
  set.seed(12)
  n <- 60
  L <- matrix(rnorm(n * 2), n)
  A <- cbind(rbinom(n, 1, plogis(L[, 1])), rbinom(n, 1, plogis(L[, 2])))
  y <- 1 + A[, 1] + A[, 2] + rnorm(n)
  r <- Hmstrn(y, A, L, time = c(1, 2), regime = c(1, 1))
  mm <- numeric(2)
  for (t in 1:2) {
    idx <- which(apply(A[, 1:t, drop = FALSE] == 1, 1, all))
    fit <- glm(A[idx, t] ~ L[idx, t], family = binomial)
    g <- pmin(pmax(fitted(fit), 0.025), 0.975)
    w <- ifelse(A[idx, t] == 1, 1 / g, 1 / (1 - g))
    mm[t] <- sum(w * y[idx]) / sum(w)
  }
  # the internal IRLS carries a 1e-8 ridge, so glm() agrees to about 1e-7
  expect_equal(r$by_time, mm, tolerance = 1e-6)
  expect_equal(r$estimate, mm[2] - mm[1], tolerance = 1e-6)
  expect_equal(r$n_consistent, sum(A[, 1] == 1 & A[, 2] == 1))
})

test_that("HockStick, Hodprc, morie_hot, Hotcld and hot_cold_spots", {
  hs <- HockStick(9, 3)
  expect_equal(hs$stick_sum, sum(choose(2:8, 2)))
  expect_equal(hs$closed_form, choose(9, 3))
  expect_equal(hs$n_terms, 7)

  y <- c(1, 3, 2, 5, 4, 6, 8, 7, 9)
  hp <- Hodprc(y, lam = 10)
  D <- diff(diag(9), differences = 2)
  tau <- solve(diag(9) + 10 * t(D) %*% D, y)
  expect_equal(hp$trend, as.numeric(tau), tolerance = 1e-9)
  expect_equal(hp$roughness, sum((D %*% tau)^2), tolerance = 1e-9)
  expect_equal(hp$estimate, sum((y - tau)^2) + 10 * sum((D %*% tau)^2), tolerance = 1e-9)
  expect_error(Hodprc(1:2), "three observations")

  x <- sin(seq(0, 6 * pi, length.out = 60))
  x[33:36] <- c(1.5, -1.2, 1.4, -1.1)
  h <- morie_hot(x, window = 6)
  zn <- function(s) if (sqrt(mean((s - mean(s))^2)) < 1e-12) 0 * s else (s - mean(s)) / sqrt(mean((s - mean(s))^2))
  S <- lapply(1:55, function(p) zn(x[p:(p + 5)]))
  nn <- vapply(1:55, function(p) {
    q <- which(abs(1:55 - p) >= 6)
    min(vapply(q, function(k) sqrt(sum((S[[p]] - S[[k]])^2)), 0))
  }, 0)
  expect_equal(h$distance, max(nn), tolerance = 1e-12)
  expect_equal(h$location, which.max(nn))
  expect_error(morie_hot(x, window = 40), "window")

  W <- matrix(0, 6, 6)
  for (i in 1:5) W[i, i + 1] <- W[i + 1, i] <- 1
  diag(W) <- 1
  v <- c(10, 9, 8, 1, 2, 1)
  g <- Hotcld(v, W, alpha = 0.3)
  s <- sqrt(mean((v - mean(v))^2))
  z <- vapply(1:6, function(i) (sum(W[i, ] * v) - mean(v) * sum(W[i, ])) /
                (s * sqrt((6 * sum(W[i, ]^2) - sum(W[i, ])^2) / 5)), 0)
  expect_equal(g$z, z, tolerance = 1e-12)
  p <- 2 * pnorm(-abs(z))
  sig <- p.adjust(p, "BH") <= 0.3
  expect_equal(g$significant, sig)
  expect_equal(g$n_hot, sum(sig & z > 0))
  expect_equal(hot_cold_spots(v, W, 0.3)$category, g$category)
  expect_error(Hotcld(v, W[1:5, ]), "n by n")
})
