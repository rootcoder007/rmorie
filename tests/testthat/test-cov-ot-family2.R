# Coverage for otmaprc .. Outmad exports. Every expectation is recomputed
# in the test body: Sinkhorn by plain matrix scaling, exact transport by
# enumerating permutations (uniform marginals of equal size), projections
# from the van der Corput / inverse-normal construction the sliced
# distances document.

cov_ot2_scaling <- function(a, b, C, eps, iters) {
  K <- exp(-C / eps)
  v <- rep(1, length(b))
  for (it in seq_len(iters)) {
    u <- a / as.numeric(K %*% v)
    v <- b / as.numeric(crossprod(K, u))
  }
  u * K * rep(v, each = length(a))
}

cov_ot2_perms <- function(n) {
  if (n == 1) return(matrix(1L, 1, 1))
  p <- cov_ot2_perms(n - 1)
  do.call(rbind, lapply(seq_len(n), function(i) cbind(i, ifelse(p >= i, p + 1L, p))))
}

cov_ot2_assign <- function(C) {
  P <- cov_ot2_perms(nrow(C))
  costs <- apply(P, 1, function(p) sum(C[cbind(seq_along(p), p)]))
  list(cost = min(costs) / nrow(C), perm = P[which.min(costs), ])
}

cov_ot2_sqd <- function(A, B) {
  outer(seq_len(nrow(A)), seq_len(nrow(B)), Vectorize(function(i, j) sum((A[i, ] - B[j, ])^2)))
}

cov_ot2_dirs <- function(d, L) {
  vdc <- function(k) {
    x <- 0
    f <- 0.5
    while (k > 0) {
      x <- x + f * (k %% 2)
      k <- k %/% 2
      f <- f / 2
    }
    x
  }
  M <- matrix(stats::qnorm(vapply(seq_len(d * L), vdc, 0)), L, d, byrow = TRUE)
  M / sqrt(rowSums(M^2))
}

X2 <- cbind(c(0, 1, 2, 3.5), c(1, 0, 2, 1.2))
Y2 <- cbind(c(0.3, 2.2, 1.1, 4), c(0.5, 1.9, -0.4, 2.2))

test_that("Brenier1d is the monotone rearrangement", {
  x <- c(3, 1, 2, 5)
  y <- c(10, 40, 20, 30)
  r <- Brenier1d(x, y)
  expect_equal(r$map, c(30, 10, 20, 40))
  expect_equal(r$cost, mean((x - c(30, 10, 20, 40))^2), tolerance = 1e-12)
  expect_equal(r$order_x, order(x) - 1L)
  expect_equal(Brenier1d(x, y, p = 1)$cost, mean(abs(x - c(30, 10, 20, 40))), tolerance = 1e-12)
  expect_error(Brenier1d(x, y[-1]), "same length")
})

test_that("Otpr and Otmarsh transport a reduced amount of mass", {
  C <- matrix(c(1, 3, 2, 5), 2, byrow = TRUE)
  a <- c(0.5, 0.5)
  b <- c(0.5, 0.5)
  # half the mass: the cheapest cell alone can carry it
  p <- Otpr(a, b, C, 0.5)
  expect_equal(p$cost, 0.5 * min(C), tolerance = 1e-12)
  expect_equal(p$mass, 0.5, tolerance = 1e-12)
  expect_equal(p$a_left, a - rowSums(p$T), tolerance = 1e-12)
  expect_equal(Otpr(a, b, C, 1)$cost, cov_ot2_assign(C)$cost, tolerance = 1e-12)
  expect_error(Otpr(a, b, C, 2), "transported mass")
  expect_error(Otpr(a, b, C[, 1, drop = FALSE], 0.5), "does not match")
  # delta = 0.5 leaves (0.25, 0.25); both rows prefer column 1, which holds 0.5
  m <- Otmarsh(a, b, C, 0.5)
  expect_equal(m$a_shift, c(0.25, 0.25))
  expect_equal(m$cost, 0.25 * 1 + 0.25 * 2, tolerance = 1e-12)
  expect_equal(m$removed, 0.5)
  mv <- Otmarsh(a, b, C, c(0.5, 0))
  expect_equal(mv$cost, 0.5 * 2, tolerance = 1e-12)
  expect_error(Otmarsh(a, b, C, c(0.6, 0)), "more mass than a bin")
  expect_error(Otmarsh(a, b, C, c(0.1, 0.1, 0.1)), "scalar or one value")
  expect_error(Otmarsh(c(0, 0), b, C, 0.1), "no mass to remove")
})

test_that("Otmcluster assigns clouds to Wasserstein k-means centres", {
  cl <- list(cbind(c(0, 1), c(0, 0)), cbind(c(0.2, 1.1), c(0.1, -0.1)),
             cbind(c(5, 6), c(5, 5)), cbind(c(5.3, 6.2), c(4.9, 5.2)))
  r <- Otmcluster(cl, k = 2, max_iter = 1)
  # initial centres are clouds 1 and 2; cloud 1 and 2 cost 0 to themselves
  cost <- function(A, B) cov_ot2_assign(cov_ot2_sqd(A, B))$cost
  lab <- vapply(cl, function(X) which.min(c(cost(cl[[1]], X), cost(cl[[2]], X))) - 1L, 0L)
  expect_equal(r$labels, lab)
  inert <- sum(vapply(cl, function(X) min(cost(cl[[1]], X), cost(cl[[2]], X)), 0))
  expect_equal(r$inertia, inert, tolerance = 1e-12)
  # one cluster, one step: the centre is the mean of the matched points
  r1 <- Otmcluster(cl, k = 1, max_iter = 1)
  Z <- Reduce(`+`, lapply(cl, function(X) X[cov_ot2_assign(cov_ot2_sqd(cl[[1]], X))$perm, ])) / 4
  expect_equal(r1$centers[[1]], Z, tolerance = 1e-12)
  expect_equal(r1$labels, rep(0L, 4))
  expect_error(Otmcluster(list(), 1), "no input clouds")
  expect_error(Otmcluster(cl, 5), "k must lie")
  expect_error(Otmcluster(list(cl[[1]], cl[[1]][1, , drop = FALSE]), 1), "same shape")
})

test_that("Otmd uses the Mahalanobis ground cost", {
  S <- matrix(c(2, 0.5, 0.5, 1), 2)
  r <- Otmd(X2, Y2, S)
  Si <- solve(S)
  C <- outer(1:4, 1:4, Vectorize(function(i, j) {
    dv <- X2[i, ] - Y2[j, ]
    sum(dv * (Si %*% dv))
  }))
  expect_equal(r$C, C, tolerance = 1e-12)
  expect_equal(r$cost, cov_ot2_assign(C)$cost, tolerance = 1e-12)
  expect_error(Otmd(X2, Y2, diag(3)), "Sigma must be d by d")
})

test_that("Otmm averages entropic costs over cyclic minibatches", {
  r <- Otmm(X2, Y2[1:3, ], batch_size = 2, n_batches = 3, epsilon = 0.5)
  per <- vapply(0:2, function(b) {
    ix <- ((b * 2 + 0:1) %% 4) + 1
    iy <- ((b * 2 + 0:1) %% 3) + 1
    C <- cov_ot2_sqd(X2[ix, ], Y2[iy, ])
    sum(cov_ot2_scaling(c(0.5, 0.5), c(0.5, 0.5), C, 0.5, 200) * C)
  }, 0)
  expect_equal(r$per_batch, per, tolerance = 1e-10)
  expect_equal(r$loss, mean(per), tolerance = 1e-10)
  expect_error(Otmm(X2, Y2, 5, 1, 0.5), "exceeds a cloud")
  expect_error(Otmm(X2, Y2, 0, 1, 0.5), "must be positive")
  expect_error(Otmm(X2, Y2[, 1, drop = FALSE], 1, 1, 0.5), "share a dimension")
})

test_that("Otmot with two margins is ordinary Sinkhorn and fits three margins", {
  a <- c(0.3, 0.7)
  b <- c(0.2, 0.5, 0.3)
  C <- outer(c(0, 1), c(0, 0.5, 2), "-")^2
  r <- Otmot(list(a, b), as.numeric(t(C)), 0.4, max_iter = 150)
  Tm <- cov_ot2_scaling(a, b, C, 0.4, 150)
  expect_equal(r$T, as.numeric(t(Tm)), tolerance = 1e-12)
  expect_equal(r$cost, sum(Tm * C), tolerance = 1e-12)
  c3 <- c(0.5, 0.5)
  Ct <- array(0, c(2, 3, 2))
  for (i in 1:2) for (j in 1:3) for (k in 1:2) Ct[i, j, k] <- (i - j)^2 + (j - k)^2 + 0.1 * i * k
  # the function reads the tensor with the last index fastest
  r3 <- Otmot(list(a, b, c3), as.numeric(aperm(Ct, 3:1)), 0.5, max_iter = 300)
  P <- aperm(array(r3$T, c(2, 3, 2)), 3:1)
  expect_equal(apply(P, 1, sum), a, tolerance = 1e-9)
  expect_equal(apply(P, 2, sum), b, tolerance = 1e-9)
  expect_equal(apply(P, 3, sum), c3, tolerance = 1e-9)
  expect_equal(r3$cost, sum(P * Ct), tolerance = 1e-12)
  expect_error(Otmot(list(a), 1, 0.4), "at least two margins")
  expect_error(Otmot(list(a, b), 1:5, 0.4), "does not match")
  expect_error(Otmot(list(a, b), as.numeric(C), 0), "epsilon must be positive")
})

test_that("Otmqd: nearest-centroid assignment is the optimal transport", {
  cen <- rbind(c(0, 0), c(3, 2))
  r <- Otmqd(X2, cen)
  C <- cov_ot2_sqd(X2, cen)
  lab <- apply(C, 1, which.min)
  expect_equal(r$labels, lab - 1L)
  expect_equal(r$dist_assign, mean(C[cbind(1:4, lab)]), tolerance = 1e-12)
  expect_equal(r$weights, tabulate(lab, 2) / 4)
  expect_equal(r$dist, r$dist_assign, tolerance = 1e-12)
  expect_error(Otmqd(X2, cen[, 1, drop = FALSE]), "same space")
  expect_error(Otmqd(X2, cen[0, , drop = FALSE]), "empty")
})

test_that("Otsw, Otmsw and Otsd average one-dimensional distances over projections", {
  L <- 6
  TH <- cov_ot2_dirs(2, L)
  per <- vapply(1:L, function(k) mean((sort(X2 %*% TH[k, ]) - sort(Y2 %*% TH[k, ]))^2), 0)
  r <- Otsw(X2, Y2, n_proj = L)
  expect_equal(r$per_proj, per, tolerance = 1e-12)
  expect_equal(r$SW, sqrt(mean(per)), tolerance = 1e-12)
  # a projection is 1-Lipschitz, so no slice exceeds the full W_2^2
  expect_true(all(per <= cov_ot2_assign(cov_ot2_sqd(X2, Y2))$cost + 1e-12))
  m <- Otmsw(X2, Y2, n_proj = L)
  expect_equal(m$per_proj, sqrt(per), tolerance = 1e-12)
  expect_equal(m$MSW, sqrt(max(per)), tolerance = 1e-12)
  expect_equal(m$theta_star, TH[which.max(per), ], tolerance = 1e-12)
  s <- Otsd(X2, Y2[1:3, ], n_proj = L, p = 1)
  g <- (1:4 - 0.5) / 4
  pq <- vapply(1:L, function(k) {
    mean(abs(stats::quantile(X2 %*% TH[k, ], g, type = 7, names = FALSE) -
               stats::quantile(Y2[1:3, ] %*% TH[k, ], g, type = 7, names = FALSE)))
  }, 0)
  expect_equal(s$per_proj, pq, tolerance = 1e-12)
  expect_equal(s$SW, mean(pq), tolerance = 1e-12)
  # in one dimension the only direction is 1
  x <- c(0.5, 2, 1, 4)
  y <- c(3, 1.5, 0, 2.5)
  expect_equal(Otsw(cbind(x), cbind(y), n_proj = 3)$SW, sqrt(mean((sort(x) - sort(y))^2)),
               tolerance = 1e-12)
  expect_error(Otsw(X2, Y2[1:3, ]), "equal point counts")
  expect_error(Otmsw(X2, Y2[1:3, ]), "equal point counts")
  expect_error(Otsd(X2, Y2, p = 0), "p must be positive")
  expect_error(Otsd(X2, Y2[, 1, drop = FALSE]), "share a dimension")
})

test_that("Rasscale is RAS biproportional fitting", {
  K <- matrix(c(2, 1, 0, 1, 3, 1, 4, 0, 2), 3)
  r <- c(5, 3, 4)
  cc <- c(6, 2, 4)
  res <- Rasscale(K, r, cc, max_iter = 400)
  v <- rep(1, 3)
  for (i in 1:400) {
    u <- r / as.numeric(K %*% v)
    v <- cc / as.numeric(crossprod(K, u))
  }
  expect_equal(res$M, u * K * rep(v, each = 3), tolerance = 1e-12)
  expect_lt(res$row_error, 1e-9)
  expect_lt(res$col_error, 1e-12)
  expect_error(Rasscale(K, r, cc + 1), "same total")
  expect_error(Rasscale(-K, r, cc), "non-negative")
  expect_error(Rasscale(K, r[-1], cc), "match the shape")
})

test_that("Otmxh and Otwsg use the Gaussian (Bures) W2 cost", {
  sqrtm <- function(S) {
    e <- eigen(S, symmetric = TRUE)
    e$vectors %*% diag(sqrt(pmax(e$values, 0))) %*% t(e$vectors)
  }
  w2 <- function(m1, S1, m2, S2) {
    R <- sqrtm(S1)
    sum((m1 - m2)^2) + sum(diag(S1)) + sum(diag(S2)) - 2 * sum(diag(sqrtm(R %*% S2 %*% R)))
  }
  S1 <- matrix(c(2, 0.3, 0.3, 1), 2)
  S2 <- matrix(c(1, -0.2, -0.2, 0.5), 2)
  g <- Otwsg(c(0, 1), S1, c(1, -1), S2)
  expect_equal(g$W2_sq, w2(c(0, 1), S1, c(1, -1), S2), tolerance = 1e-10)
  expect_equal(g$mean_part, 5)
  expect_equal(g$bures_sq, g$W2_sq - 5, tolerance = 1e-12)
  # diagonal covariances: Bures reduces to sum (sqrt s1 - sqrt s2)^2
  d <- Otwsg(c(0, 0), diag(c(4, 1)), c(0, 0), diag(c(1, 9)))
  expect_equal(d$W2_sq, (2 - 1)^2 + (1 - 3)^2, tolerance = 1e-10)
  mus1 <- rbind(c(0, 0), c(3, 1))
  mus2 <- rbind(c(0.5, 0), c(2, 2))
  Sl1 <- list(S1, diag(2))
  Sl2 <- list(S2, diag(c(0.5, 2)))
  p <- c(0.3, 0.7)
  q <- c(0.6, 0.4)
  m <- Otmxh(mus1, Sl1, p, mus2, Sl2, q)
  C <- outer(1:2, 1:2, Vectorize(function(k, l) w2(mus1[k, ], Sl1[[k]], mus2[l, ], Sl2[[l]])))
  expect_equal(m$C, C, tolerance = 1e-10)
  # 2 x 2 transport: T11 ranges over [max(0, p1 - q2), min(p1, q1)], cost linear in it
  tc <- function(t) t * C[1, 1] + (p[1] - t) * C[1, 2] + (q[1] - t) * C[2, 1] + (p[2] - q[1] + t) * C[2, 2]
  best <- min(tc(max(0, p[1] - q[2])), tc(min(p[1], q[1])))
  expect_equal(m$MW2_sq, best, tolerance = 1e-10)
  expect_equal(m$MW2, sqrt(best), tolerance = 1e-10)
  expect_error(Otmxh(mus1, Sl1, p, mus2[, 1, drop = FALSE], Sl2, q), "same dimension")
  expect_error(Otmxh(mus1, Sl1[1], p, mus2, Sl2, q), "agree in count")
})

test_that("Sinkhlog, Sinkhwarm and Otreg solve the entropic problem", {
  a <- c(0.2, 0.5, 0.3)
  b <- c(0.4, 0.4, 0.2)
  C <- outer(c(0, 1, 2), c(0.5, 1.5, 2.5), "-")^2
  eps <- 0.3
  Tm <- cov_ot2_scaling(a, b, C, eps, 2000)
  s <- Sinkhlog(a, b, C, eps, max_iter = 2000)
  expect_equal(s$T, Tm, tolerance = 1e-10)
  expect_equal(s$cost, sum(Tm * C), tolerance = 1e-10)
  expect_lt(s$err, 1e-13)
  expect_lt(s$n_iter, 2000L)
  expect_equal(s$T, exp((outer(s$f, s$g, "+") - C) / eps), tolerance = 1e-12)
  w <- Sinkhwarm(a, b, C, eps, f0 = s$f, g0 = s$g, max_iter = 2000)
  expect_equal(w$T, Tm, tolerance = 1e-10)
  expect_equal(w$u, exp(w$f / eps), tolerance = 1e-12)
  expect_equal(w$n_iter_cold, s$n_iter)
  expect_equal(w$saved, s$n_iter - w$n_iter)
  expect_lte(w$n_iter, 2L)
  o <- Otreg(a, b, C, eps, max_iter = 300)
  T3 <- cov_ot2_scaling(a, b, C, eps, 300)
  expect_equal(o$primal_cost, sum(T3 * C), tolerance = 1e-10)
  expect_equal(o$dual_value, sum(a * o$f) + sum(b * o$g) - eps * sum(T3), tolerance = 1e-10)
  expect_error(Otreg(a, b, C[, 1:2], eps), "does not match")
})

test_that("Ototk and Otsobm evaluate their closed forms", {
  a <- c(0.2, 0.3, 0.5)
  b <- c(0.1, 0.9)
  r <- Ototk(a, b, c(1, -2, 0.5), c(3, 0.25))
  expect_equal(r$dual_val, sum(a * c(1, -2, 0.5)) + sum(b * c(3, 0.25)), tolerance = 1e-12)
  L <- matrix(c(2, 0.5, 0, 0.5, 1, 0.2, 0, 0.2, 3), 3)
  mu <- c(0.2, 0.5, 0.3)
  nu <- c(0.3, 0.3, 0.4)
  s <- Otsobm(mu, nu, L)
  d <- mu - nu
  expect_equal(s$quad_form, sum(d * (L %*% d)), tolerance = 1e-12)
  expect_equal(s$W1_sob, sqrt(sum(d * (L %*% d))), tolerance = 1e-12)
  expect_equal(s$mass_gap, 0, tolerance = 1e-12)
  expect_error(Otsobm(mu, nu[-1], L), "match both measures")
})

test_that("Otunbal is the Chizat et al. unbalanced scaling iteration", {
  a <- c(0.3, 0.9)
  b <- c(0.5, 0.2, 0.4)
  C <- outer(c(0, 1), c(0, 0.5, 2), "-")^2
  eps <- 0.2
  lam <- 1.5
  pw <- lam / (lam + eps)
  K <- exp(-C / eps)
  v <- rep(1, 3)
  for (i in 1:120) {
    u <- (a / as.numeric(K %*% v))^pw
    v <- (b / as.numeric(crossprod(K, u)))^pw
  }
  Tm <- u * K * rep(v, each = 2)
  r <- Otunbal(a, b, C, eps, lam, max_iter = 120)
  expect_equal(r$T, Tm, tolerance = 1e-12)
  expect_equal(r$cost, sum(Tm * C), tolerance = 1e-12)
  expect_equal(c(r$mass_a, r$mass_b), c(1.2, 1.1))
  expect_error(Otunbal(a, b, C, eps, 0), "positive")
  expect_error(Otunbal(a, b, t(C), eps, lam), "does not match")
})

test_that("Otws and Otws2 are sorted-sample Wasserstein distances", {
  x <- c(0.5, 2, 1, 4)
  y <- c(3, 1.5, 0, 2.5)
  expect_equal(Otws(x, y)$W1, mean(abs(sort(x) - sort(y))), tolerance = 1e-12)
  r <- Otws2(x, y, p = 3)
  expect_equal(r$Wp_p, mean(abs(sort(x) - sort(y))^3), tolerance = 1e-12)
  expect_equal(r$Wp, mean(abs(sort(x) - sort(y))^3)^(1 / 3), tolerance = 1e-12)
  expect_error(Otws2(x, y[-1]), "equal length")
})

test_that("Outmad applies the MAD-median rule", {
  x <- c(2.1, 2.5, 1.9, 2.2, 2.4, 9.0, 2.0, -3.0)
  r <- Outmad(x)
  s <- stats::mad(x, constant = 1) / 0.6745
  dis <- abs(x - stats::median(x)) / s
  expect_equal(r$dis, dis, tolerance = 1e-12)
  expect_equal(r$which, which(dis > 2.24))
  expect_equal(r$out_val, x[dis > 2.24])
  expect_equal(r$estimate, sum(dis > 2.24))
  expect_equal(Outmad(x, crit = 50)$n_out, 0L)
  expect_error(Outmad(1), "at least 2")
  expect_error(Outmad(c(1, NA)), "missing")
  expect_error(Outmad(x, crit = 0), "crit must be positive")
  expect_error(Outmad(c(1, 1, 1, 5)), "median absolute deviation is zero")
})
