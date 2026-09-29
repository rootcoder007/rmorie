# Coverage for vbnpc .. vinasc exports. Every expectation is recomputed in
# the test body.

test_that("Vbnpc reaches a stick-breaking mean-field fixed point", {
  y <- c(-2.1, -1.8, -2.4, 0.1, 0.3, -0.2, 3.0, 2.7, 3.3, 2.9)
  r <- Vbnpc(y, K_truncate = 4, alpha = 1, sigma2 = 0.3, s0 = 5)
  expect_equal(r$elbo_monotone, 1)
  expect_equal(r$converged, 1)
  expect_equal(rowSums(r$phi), rep(1, 10), tolerance = 1e-12)
  Nt <- colSums(r$phi)
  Sy <- colSums(r$phi * y)
  p0 <- 1 / 25
  s2 <- 1 / (p0 + Nt / 0.3)
  expect_equal(r$s2, s2, tolerance = 1e-12)
  expect_equal(r$m, (Sy / 0.3) * s2, tolerance = 1e-12)
  expect_equal(r$gamma1, 1 + Nt, tolerance = 1e-12)
  expect_equal(r$gamma2, 1 + rev(cumsum(rev(Nt))) - Nt, tolerance = 1e-12)
  v <- r$gamma1 / (r$gamma1 + r$gamma2)
  v[4] <- 1
  expect_equal(r$weights, v * c(1, cumprod(1 - v)[-4]), tolerance = 1e-12)
  expect_equal(sum(r$weights), 1, tolerance = 1e-12)
  expect_error(Vbnpc(numeric(0)), "y is empty")
  expect_error(Vbnpc(y, alpha = 0), "alpha must be positive")
})

test_that("morie_vcomp estimates balanced variance components and the ICC interval", {
  g <- rep(1:5, each = 4)
  y <- c(10.1, 9.8, 10.4, 10.0, 12.2, 11.9, 12.5, 12.1, 9.1, 9.5, 8.8, 9.2, 11.0, 11.3, 10.7, 11.2, 10.5, 10.9, 10.2, 10.6)
  a <- morie_vcomp(y, g, method = "anova")
  f <- stats::anova(stats::lm(y ~ factor(g)))
  msa <- f[1, "Mean Sq"]
  mse <- f[2, "Mean Sq"]
  expect_equal(a$sigma2_e, mse, tolerance = 1e-10)
  expect_equal(a$sigma2_a, (msa - mse) / 4, tolerance = 1e-10)
  Fv <- msa / mse
  lo <- (Fv / stats::qf(0.975, 4, 15) - 1) / (Fv / stats::qf(0.975, 4, 15) - 1 + 4)
  hi <- (Fv / stats::qf(0.025, 4, 15) - 1) / (Fv / stats::qf(0.025, 4, 15) - 1 + 4)
  expect_equal(c(a$icc_lower, a$icc_upper), c(max(lo, 0), min(hi, 1)), tolerance = 1e-10)
  expect_true(a$balanced)
  r <- morie_vcomp(y, g)
  # balanced one-way REML equals the ANOVA estimates when they are positive
  expect_equal(c(r$sigma2_a, r$sigma2_e), c(a$sigma2_a, a$sigma2_e), tolerance = 1e-6)
  expect_equal(r$icc, r$sigma2_a / (r$sigma2_a + r$sigma2_e), tolerance = 1e-12)
  expect_error(morie_vcomp(y, g, method = "ml"), "'reml' or 'anova'")
  expect_error(morie_vcomp(y, g, conf_level = 1), "conf_level must lie")
})

vr_len <- function(route, P) {
  pts <- rbind(P[1, ], P[route + 1, , drop = FALSE], P[1, ])
  sum(sqrt(rowSums(diff(pts)^2)))
}

test_that("MultiDepotVrp, PeriodicVrp and SolomonVrptw build feasible routes", {
  dep <- rbind(c(0, 0), c(10, 0))
  cus <- rbind(c(1, 2), c(2, -1), c(9, 1), c(11, 2), c(3, 3), c(8, -2))
  dem <- c(3, 4, 2, 5, 3, 4)
  r <- MultiDepotVrp(dep, cus, dem, capacity = 8)
  near <- apply(cus, 1, function(p) which.min(colSums((t(dep) - p)^2)))
  expect_equal(r$assignment, near - 1L)
  tot <- 0
  for (d in 1:2) {
    Pd <- rbind(dep[d, ], cus)
    for (rt in r$routes[[d]]) {
      expect_true(all(near[rt + 1] == d))
      expect_lte(sum(dem[rt + 1]), 8)
      tot <- tot + vr_len(rt + 1, Pd)
    }
  }
  expect_setequal(unlist(r$routes), 0:5)
  expect_equal(r$length, tot, tolerance = 1e-10)
  P <- rbind(c(0, 0), c(1, 2), c(2, -1), c(-1, 1), c(3, 3))
  pv <- PeriodicVrp(P, demand = c(0, 2, 3, 1, 2), frequency = c(0, 2, 1, 4, 2), horizon = 4, capacity = 5)
  freq <- c(2, 1, 4, 2)
  for (i in 1:4) {
    dd <- pv$days[[i]]
    expect_length(dd, freq[i])
    expect_equal(unique(diff(dd)), if (freq[i] > 1) 4 / freq[i] else numeric(0))
  }
  load <- vapply(0:3, function(d) sum(c(2, 3, 1, 2)[vapply(pv$days, function(x) d %in% x, TRUE)]), 0)
  expect_equal(pv$daily_load, load)
  for (d in 1:4) expect_setequal(unlist(pv$routes[[d]]), which(vapply(pv$days, function(x) (d - 1) %in% x, TRUE)))
  expect_error(PeriodicVrp(P, c(0, 2, 3, 1, 2), c(0, 3, 1, 4, 2), 4, 5), "positive divisors")
  Ps <- rbind(c(0, 0), c(2, 0), c(0, 3), c(-2, 1), c(1, 1))
  rd <- c(0, 0, 5, 0, 2)
  du <- c(100, 10, 20, 30, 15)
  sv <- c(0, 1, 1, 1, 1)
  so <- SolomonVrptw(Ps, demand = c(0, 2, 2, 2, 2), ready = rd, due = du, service = sv, capacity = 5)
  D <- as.matrix(stats::dist(Ps))
  expect_setequal(unlist(so$routes), 1:4)
  for (k in seq_along(so$routes)) {
    rt <- so$routes[[k]]
    expect_lte(length(rt) * 2, 5)
    t <- 0
    prev <- 1
    st <- numeric(0)
    for (u in rt + 1) {
      t <- max(rd[u], t + D[prev, u])
      expect_lte(t, du[u])
      st <- c(st, t)
      t <- t + sv[u]
      prev <- u
    }
    expect_equal(so$schedules[[k]], st, tolerance = 1e-12)
  }
  expect_equal(so$length, sum(vapply(so$routes, vr_len, 0, P = Ps)), tolerance = 1e-10)
  expect_error(SolomonVrptw(Ps, c(0, 9, 2, 2, 2), rd, du, sv, 5), "cannot be served on its own")
})

test_that("Vgrm is the Matheron semivariogram", {
  P <- cbind(c(0, 1, 2, 0.5, 1.5, 2.5, 0.2), c(0, 0.5, 0, 1.2, 1, 1.4, 2.0))
  z <- c(1.2, 0.4, -0.3, 0.8, 0.1, 0.6, 1.0)
  r <- Vgrm(P, z, bins = 3)
  D <- as.matrix(stats::dist(P))
  ij <- which(upper.tri(D), arr.ind = TRUE)
  d <- D[ij]
  sq <- (z[ij[, 1]] - z[ij[, 2]])^2
  md <- max(d) / 2
  ed <- seq(0, md, length.out = 4)
  k <- d <= md
  b <- pmin(pmax(findInterval(d[k], ed), 1), 3)
  expect_equal(r$n_pairs, tabulate(b, 3))
  expect_equal(r$gamma, vapply(1:3, function(q) if (any(b == q)) sum(sq[k][b == q]) / (2 * sum(b == q)) else NA_real_, 0),
               tolerance = 1e-12)
  e <- Vgrm(P, z, bins = c(0, 1, 2.5))
  ke <- d <= 2.5
  be <- pmin(pmax(findInterval(d[ke], c(0, 1, 2.5)), 1), 2)
  expect_equal(e$lag, vapply(1:2, function(q) mean(d[ke][be == q]), 0), tolerance = 1e-12)
  expect_error(Vgrm(P, z[-1]), "same number of rows")
  expect_error(Vgrm(P, z, bins = c(1, 0)), "ascending")
})

test_that("Vilbrt runs dual-stream co-attention", {
  img <- rbind(c(0.2, 0.5, -0.1), c(0.4, -0.3, 0.8))
  txt <- rbind(c(0.1, 0.2, 0.3, 0.4), c(-0.2, 0.5, 0.1, 0.0), c(0.3, -0.1, 0.2, 0.6))
  r <- Vilbrt(img, txt, d_model = 4, seed = 2)
  W <- function(nr, nc, s) matrix(.morie_w4d_lcg_vec(nr * nc, s), nr, nc, byrow = TRUE)
  Hv <- img %*% W(3, 4, 4)
  Ht <- txt
  att <- function(Q, K, V) {
    S <- Q %*% t(K) / 2
    A <- exp(S - apply(S, 1, max))
    A <- A / rowSums(A)
    list(Y = A %*% V, A = A)
  }
  v2t <- att(Hv %*% W(4, 4, 12), Ht %*% W(4, 4, 13), Ht %*% W(4, 4, 14))
  t2v <- att(Ht %*% W(4, 4, 22), Hv %*% W(4, 4, 23), Hv %*% W(4, 4, 24))
  expect_equal(r$attention_v2t, v2t$A, tolerance = 1e-12)
  expect_equal(r$image_out, Hv + v2t$Y, tolerance = 1e-12)
  expect_equal(r$text_out, Ht + t2v$Y, tolerance = 1e-12)
  expect_equal(r$pooled, 0.5 * (colMeans(Hv + v2t$Y) + colMeans(Ht + t2v$Y)), tolerance = 1e-12)
})

test_that("Vinasc evaluates the Vina scoring terms", {
  R <- rbind(c(0, 0, 0, 1.9, 1), c(3, 0, 0, 1.8, 2), c(0, 9, 0, 1.7, 0))
  L <- rbind(c(1.5, 1.0, 0, 1.5, 1), c(4.0, 0.5, 0.3, 1.6, 2))
  r <- Vinasc(R, L, n_rot = 3)
  g1 <- g2 <- rp <- hy <- hb <- 0
  for (i in 1:3) for (j in 1:2) {
    rr <- sqrt(sum((R[i, 1:3] - L[j, 1:3])^2))
    if (rr > 8) next
    d <- rr - R[i, 4] - L[j, 4]
    g1 <- g1 + exp(-(d / 0.5)^2)
    g2 <- g2 + exp(-((d - 3) / 2)^2)
    rp <- rp + if (d < 0) d^2 else 0
    if (R[i, 5] == 1 && L[j, 5] == 1) hy <- hy + if (d < 0.5) 1 else if (d < 1.5) 1.5 - d else 0
    if (R[i, 5] == 2 && L[j, 5] == 2) hb <- hb + if (d < -0.7) 1 else if (d < 0) -d / 0.7 else 0
  }
  ci <- -0.0356 * g1 - 0.00516 * g2 + 0.840 * rp - 0.0351 * hy - 0.587 * hb
  expect_equal(c(r$gauss1, r$gauss2, r$repulsion, r$hydrophobic, r$hbond), c(g1, g2, rp, hy, hb), tolerance = 1e-12)
  expect_equal(r$estimate, ci / (1 + 0.0585 * 3), tolerance = 1e-12)
})
