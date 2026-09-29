# Coverage for network centralities (degree, betweenness, closeness,
# eigenvector), error/attack tolerance, cosine-normalised graph kernels
# and the graphical lasso; recomputed with Floyd-Warshall distances,
# matrix-power path counts, eigen(), combn() graphlet counts and glasso.

floyd <- function(A) {
  n <- nrow(A)
  D <- ifelse(A != 0, 1, Inf)
  diag(D) <- 0
  for (k in seq_len(n)) D <- pmin(D, outer(D[, k], D[k, ], "+"))
  D
}

undirected <- function(n, edges) {
  A <- matrix(0, n, n)
  A[edges] <- 1
  A[edges[, 2:1, drop = FALSE]] <- 1
  A
}

test_that("Netdeg, Netclocent and Neteigcent match their definitions", {
  A <- undirected(6, rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(5, 6), c(4, 6)))
  expect_equal(Netdeg(A, 2)$estimate, sum(A[3, ]) / 5, tolerance = 1e-12)
  As <- A
  As[1, 1] <- 1
  expect_equal(Netdeg(As, 0)$degree, sum(A[1, ]))
  expect_error(Netdeg(A, 6), "out of range")
  expect_error(Netdeg(matrix(1, 1, 1)), "single-node")
  expect_error(Netdeg(A[, -1]), "must be square")
  expect_error(Netdeg(matrix(numeric(0), 0, 0)), "empty")
  D <- floyd(A)
  cl <- Netclocent(A)
  expect_equal(cl$closeness, 5 / rowSums(D), tolerance = 1e-12)
  A7 <- rbind(cbind(A, 0), 0)
  c7 <- Netclocent(A7)
  expect_true(is.na(c7$closeness[7]))
  expect_equal(c7$reachable, c(rep(6L, 6), 1L))
  e <- eigen(A, symmetric = TRUE)
  v <- e$vectors[, 1]
  if (sum(v) < 0) v <- -v
  ec <- Neteigcent(A)
  expect_equal(ec$eigenvalue, e$values[1], tolerance = 1e-10)
  expect_equal(ec$centrality, v / max(abs(v)), tolerance = 1e-9)
})

test_that("Netbtw counts shortest-path dependencies", {
  btw <- function(A) {
    n <- nrow(A)
    B <- (A != 0) * 1
    D <- floyd(B)
    P <- list(diag(n))
    for (k in seq_len(n)) P[[k + 1]] <- P[[k]] %*% B
    sg <- function(s, t) if (is.finite(D[s, t])) P[[D[s, t] + 1]][s, t] else 0
    vapply(seq_len(n), function(v) {
      tot <- 0
      for (s in seq_len(n)) for (t in seq_len(n)) {
        if (s == t || s == v || t == v || !is.finite(D[s, t])) next
        if (D[s, v] + D[v, t] == D[s, t]) tot <- tot + sg(s, v) * sg(v, t) / sg(s, t)
      }
      tot
    }, 0)
  }
  A <- undirected(6, rbind(c(1, 2), c(1, 3), c(2, 4), c(3, 4), c(4, 5), c(5, 6)))
  cb <- btw(A)
  for (v in 0:5) {
    r <- Netbtw(A, v)
    expect_equal(r$cb_ordered, cb[v + 1], tolerance = 1e-12)
    expect_equal(r$estimate, cb[v + 1] / 2, tolerance = 1e-12)
    expect_equal(r$normalized, cb[v + 1] / 2 / (5 * 4 / 2), tolerance = 1e-12)
  }
  Ad <- matrix(0, 4, 4)
  Ad[cbind(c(1, 2, 3, 1), c(2, 3, 4, 3))] <- 1
  cd <- btw(Ad)
  dr <- Netbtw(Ad, 2)
  expect_equal(dr$symmetric, 0)
  expect_equal(dr$estimate, cd[3], tolerance = 1e-12)
  expect_equal(dr$normalized, cd[3] / 6, tolerance = 1e-12)
  expect_equal(Netbtw(matrix(c(0, 1, 1, 0), 2), 0)$normalized, 0)
  expect_error(Netbtw(A, -1), "out of range")
  expect_error(Netbtw(A[-1, ]), "must be square")
  expect_error(Netbtw(matrix(numeric(0), 0, 0)), "empty")
})

test_that("Netattack removes hubs or LCG-chosen nodes and measures the remnant", {
  A <- undirected(8, rbind(c(1, 2), c(1, 3), c(1, 4), c(1, 5), c(2, 3), c(5, 6), c(6, 7), c(7, 8)))
  summ <- function(removed) {
    keep <- setdiff(1:8, removed)
    D <- floyd(A[keep, keep])
    lab <- apply(is.finite(D), 1, function(r) min(which(r)))
    sz <- sort(as.integer(table(lab)), decreasing = TRUE)
    off <- D[row(D) != col(D) & is.finite(D)]
    list(giant = sz[1] / 8, frag = if (length(sz) > 1) mean(sz[-1]) else NA_real_,
         diam = mean(off), nc = length(sz))
  }
  deg <- rowSums(A)
  rem <- sort(order(-deg, 1:8)[1:2])
  r <- Netattack(A, k = 2)
  s <- summ(rem)
  expect_equal(r$removed, rem - 1L)
  expect_equal(c(r$s_giant, r$mean_fragment, r$diameter, r$n_components), c(s$giant, s$frag, s$diam, s$nc),
               tolerance = 1e-12)
  g <- .t1_lcg(4)
  pool <- 1:8
  pick <- integer(0)
  for (i in 1:3) {
    j <- min(floor(g$unif() * length(pool)) + 1, length(pool))
    pick <- c(pick, pool[j])
    pool <- pool[-j]
  }
  er <- Netattack(A, strategy = "error", k = 3, seed = 4)
  se <- summ(sort(pick))
  expect_equal(er$removed, sort(pick) - 1L)
  expect_equal(c(er$s_giant, er$n_components), c(se$giant, se$nc), tolerance = 1e-12)
  expect_equal(er$diameter, se$diam, tolerance = 1e-12)
  expect_error(Netattack(A, strategy = "random"), "strategy must be")
})

test_that("Graphcmp cosine-normalises the WL, random-walk and graphlet kernels", {
  G1 <- undirected(4, rbind(c(1, 2), c(2, 3), c(3, 4)))
  G2 <- undirected(4, rbind(c(1, 2), c(2, 3), c(1, 3), c(3, 4)))
  wl <- function(a, b) Wlkernel(a, b, 2)$estimate
  r <- Graphcmp(G1, G2, h = 2)
  expect_equal(r$raw, wl(G1, G2))
  expect_equal(r$estimate, wl(G1, G2) / sqrt(wl(G1, G1) * wl(G2, G2)), tolerance = 1e-12)
  expect_equal(Graphcmp(G1, G2, h = 0)$estimate, 1, tolerance = 1e-12)
  rwk <- function(a, b) sum(solve(diag(16) - 0.1 * kronecker(a, b), rep(1, 16)))
  rw <- Graphcmp(G1, G2, kernel = "rw", lam = 0.1)
  expect_equal(rw$raw, rwk(G1, G2), tolerance = 1e-9)
  expect_equal(rw$estimate, rwk(G1, G2) / sqrt(rwk(G1, G1) * rwk(G2, G2)), tolerance = 1e-9)
  gl <- function(A) {
    tr <- combn(4, 3)
    tabulate(apply(tr, 2, function(i) sum(A[i, i]) / 2) + 1, 4)
  }
  h1 <- gl(G1)
  h2 <- gl(G2)
  gk <- Graphcmp(G1, G2, kernel = "graphlet")
  expect_equal(gk$estimate, sum(h1 * h2) / sqrt(sum(h1^2) * sum(h2^2)), tolerance = 1e-12)
  expect_equal(gk$kernel, "graphlet")
})

test_that("morie_netcms solves the graphical lasso", {
  set.seed(9)
  X <- matrix(rnorm(60 * 4), 60)
  X[, 2] <- X[, 2] + 0.8 * X[, 1]
  X[, 4] <- X[, 4] - 0.6 * X[, 3]
  S <- crossprod(sweep(X, 2, colMeans(X))) / 60
  r <- morie_netcms(X, lam = 0.1, tol = 1e-12, maxit = 2000)
  W <- r$covariance_fit
  Th <- r$precision
  expect_equal(diag(W), diag(S) + 0.1, tolerance = 1e-12)
  # KKT of the penalised likelihood: W - S = lam * sign(Theta) on the
  # support and |W - S| <= lam off it; coordinate descent stops at 1e-12
  off <- row(S) != col(S)
  nz <- off & abs(Th) > 1e-10
  expect_equal((W - S)[nz], 0.1 * sign(Th[nz]), tolerance = 1e-8)
  expect_true(all(abs((W - S)[off & !nz]) <= 0.1 + 1e-8))
  expect_equal(Th, solve(W), tolerance = 1e-8)
  expect_equal(r$partial_correlations[off], (-Th / sqrt(outer(diag(Th), diag(Th))))[off], tolerance = 1e-12)
  expect_equal(r$n_edges, sum(nz) / 2)
  expect_equal(morie_netcms(S = S, lam = 0.1, tol = 1e-12, maxit = 2000)$precision, Th, tolerance = 1e-12)
  expect_equal(morie_netcms(S = S, lam = 0, tol = 1e-12, maxit = 2000)$precision, solve(S), tolerance = 1e-8)
  expect_error(morie_netcms(), "provide data or S")
  expect_error(morie_netcms(S = S, lam = -1), "non-negative")
  skip_if_not_installed("glasso")
  g <- glasso::glasso(S, rho = 0.1, thr = 1e-12, maxit = 1e4)
  # two different coordinate-descent schedules; both stop near 1e-12 in
  # the inner updates, which leaves roughly 1e-7 in the precision
  expect_equal(Th, g$wi, tolerance = 1e-6)
})
