# Coverage for the sgt* spectral-graph exports and Shewh. Every expectation
# is recomputed in the test body.

cov_sgt_graph <- function() {
  A <- matrix(0, 7, 7)
  ed <- rbind(c(1, 2, 1), c(1, 3, 2), c(2, 3, 1), c(3, 4, 0.5), c(4, 5, 1), c(4, 6, 1.5), c(5, 6, 2), c(6, 7, 1),
              c(5, 7, 1))
  A[ed[, 1:2]] <- ed[, 3]
  A[ed[, 2:1]] <- ed[, 3]
  A
}

cov_sgt_mod <- function(A, cm) {
  k <- rowSums(A)
  m2 <- sum(A)
  sum((A - outer(k, k) / m2)[outer(cm, cm, "==")]) / m2
}

test_that("Louvain local moving reaches a single-move local optimum", {
  A <- cov_sgt_graph()
  r <- sgtcoml(A)
  cm <- r$communities
  expect_equal(r$estimate, cov_sgt_mod(A, cm), tolerance = 1e-12)
  expect_equal(r$modularity_before, cov_sgt_mod(A, 0:6), tolerance = 1e-12)
  for (i in 1:7) for (c2 in unique(cm)) {
    alt <- cm
    alt[i] <- c2
    expect_lte(cov_sgt_mod(A, alt), r$estimate + 1e-12)
  }
  expect_equal(cm[1], 0L)
  expect_same_function(morie_sgtcoml, sgtcoml)
  expect_equal(Sgtmodq(A, cm)$Q, r$estimate, tolerance = 1e-12)
  expect_error(Sgtmodq(A, cm[-1]), "one entry per node")
  expect_error(Sgtmodq(matrix(0, 2, 2), c(0, 1)), "no edge weight")
})

test_that("degree matrix, Laplacians and their spectra", {
  A <- cov_sgt_graph()
  d <- rowSums(A)
  expect_equal(Degmat(A)$D, diag(d))
  expect_equal(Degmat(A)$volume, sum(d))
  expect_error(Degmat(-A), "non-negative")
  expect_error(Degmat(A + upper.tri(A)), "symmetric")
  L <- diag(d) - A
  expect_equal(Graphlap(A)$L, L, tolerance = 1e-12)
  expect_equal(Graphlap(A)$rowsum, rep(0, 7), tolerance = 1e-12)
  Ls <- diag(1 / sqrt(d)) %*% L %*% diag(1 / sqrt(d))
  expect_equal(Normlap(A)$Lcal, Ls, tolerance = 1e-12)
  ev <- sort(eigen(Ls, symmetric = TRUE)$values)
  sp <- Lapspec(A)
  expect_equal(sp$values, ev, tolerance = 1e-10)
  expect_equal(sp$n_components, 1L)
  expect_equal(sp$lambda1, ev[2], tolerance = 1e-10)
  expect_equal(Rwlap(A)$Lrw, diag(7) - A / d, tolerance = 1e-12)
  expect_error(Rwlap(matrix(0, 2, 2)), "positive degree")
  e2 <- Sgtlap2(A, k = 2)
  expect_equal(e2$eigvals, ev[2:3], tolerance = 1e-10)
  expect_equal(abs(colSums(e2$Y * eigen(Ls, symmetric = TRUE)$vectors[, 6:5])), c(1, 1), tolerance = 1e-8)
  expect_error(Sgtlap2(A, k = 7), "1 <= k <= n - 1")
  gap <- Eigengap(ev)
  expect_equal(gap$gaps, diff(ev), tolerance = 1e-12)
  expect_equal(gap$k, which.max(diff(ev)))
  expect_error(Eigengap(rev(ev)), "increasing order")
  expect_error(Eigengap(ev, kmax = 7), "kmax must satisfy")
  vs <- Vstrength(A)
  expect_equal(vs$strength, d)
  expect_equal(vs$degree, rowSums(A != 0))
  E <- rbind(c(1, 2, 1), c(2, 3, 2), c(3, 3, 5), c(1, 3, 0.5))
  wl <- Wgtlap(E)
  W <- matrix(0, 3, 3)
  W[1, 2] <- W[2, 1] <- 1
  W[2, 3] <- W[3, 2] <- 2
  W[3, 3] <- 5
  W[1, 3] <- W[3, 1] <- 0.5
  expect_equal(wl$W, W)
  # a self-loop adds to the degree but cancels out of the Laplacian
  Wo <- W - diag(diag(W))
  expect_equal(wl$L, diag(rowSums(Wo)) - Wo, tolerance = 1e-12)
  expect_error(Wgtlap(E[, 1:2]), "must be \\(u, v, weight\\)")
  expect_error(Wgtlap(rbind(c(1, 4, 1)), n = 3), "out of range")
})

test_that("graph kernels and centralities", {
  A <- cov_sgt_graph()
  L <- diag(rowSums(A)) - A
  e <- eigen(L, symmetric = TRUE)
  K <- e$vectors %*% diag(exp(-0.7 * e$values)) %*% t(e$vectors)
  df <- sgtdiff(A, beta = 0.7)
  expect_equal(df$kernel, K, tolerance = 1e-10)
  expect_equal(df$estimate, sum(exp(-0.7 * e$values)), tolerance = 1e-10)
  expect_same_function(morie_sgtdiff, sgtdiff)
  B <- (A > 0) * 1
  ev <- eigen(B, symmetric = TRUE)
  v <- abs(ev$vectors[, 1])
  expect_equal(Sgteigcent(B)$centrality, v / max(v), tolerance = 1e-10)
  expect_equal(Sgteigcent(B)$eigenvalue, ev$values[1], tolerance = 1e-10)
  ka <- sgtkem(B, alpha = 0.1)
  expect_equal(ka$centrality, as.numeric(solve(diag(7) - 0.1 * B, rep(1, 7))) - 1, tolerance = 1e-12)
  expect_equal(sgtkem(B)$alpha, 0.5 / max(abs(ev$values)), tolerance = 1e-12)
  expect_error(sgtkem(B, alpha = 1), "below 1 / spectral radius")
  expect_same_function(morie_sgtkem, sgtkem)
  rw <- Sgtrwk(B, lam = 0.1)
  expect_equal(rw$K, solve(diag(7) - 0.1 * B), tolerance = 1e-12)
  expect_error(Sgtrwk(B, lam = 0), "lam must be positive")
  pr <- Sgtpgr(B, d = 0.85, max_iter = 300)
  P <- B / rowSums(B)
  expect_equal(pr$pr, solve(diag(7) - 0.85 * t(P), rep(0.15 / 7, 7)), tolerance = 1e-10)
  D <- rbind(c(0, 1, 1, 0), c(0, 0, 1, 0), c(1, 0, 0, 1), c(0, 0, 1, 0))
  h <- sgthits(D)
  sa <- svd(D)
  expect_equal(h$authority, abs(sa$v[, 1]) / max(abs(sa$v[, 1])), tolerance = 1e-8)
  expect_equal(h$hub, abs(sa$u[, 1]) / max(abs(sa$u[, 1])), tolerance = 1e-8)
  expect_equal(h$eigenvalue, sa$d[1]^2, tolerance = 1e-8)
  expect_same_function(morie_sgthits, sgthits)
  sb <- sgtsbpd(B)
  expect_equal(sb$estimate, max(abs(ev$values)), tolerance = 1e-10)
  expect_equal(sb$perron_vector, abs(ev$vectors[, 1]), tolerance = 1e-10)
  expect_same_function(morie_sgtsbpd, sgtsbpd)
})

test_that("GCN layer, kernel PCA and Isomap", {
  A <- (cov_sgt_graph() > 0) * 1
  X <- cbind(1:7 / 7, c(1, 0, 1, 0, 1, 0, 1))
  Wt <- matrix(c(0.5, -1, 0.3, 0.8), 2)
  Ai <- A + diag(7)
  An <- Ai / sqrt(outer(rowSums(Ai), rowSums(Ai)))
  g <- Sgtgrn(A, X, Wt)
  expect_equal(g$X_next, pmax(An %*% X %*% Wt, 0), tolerance = 1e-12)
  expect_equal(Sgtgrn(A, X, Wt, activation = "none")$X_next, An %*% X %*% Wt, tolerance = 1e-12)
  P <- cbind(c(0, 1, 2, 3, 4, 5), c(0, 0.5, 0.3, 1.2, 0.8, 1.5))
  kp <- Kernelpca(P, kernel = "rbf", k = 2, gamma = 0.3)
  Kg <- exp(-0.3 * as.matrix(stats::dist(P))^2)
  J <- diag(6) - 1 / 6
  Kc <- J %*% Kg %*% J
  e <- eigen(Kc, symmetric = TRUE)
  expect_equal(kp$eigvals, e$values[1:2], tolerance = 1e-10)
  # scores are Kc alpha with alpha = v / sqrt(lambda), i.e. sqrt(lambda) v up to sign
  expect_equal(abs(kp$Y), abs(e$vectors[, 1:2] %*% diag(sqrt(e$values[1:2]))), tolerance = 1e-8)
  lin <- Kernelpca(P, kernel = "linear", k = 1)
  expect_equal(lin$eigvals, stats::prcomp(P)$sdev[1]^2 * 5, tolerance = 1e-10)
  im <- Isomapmds(P, k_nn = 5, dim = 2)
  # with every point a neighbour of every other, Isomap is classical MDS
  cm <- stats::cmdscale(stats::dist(P), k = 2, eig = TRUE)
  expect_equal(im$eigvals, cm$eig[1:2], tolerance = 1e-10)
  expect_equal(abs(im$Y), abs(unname(cm$points)), tolerance = 1e-8)
})

test_that("Leiden refinement, mixing time, Ncut, Fiedler and NJW partitions", {
  A <- cov_sgt_graph()
  r <- Sgtleid(A, c(0, 0, 0, 1, 1, 1, 1), gamma = 0.3)
  for (s in unique(r$labels_new)) expect_length(unique(c(0, 0, 0, 1, 1, 1, 1)[r$labels_new == s]), 1)
  q <- 0
  for (s in unique(r$labels_new)) {
    m <- which(r$labels_new == s)
    q <- q + sum(A[m, m]) / 2 - 0.3 * choose(length(m), 2)
  }
  expect_equal(r$Q_new, q, tolerance = 1e-12)
  d <- rowSums(A)
  ev <- sort(eigen(A / sqrt(outer(d, d)), symmetric = TRUE)$values)
  mx <- Sgtmix(A, epsilon = 0.05)
  expect_equal(mx$slem, max(abs(ev[1:6])), tolerance = 1e-10)
  expect_equal(mx$tau_mix, log(20) / (1 - max(abs(ev[1:6]))), tolerance = 1e-10)
  expect_error(Sgtmix(A, epsilon = 1), "epsilon must lie")
  lab <- c("a", "a", "a", "b", "b", "b", "b")
  nc <- Ncut(A, lab)
  vol <- tapply(d, lab, sum)
  cut <- sum(A[1:3, 4:7])
  expect_equal(nc$ncut, cut / vol[["a"]] + cut / vol[["b"]], tolerance = 1e-12)
  expect_equal(nc$cut, cut)
  expect_equal(nc$nassoc, 2 - nc$ncut, tolerance = 1e-12)
  fc <- Fiedlercut(A)
  Ls <- diag(7) - A / sqrt(outer(d, d))
  e <- eigen(Ls, symmetric = TRUE)
  f <- e$vectors[, 6] / sqrt(d)
  f <- f * sign(f[which.max(abs(f))])
  expect_equal(fc$estimate, e$values[6], tolerance = 1e-10)
  expect_equal(fc$labels, as.integer(f >= 0))
  sc <- Sgtsck(A, k = 2)
  expect_equal(sc$eigvals, sort(e$values)[1:2], tolerance = 1e-10)
  expect_equal(length(unique(sc$labels[1:3])), 1)
  expect_equal(length(unique(sc$labels[4:7])), 1)
  expect_error(Sgtsck(A, k = 8), "1 <= k <= n")
})

test_that("SBM detectability, spanning trees, WL refinement, walks and Shewhart", {
  s <- Sgtsbnd(8, 2, k = 2)
  cc <- (8 + 2) / 2
  expect_equal(s$margin, 6 - 2 * sqrt(cc), tolerance = 1e-12)
  expect_equal(s$detectable, 1)
  expect_equal(Sgtsbnd(3, 2)$detectable, 0)
  expect_error(Sgtsbnd(1, 1, k = 1), "at least 2")
  K4 <- matrix(1, 4, 4) - diag(4)
  expect_equal(sgtspn(K4)$estimate, 4^2)
  C5 <- matrix(0, 5, 5)
  for (i in 1:5) C5[i, i %% 5 + 1] <- C5[i %% 5 + 1, i] <- 1
  expect_equal(sgtspn(C5)$estimate, 5)
  expect_same_function(morie_sgtspn, sgtspn)
  P3 <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
  wl <- Sgtwlk(P3, max_iter = 2)
  expect_equal(wl$labels_t, c(0L, 1L, 0L))
  expect_equal(wl$history, c(1, 2, 2))
  B <- (cov_sgt_graph() > 0) * 1
  w <- Sgtwlk(B, max_iter = 1)
  expect_equal(w$labels_t, match(rowSums(B), unique(rowSums(B))) - 1L)
  x <- c(10.2, 9.8, 13.5, 10.1, 6.2, 10.0)
  sh <- Shewh(x, 10, 1, k = 3)
  expect_equal(sh$alerts, as.integer(abs(x - 10) > 3))
  expect_equal(sh$false_alarm_prob, 2 * stats::pnorm(-3), tolerance = 1e-12)
  expect_equal(sh$arl0, 1 / (2 * stats::pnorm(-3)), tolerance = 1e-10)
  expect_error(Shewh(x, 10, 0), "sigma must be")
  expect_error(Shewh(numeric(0), 0, 1), "empty")
})
