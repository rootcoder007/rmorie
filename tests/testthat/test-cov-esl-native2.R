# Coverage tests for R/esl_native2.R (Hastie, Tibshirani and Friedman,
# ESL 2nd ed.): sparse PCA, thin-plate splines, ICA, Isomap, LLE, SOM,
# LVQ, partial dependence, neural nets, RBMs, Dirichlet processes, MRFs
# and score matching. Each check recomputes a defining identity.

esl_X <- function(n = 20) {
  i <- 1:n
  cbind(sin(i), cos(1.7 * i) + 0.3 * sin(i), ((i * 7) %% 11) / 11 - 0.5)
}

test_that("sparse PCA: lambda = 0 is ordinary PCA; lambda > 0 is a soft-threshold fixed point", {
  X <- esl_X()
  S <- cov(X)
  sp <- morie_esl_sparse_pca(X, k = 2, lambda_ = 0)
  ev <- eigen(S, symmetric = TRUE)
  v1 <- ev$vectors[, 1] * sign(ev$vectors[which.max(abs(ev$vectors[, 1])), 1])
  expect_equal(sp$loadings[, 1], v1, tolerance = 1e-8)
  expect_equal(sp$adjusted_variance[1], ev$values[1], tolerance = 1e-8)
  sl <- morie_esl_sparse_pca(X, k = 1, lambda_ = 0.2)
  v <- sl$loadings[, 1]
  t <- as.numeric(S %*% v)
  st <- sign(t) * pmax(abs(t) - 0.2, 0)
  expect_equal(v, st / sqrt(sum(st^2)), tolerance = 1e-7)
  expect_error(morie_esl_sparse_pca(X, k = 4), "between 1")
  expect_error(morie_esl_sparse_pca(X, lambda_ = -1), "non-negative")
})

test_that("thin-plate spline solves the bordered system and is translation invariant", {
  X <- cbind(c(0, 1, 0, 1, 0.5, 0.2, 0.8, 0.3), c(0, 0, 1, 1, 0.5, 0.7, 0.1, 0.9))
  y <- c(1, 2, 0.5, 3, 1.7, 0.9, 2.2, 1.1)
  f <- morie_esl_thin_plate_spline(X, y, lambda_ = 0.1)
  r2 <- as.matrix(dist(X))^2
  E <- ifelse(r2 > 0, 0.5 * r2 * log(r2), 0)
  A <- cbind(1, X)
  big <- rbind(cbind(E + 0.1 * diag(8), A), cbind(t(A), matrix(0, 3, 3)))
  sol <- unname(solve(unname(big), c(y, 0, 0, 0)))
  expect_equal(f$delta, sol[1:8], tolerance = 1e-9)
  expect_equal(f$beta, sol[9:11], tolerance = 1e-9)
  lin <- 2 + 3 * X[, 1] - X[, 2]
  fl <- morie_esl_thin_plate_spline(X, lin, lambda_ = 5)
  expect_equal(fl$fitted, lin, tolerance = 1e-9)
  expect_equal(fl$delta, rep(0, 8), tolerance = 1e-9)
  fs <- morie_esl_thin_plate_spline(X + 1e6, y, lambda_ = 0.1)
  expect_equal(fs$fitted, f$fitted, tolerance = 1e-6)
  expect_error(morie_esl_thin_plate_spline(cbind(X, 1), y), "2-D")
  expect_error(morie_esl_thin_plate_spline(X, y, lambda_ = -1), "non-negative")
})

test_that("ICA whitens and recovers independent non-Gaussian sources", {
  n <- 400
  i <- 1:n
  s1 <- ((i * 37) %% 101) / 101 - 0.5
  s2 <- sign(sin(i / 3)) * abs(sin(i / 7))^3
  S0 <- cbind(s1, s2)
  A <- rbind(c(1, 0.6), c(0.4, 1))
  X <- S0 %*% t(A)
  r <- morie_esl_ica(X)
  cs <- cov(r$sources) * (n - 1) / n
  expect_equal(cs, diag(2), tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(sweep(X, 2, r$mean) %*% t(r$unmixing), r$sources, tolerance = 1e-9, ignore_attr = TRUE)
  expect_equal(r$unmixing %*% r$mixing, diag(2), tolerance = 1e-9)
  # an iterative estimate of independent components: each true source is
  # matched by one estimated source up to sign and scale (|r| > 0.98)
  cc <- abs(cor(r$sources, S0))
  expect_true(all(apply(cc, 2, max) > 0.98))
  expect_error(morie_esl_ica(X, fun = "tanh"), "fun must be")
  expect_error(morie_esl_ica(X, k = 3), "between 1")
})

test_that("Isomap equals classical MDS of the graph geodesics; LLE weights and eigenvectors", {
  t <- seq(0, 3, length.out = 15)
  X <- cbind(t, 2 * t)
  r <- morie_esl_isomap(X, k = 1, neighbors = 2)
  # points on a line: geodesics are the Euclidean distances
  expect_equal(r$geodesic, as.matrix(dist(X)), tolerance = 1e-12, ignore_attr = TRUE)
  cm <- cmdscale(r$geodesic, k = 1)
  expect_equal(abs(as.numeric(r$embedding)), abs(as.numeric(cm)), tolerance = 1e-9)
  expect_equal(r$residual_variance, 0, tolerance = 1e-12)
  Xg <- rbind(X[1:5, ], X[11:15, ] + 100)
  expect_error(morie_esl_isomap(Xg, 1, 2), "disconnected")
  expect_error(morie_esl_isomap(X, k = 15), "between 1")
  X3 <- esl_X(15)
  l <- morie_esl_lle(X3, k = 2, neighbors = 4)
  expect_equal(rowSums(l$weights), rep(1, 15), tolerance = 1e-12)
  i <- 3
  d <- colSums((t(X3) - X3[i, ])^2)
  nb <- order(d)[2:5]
  Z <- sweep(X3[nb, ], 2, X3[i, ])
  C <- tcrossprod(Z)
  C <- C + 1e-3 * sum(diag(C)) * diag(4)
  w <- solve(C, rep(1, 4))
  expect_equal(l$weights[i, nb], w / sum(w), tolerance = 1e-12)
  M <- crossprod(diag(15) - l$weights)
  e <- l$embedding / sqrt(15)
  expect_equal(M %*% e, sweep(e, 2, l$eigenvalues[2:3], "*"), tolerance = 1e-9)
  expect_error(morie_esl_lle(X3, neighbors = 15), "between 1")
})

test_that("SOM and LVQ outputs are consistent with their prototypes", {
  X <- esl_X(30)[, 1:2]
  som <- morie_esl_self_organize(X, grid = c(2, 3), n_epochs = 10, seed = 3)
  d2 <- outer(1:30, 1:6, Vectorize(function(i, j) sum((X[i, ] - som$prototypes[j, ])^2)))
  expect_equal(som$assignment, apply(d2, 1, which.min))
  expect_equal(som$quantization_error, mean(sqrt(apply(d2, 1, min))), tolerance = 1e-12)
  expect_equal(som$counts, tabulate(som$assignment, 6))
  expect_error(morie_esl_self_organize(X, grid = c(6, 6)), "only 30")
  expect_error(morie_esl_self_organize(X, eta = 2), "eta")
  y <- ifelse(X[, 1] > 0, "p", "n")
  lv <- morie_esl_prototype_lvq(X, y, n_prototypes = 2, n_epochs = 20, seed = 1)
  d2p <- outer(1:30, 1:4, Vectorize(function(i, j) sum((X[i, ] - lv$prototypes[j, ])^2)))
  expect_equal(lv$class, lv$prototype_class[apply(d2p, 1, which.min)])
  expect_equal(lv$accuracy, mean(lv$class == y))
  expect_error(morie_esl_prototype_lvq(X, y, n_prototypes = 40), "fewer than")
})

test_that("partial dependence of a linear model is linear in the grid", {
  X <- esl_X()
  beta <- c(2, -1, 0.5)
  model <- function(Z) as.numeric(Z %*% beta)
  pd <- morie_esl_partial_dependence(model, X, S = 2, n_grid = 5)
  g <- as.numeric(quantile(X[, 2], seq(0.05, 0.95, length.out = 5)))
  expect_equal(as.numeric(pd$grid), g, tolerance = 1e-12)
  expect_equal(pd$pd, mean(X[, -2] %*% beta[-2]) + beta[2] * g, tolerance = 1e-12)
  pd2 <- morie_esl_partial_dependence(model, X, S = c(1, 3), grid = rbind(c(0, 0), c(1, 1)))
  expect_equal(pd2$pd, mean(X[, 2]) * -1 + c(0, 2.5), tolerance = 1e-12)
  far <- morie_esl_partial_dependence(model, X, S = 1, grid = 100)
  expect_true(far$extrapolation_warning)
  expect_error(morie_esl_partial_dependence(model, X, S = 4), "outside")
})

test_that("neural net: returned weights reproduce the fitted values; loss falls", {
  X <- esl_X(25)
  y <- X[, 1] - 2 * X[, 2]^2
  nn <- morie_esl_neural_net(X, y, M = 4, n_epochs = 300, lr = 0.2, seed = 2)
  Xs <- sweep(sweep(X, 2, nn$mean), 2, nn$sd, "/")
  H <- plogis(sweep(Xs %*% nn$alpha, 2, nn$alpha0, "+"))
  expect_equal(nn$fitted, as.numeric(H %*% nn$beta + nn$beta0), tolerance = 1e-12)
  expect_lt(nn$loss_path[300], nn$loss_path[1])
  cl <- morie_esl_neural_net(X, ifelse(y > 0, "a", "b"), M = 3, task = "classification", n_epochs = 100, seed = 1)
  expect_equal(rowSums(cl$prob), rep(1, 25), tolerance = 1e-12)
  expect_equal(cl$class, cl$classes[max.col(cl$prob)])
  expect_error(morie_esl_neural_net(X, y, task = "ranking"), "task must be")
  expect_error(morie_esl_neural_net(X, y, M = 0), "at least 1")
})

test_that("RBM free energy, hidden probabilities and reconstruction", {
  V <- rbind(c(1, 0, 1, 0), c(1, 1, 0, 0), c(0, 0, 1, 1), c(1, 0, 1, 1), c(0, 1, 0, 0))
  r <- morie_esl_boltzmann(V, h = 3, n_epochs = 30, seed = 4)
  act <- sweep(V %*% r$W, 2, r$b, "+")
  expect_equal(r$hidden_prob, plogis(act), tolerance = 1e-12)
  expect_equal(r$reconstruction, plogis(sweep(r$hidden_prob %*% t(r$W), 2, r$a, "+")), tolerance = 1e-12)
  expect_equal(r$free_energy, -as.numeric(V %*% r$a) - rowSums(log1p(exp(act))), tolerance = 1e-12)
  expect_equal(r$reconstruction_error, mean((V - r$reconstruction)^2), tolerance = 1e-12)
  expect_error(morie_esl_boltzmann(V + 0.5), "binary")
  expect_error(morie_esl_boltzmann(V, h = 0), "at least 1")
})

test_that("Dirichlet-process stick breaking", {
  d <- morie_esl_dirichlet_proc(alpha = 2, n_atoms = 200, size = 50, seed = 5)
  expect_equal(sum(d$weights) + d$truncation_mass, 1, tolerance = 1e-12)
  expect_true(all(d$weights > 0))
  expect_equal(d$expected_clusters, 2 * log1p(50 / 2), tolerance = 1e-12)
  expect_equal(d$n_clusters, length(unique(d$labels)))
  expect_equal(d$samples, d$atoms[d$labels])
  expect_warning(morie_esl_dirichlet_proc(alpha = 5, n_atoms = 3, seed = 1), "unbroken")
  expect_error(morie_esl_dirichlet_proc(alpha = 0), "positive")
  g0 <- morie_esl_dirichlet_proc(1, G0 = function(k) seq_len(k) / 10, n_atoms = 60, seed = 2)
  expect_equal(g0$atoms, (1:60) / 10)
})

test_that("pairwise MRF partition function by brute force", {
  E <- rbind(c(1, 2), c(2, 3))
  P12 <- rbind(c(2, 0.5), c(0.5, 1))
  r <- morie_esl_markov_rf(E, psi = list(`1-2` = P12))
  dflt <- matrix(exp(-1), 2, 2)
  diag(dflt) <- exp(1)
  w <- numeric(8)
  k <- 0
  for (x3 in 0:1) for (x2 in 0:1) for (x1 in 0:1) {
    k <- k + 1
    w[k] <- P12[x1 + 1, x2 + 1] * dflt[x2 + 1, x3 + 1]
  }
  expect_equal(r$log_Z, log(sum(w)), tolerance = 1e-12)
  expect_equal(r$probabilities, w / sum(w), tolerance = 1e-12)
  expect_equal(rowSums(r$marginals), rep(1, 3), tolerance = 1e-12)
  adj <- matrix(0, 3, 3)
  adj[1, 2] <- adj[2, 1] <- adj[2, 3] <- adj[3, 2] <- 1
  expect_equal(morie_esl_markov_rf(adj)$log_Z, morie_esl_markov_rf(E)$log_Z, tolerance = 1e-12)
  expect_error(morie_esl_markov_rf(E, states = 1), "at least 2")
  expect_error(morie_esl_markov_rf(E, psi = list(`1-2` = diag(3))), "not 2 by 2")
})

test_that("Hyvarinen score-matching objective", {
  X <- esl_X()[, 1:2]
  sc <- function(Z) -Z
  r <- morie_esl_score_match(sc, X, grad_score = function(Z) matrix(-1, nrow(Z), ncol(Z)))
  expect_equal(r$objective, mean(-2 + 0.5 * rowSums(X^2)), tolerance = 1e-12)
  # central differences are exact for a linear score up to rounding
  expect_equal(morie_esl_score_match(sc, X)$objective, r$objective, tolerance = 1e-9)
  expect_error(morie_esl_score_match(function(Z) Z[, 1, drop = FALSE], X), "expected")
})
