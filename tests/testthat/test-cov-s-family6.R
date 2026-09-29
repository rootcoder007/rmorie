# Coverage for setT, sgflrt, sgpr, sgtbtw, sgtcheb, sgtcheeg exports.
# Every expectation is recomputed in the test body.

test_that("Set Transformer PMA is MAB(S, rFF(Z)) with post-norm layers", {
  Z <- rbind(c(0.2, -0.5), c(1.0, 0.3), c(-0.4, 0.8), c(0.6, 0.1))
  S <- rbind(c(0.1, 0.2))
  p <- list(Wq = diag(2), Wk = matrix(c(0.5, 0.2, -0.3, 0.8), 2), Wv = matrix(c(1, 0, 0.4, 1), 2),
            W1 = matrix(c(0.7, -0.2, 0.1, 0.5, 0.3, -0.6), 2), b1 = c(0.1, 0, -0.1),
            W2 = matrix(c(0.4, 0.2, -0.5, 0.3, 0.6, 0.1), 3), b2 = c(0.05, -0.05))
  ln <- function(M) t(apply(M, 1, function(r) (r - mean(r)) / sqrt(mean((r - mean(r))^2) + 1e-5)))
  rff <- function(M) sweep(pmax(sweep(M %*% p$W1, 2, p$b1, "+"), 0) %*% p$W2, 2, p$b2, "+")
  FZ <- rff(Z)
  Sc <- (S %*% p$Wq) %*% t(FZ %*% p$Wk) / sqrt(2)
  W <- exp(Sc - max(Sc)) / sum(exp(Sc - max(Sc)))
  H <- ln(S + W %*% (FZ %*% p$Wv))
  O <- ln(H + rff(H))
  for (fn in list(morie_setT_setT, setT)) {
    r <- fn(Z, S, p)
    expect_equal(r$output, O, tolerance = 1e-12)
    expect_equal(r$attention, W, tolerance = 1e-12)
  }
  expect_equal(set_transformer(X = Z, k = 1, S = S, params = p)$output, O, tolerance = 1e-12)
  expect_equal(morie_setT_set_transformer(X = Z, S = S, params = p)$output, O, tolerance = 1e-12)
  expect_error(set_transformer(X = Z, k = 2, S = S, params = p), "seed rows but k")
  expect_error(set_transformer(X = Z), "required")
  expect_error(setT(Z, cbind(S, 1), p), "Z width")
  expect_error(setT(Z, S, p[-1]), "missing Wq")
  expect_error(morie_setT_setT(Z, S, p[-4]), "missing W1")
})

test_that("Sparsegp gives the FITC and DTC predictive equations", {
  X <- cbind(c(0, 0.3, 0.7, 1.1, 1.6, 2.0, 2.4))
  y <- sin(X[, 1]) + c(0.05, -0.02, 0.03, 0, -0.04, 0.02, 0.01)
  Xt <- cbind(c(0.5, 1.8))
  r <- Sparsegp(X, y, M = 3, X_test = Xt, gamma = 0.8, sigma2 = 0.01)
  idx <- round((0:2) * 6 / 2) + 1
  Zi <- X[idx, , drop = FALSE]
  k <- function(A, B) exp(-0.8 * outer(A[, 1], B[, 1], "-")^2)
  Kmm <- k(Zi, Zi) + diag(1e-8, 3)
  Knm <- k(X, Zi)
  Ktm <- k(Xt, Zi)
  pred <- function(fitc) {
    q <- rowSums((Knm %*% solve(Kmm)) * Knm)
    lam <- (if (fitc) pmax(1 - q, 0) else 0) + 0.01
    Sg <- solve(Kmm + t(Knm) %*% (Knm / lam))
    mu <- Ktm %*% Sg %*% t(Knm) %*% (y / lam)
    v <- 1 - rowSums((Ktm %*% solve(Kmm)) * Ktm) + rowSums((Ktm %*% Sg) * Ktm)
    list(mu = as.numeric(mu), v = v)
  }
  f <- pred(TRUE)
  d <- pred(FALSE)
  expect_equal(r$inducing, idx - 1L)
  expect_equal(r$pred_fitc, f$mu, tolerance = 1e-8)
  expect_equal(r$pred_dtc, d$mu, tolerance = 1e-8)
  expect_equal(r$var_fitc, f$v, tolerance = 1e-8)
  expect_equal(r$var_dtc, d$v, tolerance = 1e-8)
})

test_that("Btwcent matches igraph betweenness", {
  A <- matrix(0, 6, 6)
  ed <- rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6), c(5, 6))
  A[ed] <- 1
  A[ed[, 2:1]] <- 1
  r <- Btwcent(A, normalise = TRUE)
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected")
  expect_equal(r$raw, igraph::betweenness(g), tolerance = 1e-12)
  expect_equal(r$betweenness, igraph::betweenness(g) / 10, tolerance = 1e-12)
})

test_that("Cheeger constant by enumeration and by the Fiedler sweep", {
  W <- matrix(0, 6, 6)
  ed <- rbind(c(1, 2, 1), c(1, 3, 2), c(2, 3, 1), c(3, 4, 0.5), c(4, 5, 1), c(4, 6, 1), c(5, 6, 2))
  W[ed[, 1:2]] <- ed[, 3]
  W[ed[, 2:1]] <- ed[, 3]
  d <- rowSums(W)
  hmin <- Inf
  for (m in 1:31) {
    S <- c(1, which(as.integer(intToBits(m))[1:5] == 1) + 1)
    if (length(S) == 6) next
    hmin <- min(hmin, sum(W[S, -S]) / min(sum(d[S]), sum(d[-S])))
  }
  r <- Sgtcheegerbound(W)
  expect_equal(r$h, hmin, tolerance = 1e-12)
  Ln <- diag(6) - W / sqrt(outer(d, d))
  lam <- sort(eigen(Ln, symmetric = TRUE)$values)
  expect_equal(r$lambda1, lam[2], tolerance = 1e-10)
  # Cheeger inequalities h^2 / 2 <= lambda1 <= 2 h
  expect_true(r$lower_bound <= lam[2] && lam[2] <= r$upper_bound)
  expect_error(Sgtcheegerbound(W, max_n = 5), "refused above")
  c2 <- Sgtcheegerconstant(W)
  expect_equal(c2$lambda2, lam[2], tolerance = 1e-10)
  S <- c2$cut_set + 1
  expect_equal(c2$sweep_min, sum(W[S, -S]) / min(sum(d[S]), sum(d[-S])), tolerance = 1e-12)
  expect_gte(c2$sweep_min, hmin - 1e-12)
  expect_lte(c2$sweep_min, sqrt(2 * lam[2]) + 1e-12)
  expect_error(Sgtcheegerconstant(matrix(0, 3, 3)), "isolated vertices")
})

test_that("spatial GLMM: Gaussian GLS identity and Poisson score equations", {
  C <- cbind(c(0, 1, 2, 0.5, 1.5, 2.5, 0.2, 1.8), c(0, 0.5, 0, 1.5, 1, 1.2, 2, 2.2))
  x <- c(0.2, 1.1, -0.5, 0.8, 1.9, -1.0, 0.3, 0.6)
  X <- cbind(1, x)
  y <- c(1.3, 2.2, 0.1, 1.9, 3.5, -0.2, 0.9, 1.6)
  g <- morie_sgflrt_spatial_glmm_fit(y, X, C, family = "gaussian", sigma2 = 0.8, phi = 1.2, dispersion = 0.3)
  D <- as.matrix(stats::dist(C))
  V <- 0.8 * exp(-D / 1.2) + diag(0.3, 8)
  Vi <- solve(V)
  b <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  # the jittered Cholesky (1e-10 relative) perturbs the solution slightly
  expect_equal(g$coefficients, as.numeric(b), tolerance = 1e-8)
  expect_equal(g$spatial_effect, as.numeric(0.8 * exp(-D / 1.2) %*% Vi %*% (y - X %*% b)), tolerance = 1e-8)
  expect_lt(g$gls_identity_gap, 1e-8)
  if (requireNamespace("mvtnorm", quietly = TRUE)) {
    # for a Gaussian response the Laplace approximation is exact
    expect_equal(g$laplace_loglik, mvtnorm::dmvnorm(y, as.numeric(X %*% b), V, log = TRUE), tolerance = 1e-8)
  }
  yc <- c(2, 5, 0, 3, 9, 0, 1, 4)
  p <- morie_sgflrt_spatial_glmm_fit(yc, X, C, family = "poisson", sigma2 = 0.5, phi = 1)
  mu <- p$fitted
  expect_lt(max(abs(crossprod(X, yc - mu))), 1e-7)
  R <- 0.5 * exp(-D / 1)
  # the mode also solves u = Sigma (y - mu)
  expect_equal(p$spatial_effect, as.numeric(R %*% (yc - mu)), tolerance = 1e-6)
  p0 <- morie_sgflrt_spatial_glmm_fit(yc, X, C, family = "poisson", sigma2 = 0)
  expect_equal(p0$coefficients, unname(stats::coef(stats::glm(yc ~ x, family = stats::poisson()))), tolerance = 1e-6)
  expect_error(morie_sgflrt_spatial_glmm_fit(y, X, C, family = "gamma"), "family must be")
  expect_error(morie_sgflrt_spatial_glmm_fit(yc, X, C, dispersion = 2), "only the gaussian family")
  expect_error(morie_sgflrt_spatial_glmm_fit(c(0.5, yc[-1]), X, C), "non-negative counts")
  expect_error(morie_sgflrt_spatial_glmm_fit(y[-1], X, C), "design rows")
  expect_error(morie_sgflrt_spatial_glmm_fit(yc, X, C, nugget = -1), "nugget cannot be negative")
})
