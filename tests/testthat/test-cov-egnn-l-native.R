# Coverage for E(n)-equivariant GNN layers (Satorras, Hoogeboom & Welling
# 2021). One layer is recomputed from eqs. (3)-(6) with explicit message
# functions, and the equivariance property -- x moves with any rotation and
# translation, h does not move -- is checked for random orthogonal maps.

.phi_e <- function(hi, hj, d2, a) c(tanh(sum(hi) - sum(hj) + d2), 0.5 * d2)
.phi_x <- function(m) 0.3 * m[1] - 0.1 * m[2]
.phi_h <- function(h, m) h + 0.2 * m[1]
.H <- list(c(0.1, 0.5), c(-0.3, 0.2), c(0.7, -0.4), c(0.05, 0.05))
.X <- list(c(0, 0, 1), c(1, 0.5, 0), c(-0.5, 1, 0.3), c(0.2, -1, -0.8))

test_that("one layer follows eqs. (3)-(6)", {
  r <- egcl(.H, .X, .phi_e, .phi_x, .phi_h)
  n <- 4
  for (i in 1:n) {
    acc <- .X[[i]]
    msum <- c(0, 0)
    for (j in setdiff(1:n, i)) {
      m <- .phi_e(.H[[i]], .H[[j]], sum((.X[[i]] - .X[[j]])^2), NULL)
      expect_equal(r$messages[[i]][[j]], m, tolerance = 1e-12)
      acc <- acc + (.X[[i]] - .X[[j]]) * .phi_x(m) / (n - 1)
      msum <- msum + m
    }
    expect_equal(r$X[[i]], acc, tolerance = 1e-12)
    expect_equal(r$H[[i]], .H[[i]] + 0.2 * msum[1], tolerance = 1e-12)
  }
  expect_equal(edge_message(.H[[1]], .H[[2]], .X[[1]], .X[[2]], .phi_e), .phi_e(.H[[1]], .H[[2]], sum((.X[[1]] - .X[[2]])^2), NULL))
  M <- r$messages
  expect_equal(coord_update(.X, M, .phi_x, C = 0.5)[[2]],
               .X[[2]] + 0.5 * Reduce(`+`, lapply(c(1, 3, 4), function(j) (.X[[2]] - .X[[j]]) * .phi_x(M[[2]][[j]]))), tolerance = 1e-12)
  expect_error(coord_update(.X[1], M, .phi_x), "at least 2 particles")
  expect_error(egcl(.H, .X, .phi_e, .phi_x, .phi_h, mode = "force"), "mode must be one of")
})

test_that("the momentum variant integrates velocities with step dt", {
  V <- list(c(0.1, 0, 0), c(0, 0.1, 0), c(0, 0, 0.1), c(0.1, 0.1, 0.1))
  pv <- function(h) 1 + 0.1 * sum(h)
  r <- egcl(.H, .X, .phi_e, .phi_x, .phi_h, V = V, mode = "momentum", phi_v = pv, dt = 0.5)
  for (i in 1:4) {
    acc <- pv(.H[[i]]) * V[[i]]
    for (j in setdiff(1:4, i)) {
      m <- .phi_e(.H[[i]], .H[[j]], sum((.X[[i]] - .X[[j]])^2), NULL)
      acc <- acc + (.X[[i]] - .X[[j]]) * .phi_x(m) / 3
    }
    expect_equal(r$V[[i]], acc, tolerance = 1e-12)
    expect_equal(r$X[[i]], .X[[i]] + 0.5 * acc, tolerance = 1e-12)
  }
  expect_error(egcl(.H, .X, .phi_e, .phi_x, .phi_h, mode = "momentum"), "needs V and phi_v")
})

test_that("stacked layers compose and every entry point is the same network", {
  one <- egcl(.H, .X, .phi_e, .phi_x, .phi_h)
  two <- egcl(one$H, one$X, .phi_e, .phi_x, .phi_h)
  r <- run_egnn(.H, .X, 2, .phi_e, .phi_x, .phi_h)
  expect_equal(r$X, two$X, tolerance = 1e-12)
  expect_equal(r$H, two$H, tolerance = 1e-12)
  for (f in list(egnn_layer, egnnlayer, equivariantgnn, morie_egnnL)) {
    expect_equal(f(.H, .X, 2, .phi_e, .phi_x, .phi_h)$X, r$X, tolerance = 1e-12)
  }
})

test_that("coordinates are E(n)-equivariant and features invariant", {
  set.seed(1)
  Q <- qr.Q(qr(matrix(stats::rnorm(9), 3)))
  g <- c(0.4, -1.2, 2)
  ql <- lapply(1:3, function(a) as.numeric(Q[a, ]))
  e <- morie_egnnL_equivariance_error(.H, .X, .phi_e, .phi_x, .phi_h, ql, g, layers = 3)
  expect_lt(e$coordinate_error, 1e-12)
  expect_lt(e$feature_error, 1e-12)
  expect_true(e$equivariant && e$invariant)
  base <- run_egnn(.H, .X, 3, .phi_e, .phi_x, .phi_h)
  moved <- run_egnn(.H, lapply(.X, function(x) as.numeric(Q %*% x) + g), 3, .phi_e, .phi_x, .phi_h)
  expect_equal(moved$X, lapply(base$X, function(x) as.numeric(Q %*% x) + g), tolerance = 1e-12)
  expect_equal(moved$H, base$H, tolerance = 1e-12)
})
