# Coverage for GRACE (Zhu et al. 2020): edge dropping and feature-
# dimension masking driven by the counter generator, view generation
# (mask first, then drop), and the symmetric InfoNCE objective with
# inter- and intra-view negatives recomputed from normalised embeddings.

test_that("edges survive with u >= p and masked feature columns are zeroed", {
  ed <- list(c(0, 1), c(1, 2), c(2, 3), c(3, 0), c(0, 2))
  u <- .ghc_unif(.ghc_rng(3), 5)
  expect_identical(morie_drop_edges(ed, 0.4, .ghc_rng(3)), ed[u >= 0.4])
  expect_identical(morie_drop_edges(list(), 0.4, .ghc_rng(3)), list())
  expect_error(morie_drop_edges(ed, 1, .ghc_rng(3)), "\\[0,1\\)")
  X <- matrix(1:12, 3)
  m <- morie_mask_features(X, 0.5, .ghc_rng(8))
  k <- as.numeric(.ghc_unif(.ghc_rng(8), 4) >= 0.5)
  expect_equal(m$kept, k)
  expect_equal(m$X, sweep(X, 2, k, "*"))
  expect_identical(m$n_masked, as.integer(4 - sum(k)))
  expect_error(morie_mask_features(X, -0.1, .ghc_rng(1)), "\\[0,1\\)")
  e <- .ghc_rng(5)
  v <- morie_generate_view(X, ed, 0.3, 0.5, .ghc_rng(5))
  mm <- morie_mask_features(X, 0.5, e)
  expect_equal(v$X, mm$X)
  expect_identical(v$edges, morie_drop_edges(ed, 0.3, e))
})

.nce <- function(U, V, tau, intra) {
  Un <- U / sqrt(rowSums(U^2))
  Vn <- V / sqrt(rowSums(V^2))
  l <- function(A, B) {
    S <- exp(A %*% t(B) / tau)
    Sa <- exp(A %*% t(A) / tau)
    d <- diag(S)
    -log(d / (rowSums(S) + if (intra) rowSums(Sa) - diag(Sa) else 0))
  }
  list(u = l(Un, Vn), v = l(Vn, Un))
}

test_that("the objective is the symmetric InfoNCE with intra-view negatives", {
  set.seed(4)
  U <- matrix(stats::rnorm(15), 5)
  V <- U + matrix(stats::rnorm(15, sd = 0.3), 5)
  r <- .nce(U, V, 0.4, TRUE)
  expect_equal(morie_pair_loss(U, V, 2, tau = 0.4), r$u[2], tolerance = 1e-12)
  g <- morie_grace(U, V, tau = 0.4)
  expect_equal(g$loss, mean(c(r$u, r$v)), tolerance = 1e-12)
  ri <- .nce(U, V, 0.4, FALSE)
  expect_equal(morie_graphcontrastive(U, V, tau = 0.4, intra = FALSE)$loss, mean(c(ri$u, ri$v)), tolerance = 1e-12)
  expect_lt(morie_grace(U, U)$loss, morie_grace(U, V[5:1, ])$loss)
  expect_error(morie_grace(U, V[-1, ]), "5 and 4 nodes")
  expect_error(morie_grace(U[1, , drop = FALSE], V[1, , drop = FALSE]), "at least 2 nodes")
  expect_error(morie_pair_loss(U, V, 1, tau = 0), "temperature must be positive")
  expect_error(morie_pair_loss(rbind(0, 1), rbind(1, 1), 1), "non-zero vectors")
})
