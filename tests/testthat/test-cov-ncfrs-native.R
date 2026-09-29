# Neural collaborative filtering (He et al. 2017): GMF recovering matrix
# factorisation, the ReLU MLP tower, NeuMF fusion, the log loss, and one
# replayed SGD step of the GMF fit.

test_that("GMF with identity output and h = 1 is matrix factorisation", {
  p <- c(0.3, -1.2, 0.8)
  q <- c(1.1, 0.4, -0.5)
  expect_equal(morie_ncfRS_gmf(p, q, activation = "identity"), sum(p * q), tolerance = 1e-15)
  h <- c(2, 0.5, -1)
  expect_equal(morie_ncfRS_gmf(p, q, h), stats::plogis(sum(h * p * q)), tolerance = 1e-15)
  expect_error(morie_ncfRS_gmf(p, q[-1]), "differ in length")
  expect_error(morie_ncfRS_gmf(p, q, activation = "tanh"), "identity or sigmoid")
})

test_that("the MLP tower applies ReLU layers to the concatenation; NeuMF fuses late", {
  p <- c(0.3, -1.2)
  q <- c(1.1, 0.4)
  W1 <- matrix(c(0.5, -0.2, 0.1, 0.4, -0.3, 0.2, 0.6, -0.1), 2)
  b1 <- c(0.1, -0.2)
  W2 <- matrix(c(1, -1), 1)
  b2 <- 0.05
  z1 <- pmax(0, as.numeric(W1 %*% c(p, q)) + b1)
  z2 <- pmax(0, as.numeric(W2 %*% z1) + b2)
  expect_equal(morie_ncfRS_mlp_layers(p, q, list(W1, W2), list(b1, b2)), z2, tolerance = 1e-15)
  h <- c(0.4, -0.3, 1.5)
  pg <- c(0.2, 0.9)
  qg <- c(-0.5, 0.7)
  nm <- morie_ncfRS_neumf(pg, qg, p, q, list(W1, W2), list(b1, b2), h)
  expect_equal(nm$score, stats::plogis(sum(h * c(pg * qg, z2))), tolerance = 1e-15)
  expect_error(morie_ncfRS_neumf(pg, qg, p, q, list(W1, W2), list(b1, b2), h[-1]), "h has 2 entries")
})

test_that("the log loss is the Bernoulli negative log-likelihood", {
  expect_equal(morie_ncfRS_log_loss(1, 0.8), -log(0.8), tolerance = 1e-15)
  expect_equal(morie_ncfRS_log_loss(0, 0.8), -log(0.2), tolerance = 1e-15)
  expect_equal(morie_ncfRS_log_loss(1, 0), -log(1e-12), tolerance = 1e-15)
})

test_that("one GMF SGD step follows d(log loss)/dz = sigma(z) - y", {
  pos <- list("1" = c(2L), "3" = c(1L, 4L))
  f <- morie_ncfRS_fit_gmf(pos, n_users = 3, n_items = 4, k_dim = 2, alpha = 0.1,
                           iters = 1, n_neg = 2, seed = 5)
  e <- .ghc_rng(5)
  P <- t(vapply(1:3, function(u) (.ghc_unif(e, 2L) - 0.5) * 0.2, numeric(2)))
  Q <- t(vapply(1:4, function(i) (.ghc_unif(e, 2L) - 0.5) * 0.2, numeric(2)))
  h <- c(1, 1)
  users <- c(1L, 3L)
  u <- users[as.integer(.ghc_unif(e, 1) * 2) %% 2 + 1]
  seen <- pos[[as.character(u)]]
  items <- seen
  lab <- rep(1, length(seen))
  for (k in 1:2) {
    j <- as.integer(.ghc_unif(e, 1) * 4) %% 4 + 1
    if (!(j %in% seen)) {
      items <- c(items, j)
      lab <- c(lab, 0)
    }
  }
  for (k in seq_along(items)) {
    i <- items[k]
    g <- stats::plogis(sum(h * P[u, ] * Q[i, ])) - lab[k]
    P[u, ] <- P[u, ] - 0.1 * g * h * Q[i, ]
    Q[i, ] <- Q[i, ] - 0.1 * g * h * P[u, ]
    h <- h - 0.1 * g * P[u, ] * Q[i, ]
  }
  expect_equal(f$P, P, tolerance = 1e-15)
  expect_equal(f$Q, Q, tolerance = 1e-15)
  expect_equal(f$h, h, tolerance = 1e-15)
  L <- mean(unlist(lapply(users, function(uu) vapply(1:4, function(i)
    morie_ncfRS_log_loss(as.numeric(i %in% pos[[as.character(uu)]]),
                         morie_ncfRS_gmf(P[uu, ], Q[i, ], h)), 1))))
  expect_equal(f$final_loss, L, tolerance = 1e-14)
  long <- morie_ncfRS(pos, 3, 4, k_dim = 3, iters = 600, seed = 1)
  expect_lt(long$final_loss, long$loss_history[1])
  expect_identical(morie_ncfRS_ncf, morie_ncfRS_fit_gmf)
  expect_error(morie_ncfRS_fit_gmf(pos, 1, 1), "at least 1 user, 2 items")
  expect_error(morie_ncfRS_fit_gmf(list(1, 2), 3, 4), "named list")
  expect_match(morie_ncfRS_cheatsheet(), "ASSUMPTION", fixed = TRUE)
})
