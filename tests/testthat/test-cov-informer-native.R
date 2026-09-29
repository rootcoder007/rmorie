# Coverage tests for R/informer_native.R (Zhou et al. 2021, Informer):
# the query sparsity measure, ProbSparse query selection and attention.

inf_Q <- rbind(c(0.5, -0.2, 0.1), c(2, 1.5, -1), c(0.01, 0.02, 0), c(-1.2, 0.8, 0.6), c(0.3, 0.3, 0.3))
inf_K <- rbind(c(1, 0, 0.5), c(-0.5, 1, 0.2), c(0.3, -0.7, 1), c(0.8, 0.4, -0.3))
inf_V <- rbind(c(1, 2), c(0, -1), c(3, 0.5), c(-2, 1))

test_that("sparsity measure: log-sum-exp minus mean, and the max-mean bound", {
  z <- as.numeric(inf_K %*% inf_Q[2, ]) / sqrt(3)
  expect_equal(morie_informer_sparsity_measure(inf_Q[2, ], inf_K), log(sum(exp(z))) - mean(z), tolerance = 1e-12)
  expect_equal(morie_informer_sparsity_measure(inf_Q[2, ], inf_K, "maxmean"), max(z) - mean(z), tolerance = 1e-12)
  expect_equal(morie_informer_kl_from_uniform(inf_Q[2, ], inf_K), log(sum(exp(z))) - mean(z) - log(4), tolerance = 1e-12)
  # KL from uniform is zero for a query orthogonal to every key
  expect_equal(morie_informer_kl_from_uniform(c(0, 0, 0), inf_K), 0, tolerance = 1e-12)
  expect_error(morie_informer_sparsity_measure(inf_Q[2, ], inf_K, "top"), "exact or maxmean")
  expect_error(morie_informer_sparsity_measure(1:2, inf_K), "2 dimensions")
})

test_that("complexity accounting and query selection", {
  cx <- morie_informer_complexity(96, 96, factor = 5)
  u <- min(96L, as.integer(5 * log(96)))
  expect_equal(cx$u, u)
  expect_equal(cx$probsparse, u * 96L)
  expect_equal(cx$ratio, 96 / u)
  s <- morie_informer_select_queries(inf_Q, inf_K, factor = 1)
  sc <- apply(inf_Q, 1, function(q) {
    z <- as.numeric(inf_K %*% q) / sqrt(3)
    max(z) - mean(z)
  })
  expect_equal(s$scores, sc, tolerance = 1e-12)
  expect_equal(s$u, as.integer(log(5)))
  expect_equal(s$top, sort(order(sc, decreasing = TRUE)[seq_len(s$u)]))
  ss <- morie_informer_select_queries(inf_Q, inf_K, factor = 1, n_sample = 2, seed = 3)
  idx <- order(.ghc_unif(.ghc_rng(3), 4))[1:2]
  expect_equal(ss$scores, apply(inf_Q, 1, function(q) {
    z <- as.numeric(inf_K[idx, ] %*% q) / sqrt(3)
    max(z) - mean(z)
  }), tolerance = 1e-12)
  expect_equal(ss$n_sample, 2L)
  expect_error(morie_informer_select_queries(matrix(0, 0, 3), inf_K), "non-empty")
})

test_that("full and ProbSparse attention", {
  full <- morie_informer_full_attention(inf_Q, inf_K, inf_V)
  ref <- lapply(1:5, function(i) {
    z <- as.numeric(inf_K %*% inf_Q[i, ]) / sqrt(3)
    w <- exp(z - max(z)) / sum(exp(z - max(z)))
    colSums(inf_V * w)
  })
  expect_equal(full, ref, tolerance = 1e-12)
  ps <- morie_informer_probsparse_attention(inf_Q, inf_K, inf_V, factor = 1)
  for (i in 1:5) {
    expect_equal(ps$output[[i]], if (i %in% ps$selected) ref[[i]] else colMeans(inf_V), tolerance = 1e-12)
  }
  # u = L_Q recovers full attention exactly
  expect_equal(morie_informer_probsparse_attention(inf_Q, inf_K, inf_V, factor = 10)$output, ref, tolerance = 1e-12)
  expect_error(morie_informer_full_attention(inf_Q, inf_K, inf_V[1:3, ]), "must match")
  expect_match(morie_informer_cheatsheet(), "ProbSparse")
})
