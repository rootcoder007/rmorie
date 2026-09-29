# Coverage for latent semantic analysis (Deerwester et al. 1990; Dumais
# 1991): raw, smoothed tf-idf and log-entropy weighting, the truncated SVD
# (full rank reproduces the weighted matrix; rank k is the Eckart-Young
# approximation), query fold-in q' T_k S_k^-1 and cosine ranking of the
# documents in the scaled latent space.

.X <- rbind(c(2, 0, 1, 0), c(0, 3, 0, 1), c(1, 1, 0, 0), c(0, 0, 4, 2), c(1, 0, 0, 1))

test_that("term weightings", {
  expect_equal(term_weighting(.X, "raw"), .X)
  tf <- term_weighting(.X, "tfidf")
  df <- rowSums(.X > 0)
  expect_equal(tf, .X * (log(5 / (1 + df)) + 1), tolerance = 1e-12)
  le <- term_weighting(.X)
  P <- .X / rowSums(.X)
  H <- rowSums(ifelse(P > 0, P * log(P), 0))
  expect_equal(le, (1 + H / log(4)) * log1p(.X), tolerance = 1e-12)
  z <- term_weighting(rbind(c(0, 0), c(1, 1)))
  expect_equal(z[1, ], c(0, 0))
  expect_error(term_weighting(.X, "bm25"), "weighting must be one of")
})

test_that("the truncated SVD reconstructs the Eckart-Young approximation", {
  A <- term_weighting(.X)
  full <- lsa_decompose(.X)
  expect_identical(full$full_rank, 4L)
  expect_equal(reconstruct(full), A, tolerance = 1e-12)
  m <- lsa_decompose(.X, k_dim = 2)
  s <- svd(A)
  expect_equal(m$S, s$d[1:2], tolerance = 1e-12)
  expect_equal(reconstruct(m), s$u[, 1:2] %*% diag(s$d[1:2]) %*% t(s$v[, 1:2]), tolerance = 1e-12)
  expect_equal(sqrt(sum((reconstruct(m) - A)^2)), sqrt(sum(s$d[3:4]^2)), tolerance = 1e-12)
  expect_identical(lsa, lsa_decompose)
  expect_identical(latentsemantic, lsa_decompose)
  expect_error(lsa_decompose(.X, k_dim = 5), "k must lie in 1..4")
})

test_that("fold-in and cosine ranking", {
  m <- lsa_decompose(.X, k_dim = 2, how = "raw")
  q <- c(1, 0, 1, 0, 0)
  qh <- fold_in(q, m)
  expect_equal(qh, as.numeric(q %*% m$T) / m$S, tolerance = 1e-12)
  # folding in a document column recovers its row of D
  expect_equal(fold_in(.X[, 3], m), m$D[3, ], tolerance = 1e-12)
  r <- cosine_ranking(qh, m, top_k = 3)
  DS <- sweep(m$D, 2, m$S, "*")
  cs <- as.numeric(DS %*% qh) / (sqrt(rowSums(DS^2)) * sqrt(sum(qh^2)))
  expect_equal(r$scores, sort(cs, decreasing = TRUE), tolerance = 1e-12)
  expect_identical(unlist(r$ranking), order(-cs)[1:3])
  expect_equal(cosine_ranking(c(0, 0), m)$scores, rep(0, 4))
  mm <- morie_lsa(.X, k_dim = 2, how = "raw", query = q, top_k = 3)
  expect_identical(mm$ranking, r$ranking)
  expect_equal(morie_lsa(.X, k_dim = 2)$S, lsa_decompose(.X, 2)$S)
  expect_error(fold_in(1:3, m), "3 terms but the model has 5")
})
