# Coverage for Wallach's (2006) bigram topic model: the hierarchical
# Dirichlet predictive (eq. 6 and its interpolation form), the LDA
# predictive (eq. 13 vs the eq. 15 misprint), the bigram-topic
# predictive under priors 1 and 2, and the collapsed Gibbs sampler
# regenerated sweep by sweep with array counts and the same seeded
# stream.

test_that("Dirichlet, LDA and bigram-topic predictives", {
  n <- c(3, 0, 1)
  m <- c(0.5, 0.3, 0.2)
  d <- dirichlet_predictive(n, 4, 2, m)
  expect_equal(d$predictive, (n + 2 * m) / 6, tolerance = 1e-12)
  expect_equal(d$interpolated, d$predictive, tolerance = 1e-12)
  expect_equal(d$lambda, 1 / 3)
  expect_equal(dirichlet_predictive(c(0, 0, 0), 0, 2, m)$f, c(0, 0, 0))
  expect_error(dirichlet_predictive(n, 4, 2, m[-1]), "3 counts for 2 prior")
  expect_error(dirichlet_predictive(n, 4, 2, m * 2), "must sum to 1")
  expect_error(dirichlet_predictive(n, 4, 0, m), "beta must be positive")
  l <- lda_predictive(n, 4, 2, m)
  expect_equal(l$predictive, (n + 2 * m) / 6, tolerance = 1e-12)
  expect_equal(l$eq15_as_printed, (1 / 3) * n / 4 + (2 / 3) * m, tolerance = 1e-12)
  expect_error(lda_predictive(n, 4, -1, m), "beta must be positive")
  b <- bigram_topic_predictive(n, 4, 2, m, prior = 2)
  expect_equal(b$predictive, (n + 2 * m) / 6, tolerance = 1e-12)
  expect_match(b$smoothed_by, "varies with the topic")
  expect_error(bigram_topic_predictive(n, 4, 2, m, prior = 3), "prior must be 1 or 2")
})

.gibbs_ref <- function(docs, Tn, V, a, b, iters, burn, seed) {
  mm <- rep(1 / V, V)
  nn <- rep(1 / Tn, Tn)
  e <- .ghc_rng(seed)
  z <- lapply(docs, function(doc) as.integer(.ghc_unif(e, length(doc)) * Tn) %% Tn)
  Nijk <- array(0, c(V, V, Tn))
  Njk <- matrix(0, V, Tn)
  Nkd <- matrix(0, length(docs), Tn)
  for (d in seq_along(docs)) for (t in 2:length(docs[[d]])) {
    i <- docs[[d]][t] + 1
    j <- docs[[d]][t - 1] + 1
    k <- z[[d]][t] + 1
    Nijk[i, j, k] <- Nijk[i, j, k] + 1
    Njk[j, k] <- Njk[j, k] + 1
    Nkd[d, k] <- Nkd[d, k] + 1
  }
  for (it in seq_len(iters)) for (d in seq_along(docs)) for (t in 2:length(docs[[d]])) {
    i <- docs[[d]][t] + 1
    j <- docs[[d]][t - 1] + 1
    k <- z[[d]][t] + 1
    Nijk[i, j, k] <- Nijk[i, j, k] - 1
    Njk[j, k] <- Njk[j, k] - 1
    Nkd[d, k] <- Nkd[d, k] - 1
    p <- (Nijk[i, j, ] + b * mm[i]) / (Njk[j, ] + b) * (Nkd[d, ] + a * nn)
    u <- .ghc_unif(e, 1) * sum(p)
    k <- which(u <= cumsum(p))[1]
    if (is.na(k)) k <- Tn
    z[[d]][t] <- k - 1
    Nijk[i, j, k] <- Nijk[i, j, k] + 1
    Njk[j, k] <- Njk[j, k] + 1
    Nkd[d, k] <- Nkd[d, k] + 1
  }
  list(z = z, Nkd = Nkd)
}

test_that("the collapsed Gibbs sampler is reproducible and matches its recursion", {
  docs <- list(c(0, 1, 2, 1, 0, 3), c(2, 2, 3, 1), c(1, 0, 1, 3, 3, 2, 0))
  r <- gibbs_bigram_topic(docs, 2, 4, alpha = 0.7, beta = 0.4, iters = 5, burn = 2, seed = 9)
  ref <- .gibbs_ref(docs, 2, 4, 0.7, 0.4, 5, 2, 9)
  expect_equal(lapply(r$z, as.numeric), lapply(ref$z, as.numeric))
  expect_equal(r$theta[[2]], (ref$Nkd[2, ] + 0.7 * 0.5) / (3 + 0.7), tolerance = 1e-12)
  expect_identical(r$samples_kept, 3L)
  expect_equal(rowSums(r$topic_posterior[[1]]), rep(1, 6))
  expect_equal(sum(unlist(mget(ls(r$N_ijk), r$N_ijk))), 5 + 3 + 6)
  r2 <- gibbs_bigram_topic(docs, 2, 4, alpha = 0.7, beta = 0.4, iters = 5, burn = 2, seed = 9)
  expect_identical(r2$z, r$z)
  expect_identical(morie_bigtm, gibbs_bigram_topic)
  expect_identical(bigramtopicmodel, gibbs_bigram_topic)
  expect_identical(bigram_topic, gibbs_bigram_topic)
  expect_identical(bigramtopic, gibbs_bigram_topic)
  expect_error(gibbs_bigram_topic(list(), 2, 4), "no documents")
  expect_error(gibbs_bigram_topic(docs, 0, 4), "at least 1")
  expect_error(gibbs_bigram_topic(docs, 2, 3), "outside the vocabulary of 3")
  expect_error(gibbs_bigram_topic(docs, 2, 4, m = rep(0.5, 4)), "must each sum to 1")
  expect_error(gibbs_bigram_topic(docs, 2, 4, prior = 5), "prior must be 1 or 2")
})
