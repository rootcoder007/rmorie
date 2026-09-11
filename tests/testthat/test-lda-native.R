# Anchors for Latent Dirichlet Allocation by variational EM
# (Blei, Ng & Jordan 2003).
#
# Word indices are 0-based throughout this module, which its own
# validation states by accepting 0 <= w < V. They were then used directly
# as R column subscripts, which are 1-based, so word 0 selected an empty
# column and reported itself as having zero probability under every topic,
# and every other word read the column belonging to its predecessor. The
# tests below pin the convention at each place a word index is used.

test_that("E[log theta] is the digamma difference", {
  for (g in list(c(0.5, 2, 3.5), c(1, 1, 1), c(0.01, 10), rep(2, 5))) {
    expect_equal(as.numeric(.morie_lda_e_log_theta(g)),
                 digamma(g) - digamma(sum(g)), tolerance = 1e-9)
  }
  # a symmetric Dirichlet gives every component the same value
  v <- as.numeric(.morie_lda_e_log_theta(rep(3, 4)))
  expect_equal(v, rep(v[1], 4), tolerance = 1e-12)
  # and each component is negative, being the log of something under one
  expect_true(all(v < 0))
})

# a codebook where topic k emits word k with certainty
identity_beta <- function(K) {
  B <- diag(K) * (1 - 1e-9) + 1e-9 / K
  B / rowSums(B)
}

test_that("each word reads its own column of beta", {
  # the regression, stated so that an off-by-one cannot pass: with topic k
  # emitting word k, a document of word j must put all of its weight on
  # topic j
  K <- 5
  B <- identity_beta(K)
  for (j in 0:(K - 1)) {
    r <- .morie_lda_variational_inference(rep(j, 4), alpha = 0.1, beta = B)
    phi <- if (is.matrix(r$phi)) r$phi else do.call(rbind, r$phi)
    expect_equal(as.numeric(phi[1, ]),
                 as.numeric(seq_len(K) == (j + 1)), tolerance = 1e-6)
  }
})

test_that("word 0 is a legal word", {
  # it used to select B[, 0], an empty matrix, and raise
  K <- 3
  B <- matrix(c(0.7, 0.2, 0.1, 0.1, 0.8, 0.1, 0.2, 0.2, 0.6), K, 3, byrow = TRUE)
  expect_silent(r <- .morie_lda_variational_inference(c(0, 0, 1), 0.1, B))
  phi <- if (is.matrix(r$phi)) r$phi else do.call(rbind, r$phi)
  expect_identical(dim(phi), c(3L, 3L))
  expect_true(all(is.finite(phi)))
  # and the last word of the vocabulary is legal too
  expect_silent(.morie_lda_variational_inference(c(2, 2), 0.1, B))
  # while one past it is not
  expect_error(.morie_lda_variational_inference(c(3), 0.1, B),
               "outside the vocabulary")
  expect_error(.morie_lda_variational_inference(c(-1), 0.1, B),
               "outside the vocabulary")
})

test_that("the variational parameters satisfy their own updates", {
  set.seed(1)
  K <- 3; V <- 8
  B <- matrix(runif(K * V), K, V); B <- B / rowSums(B)
  doc <- c(0, 0, 2, 4, 4, 4, 6)
  r <- .morie_lda_variational_inference(doc, alpha = 0.1, beta = B)
  phi <- if (is.matrix(r$phi)) r$phi else do.call(rbind, r$phi)
  # phi is a distribution over topics for each word
  expect_equal(as.numeric(rowSums(phi)), rep(1, length(doc)), tolerance = 1e-10)
  expect_true(all(phi >= 0))
  # gamma is alpha plus the topic mass assigned to the document
  expect_equal(as.numeric(unlist(r$gamma)), 0.1 + as.numeric(colSums(phi)),
               tolerance = 1e-7)
  expect_identical(as.integer(r$N), length(doc))
  expect_identical(as.integer(r$K), as.integer(K))
})

test_that("the bound never decreases across inner iterations", {
  set.seed(1)
  K <- 3; V <- 8
  B <- matrix(runif(K * V), K, V); B <- B / rowSums(B)
  doc <- c(0, 0, 2, 4, 4, 4, 6)
  els <- vapply(1:12, function(it) {
    v <- .morie_lda_variational_inference(doc, 0.1, B, iters = it)
    p <- if (is.matrix(v$phi)) v$phi else do.call(rbind, v$phi)
    as.numeric(.morie_lda_elbo(doc, 0.1, B, p, as.numeric(unlist(v$gamma))))
  }, numeric(1))
  expect_true(all(diff(els) >= -1e-9))
  expect_gt(els[12], els[1])
})

planted_corpus <- function() {
  set.seed(2)
  list(V = 12L, K = 3L,
       docs = c(replicate(8, sample(0:3, 10, TRUE), simplify = FALSE),
                replicate(8, sample(4:7, 10, TRUE), simplify = FALSE),
                replicate(8, sample(8:11, 10, TRUE), simplify = FALSE)))
}

test_that("variational EM recovers a planted topic structure", {
  p <- planted_corpus()
  fit <- .morie_lda_variational_em(p$docs, K = p$K, V = p$V, alpha = 0.1,
                                   iters = 25, inner = 30)
  B <- as.matrix(fit$beta)
  expect_identical(dim(B), c(p$K, p$V))
  # each row of beta is a distribution over the vocabulary
  expect_equal(as.numeric(rowSums(B)), rep(1, p$K), tolerance = 1e-8)
  expect_true(all(B >= 0))
  # the three blocks of words separate cleanly, one block per topic
  blocks <- list(0:3, 4:7, 8:11)
  assigned <- vapply(seq_len(p$K), function(k) {
    top <- order(B[k, ], decreasing = TRUE)[1:4] - 1L
    which(vapply(blocks, function(b) setequal(top, b), logical(1)))[1]
  }, numeric(1))
  expect_setequal(assigned, 1:3)
  expect_identical(as.integer(fit$n_docs), length(p$docs))
})

test_that("the EM bound never decreases", {
  p <- planted_corpus()
  fit <- .morie_lda_variational_em(p$docs, K = p$K, V = p$V, alpha = 0.1,
                                   iters = 25, inner = 30)
  h <- as.numeric(unlist(fit$elbo_history))
  expect_gt(length(h), 1L)
  expect_true(all(diff(h) >= -1e-6))
  expect_equal(h[length(h)], as.numeric(fit$final_elbo), tolerance = 1e-8)
  # the bound improves over the run
  expect_gt(h[length(h)], h[1])
})

test_that("the top words are reported on the same 0-based scale as the input", {
  p <- planted_corpus()
  fit <- .morie_lda_variational_em(p$docs, K = p$K, V = p$V, alpha = 0.1,
                                   iters = 25, inner = 30)
  B <- as.matrix(fit$beta)
  bare <- .morie_lda_topic_words(B, n_top = 2)
  named <- .morie_lda_topic_words(B, n_top = 2, vocab = paste0("w", 0:11))
  for (k in seq_along(bare)) {
    for (m in seq_along(bare[[k]])) {
      idx <- bare[[k]][[m]][[1]]
      # a word index, so inside 0..V-1
      expect_true(idx >= 0 && idx < p$V)
      # and the labelled form names the same word
      expect_identical(named[[k]][[m]][[1]], paste0("w", idx))
      # both report the same probability, which is beta at that word
      expect_equal(bare[[k]][[m]][[2]], B[k, idx + 1L], tolerance = 1e-12)
    }
  }
  # the words are given in decreasing probability
  for (k in seq_along(bare)) {
    pr <- vapply(bare[[k]], function(e) e[[2]], numeric(1))
    expect_true(all(diff(pr) <= 0))
  }
})

test_that("the arguments are validated", {
  B <- matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2)
  expect_error(.morie_lda_variational_inference(integer(0), 0.1, B),
               "document is empty")
  expect_error(.morie_lda_variational_inference(c(0), c(0.1, 0.1, 0.1), B),
               "alpha has 3 entries")
  expect_error(.morie_lda_variational_inference(c(0), c(0, 0.1), B),
               "alpha must be strictly positive")
  expect_error(.morie_lda_variational_em(list(), K = 2, V = 2),
               "no documents given")
  expect_error(.morie_lda_variational_em(list(c(0)), K = 0, V = 2),
               "at least 1")
})
