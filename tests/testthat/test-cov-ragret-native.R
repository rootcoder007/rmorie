# Retrieval for RAG (Lewis et al. 2020; Johnson, Douze & Jegou 2019):
# exact top-k, the IVF index as a k-means fixed point, probing all cells
# equals exact search, recall at k and the RAG-Sequence / RAG-Token
# marginalisations.

rg_corpus <- list(c(1, 0, 0.2), c(0.9, 0.1, 0), c(0, 1, 0.3), c(0.1, 0.8, 0.5),
                  c(-1, 0.2, 0), c(-0.8, -0.1, 0.1), c(0.5, 0.5, 0.5))

test_that("normalise and exact top-k by inner product or cosine", {
  expect_equal(morie_ragRet_normalise(c(3, 4)), c(0.6, 0.8))
  expect_error(morie_ragRet_normalise(c(0, 0)), "zero vector")
  q <- c(0.7, 0.2, 0.1)
  M <- do.call(rbind, rg_corpus)
  s <- as.numeric(M %*% q)
  r <- morie_ragRet_top_k(q, rg_corpus, 3)
  expect_identical(r$indices, order(-s)[1:3] - 1L)
  expect_equal(r$scores, sort(s, decreasing = TRUE)[1:3], tolerance = 1e-15)
  cs <- as.numeric((M / sqrt(rowSums(M^2))) %*% (q / sqrt(sum(q^2))))
  rc <- morie_ragRet_top_k(q, rg_corpus, 2, "cosine")
  expect_identical(rc$indices, order(-cs)[1:2] - 1L)
  expect_identical(length(morie_ragRet_top_k(q, rg_corpus, 99)$indices), 7L)
  expect_error(morie_ragRet_top_k(q, rg_corpus, metric = "l2"), "metric must be")
  expect_error(morie_ragRet_top_k(q, list()), "empty")
  expect_error(morie_ragRet_top_k(q[1:2], rg_corpus), "different width")
})

test_that("the IVF index is a k-means fixed point and full probing is exact", {
  ix <- morie_ragRet_ivf_index(rg_corpus, n.cells = 3, iters = 50, seed = 1)
  M <- do.call(rbind, rg_corpus)
  C <- do.call(rbind, ix$centroids)
  near <- apply(M, 1, function(x) which.min(colSums((t(C) - x)^2))) - 1L
  expect_identical(ix$assign, near)
  for (k in unique(ix$assign)) {
    expect_equal(C[k + 1, ], colMeans(M[ix$assign == k, , drop = FALSE]), tolerance = 1e-14)
  }
  expect_identical(sort(unlist(ix$lists)), 1:7)
  q <- c(0.2, 0.9, 0.3)
  full <- morie_ragRet_ivf_search(q, rg_corpus, ix, k.top = 3, nprobe = 3)
  ex <- morie_ragRet_top_k(q, rg_corpus, 3)
  expect_identical(full$indices, ex$indices)
  expect_equal(full$fraction.scanned, 1)
  one <- morie_ragRet_ivf_search(q, rg_corpus, ix, k.top = 3, nprobe = 1)
  rr <- morie_ragRet_recall_at_k(one$indices, ex$indices)
  expect_equal(rr$recall, mean(ex$indices %in% one$indices))
  expect_identical(rr$missed, ex$indices[!(ex$indices %in% one$indices)])
  expect_error(morie_ragRet_ivf_index(rg_corpus, 8), "8 cells for 7")
  expect_error(morie_ragRet_recall_at_k(1:2, integer(0)), "empty")
})

test_that("RAG-Sequence and RAG-Token marginalise over documents differently", {
  p <- c(2, 1, 1)
  toks <- list(c(0.9, 0.5), c(0.2, 0.8), c(0.6, 0.6))
  w <- p / sum(p)
  sq <- morie_ragRet_marginalise(p, toks, "sequence")
  expect_equal(sq$probability, sum(w * vapply(toks, prod, 1)), tolerance = 1e-15)
  tk <- morie_ragRet_marginalise(p, toks, "token")
  expect_equal(tk$per_token, as.numeric(do.call(cbind, toks) %*% w), tolerance = 1e-15)
  expect_equal(tk$probability, prod(tk$per_token), tolerance = 1e-15)
  expect_error(morie_ragRet_marginalise(p, toks, "beam"), "sequence or token")
  expect_error(morie_ragRet_marginalise(c(-1, 1, 1), toks), "non-negative")
  expect_error(morie_ragRet_marginalise(c(0, 0, 0), toks), "all zero")
  expect_error(morie_ragRet_marginalise(p, toks[1:2]), "3 documents but 2")
  expect_error(morie_ragRet_marginalise(p, list(1, 1:2, 1), "token"), "differ in length")
})
