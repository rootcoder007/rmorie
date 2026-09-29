# Coverage for Graphormer's structural encodings (Ying et al. 2021): the
# degree lookup, the BFS shortest-path matrix (against Floyd-Warshall),
# the spatial bias table, the path-averaged edge encoding and the biased
# attention (against softmax(Q K' / sqrt(d) + B + E) V by matrix algebra).
# Both adjacency conventions -- the named-list form and the restored
# integer-vector form -- are exercised.

# path 0-1-2-3 plus a pendant 4 on 1, and an isolated 5
.adjn <- list("0" = list("1" = 1), "1" = list("0" = 1, "2" = 1, "4" = 1), "2" = list("1" = 1, "3" = 1),
              "3" = list("2" = 1), "4" = list("1" = 1), "5" = list())
.adji <- list(1, c(0, 2, 4), c(1, 3), 2, 1, integer(0))
.fw <- function(adj, n) {
  D <- matrix(Inf, n, n)
  diag(D) <- 0
  for (v in seq_len(n)) for (w in adj[[v]]) D[v, w + 1] <- 1
  for (k in 1:n) for (i in 1:n) for (j in 1:n) D[i, j] <- min(D[i, j], D[i, k] + D[k, j])
  D[is.infinite(D)] <- -1
  D
}

test_that("shortest-path matrices equal Floyd-Warshall with -1 for unreachable", {
  ref <- .fw(.adji, 6)
  sp <- shortest_path_matrix(.adjn, 6)
  expect_equal(sp$distance, ref, ignore_attr = TRUE)
  expect_identical(sp$n_unreachable, sum(ref == -1))
  sp2 <- morie_grphmr_sp(.adji, 6)
  expect_equal(sp2$distance, ref, ignore_attr = TRUE)
})

test_that("centrality encodings are degree-indexed lookups", {
  z <- list(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
  ce <- centrality_encoding(.adjn, 6, z)
  deg <- c(1, 3, 2, 1, 1, 0)
  expect_identical(ce$degrees, as.integer(deg))
  expect_equal(ce$encoding, lapply(deg, function(d) z[[min(d, 3) + 1]]))
  ce2 <- morie_grphmr_centrality(.adji, 6, z)
  expect_equal(ce2$encoding, ce$encoding)
  dir <- list("0" = list("1" = 1, "2" = 1), "1" = list("2" = 1), "2" = list())
  zo <- list(c(10, 0), c(20, 0), c(30, 0))
  cd <- centrality_encoding(dir, 3, z, z_out = zo, directed = TRUE)
  expect_identical(cd$degrees, c(0L, 1L, 2L))
  expect_equal(cd$encoding[[1]], z[[1]] + zo[[3]])
  expect_equal(cd$encoding[[3]], z[[3]] + zo[[1]])
  cd2 <- morie_grphmr_centrality(list(c(1, 2), 2, integer(0)), 3, z, z_out = zo, directed = TRUE)
  expect_equal(cd2$encoding, cd$encoding)
})

test_that("spatial bias maps distances through the table and caps at its end", {
  D <- shortest_path_matrix(.adjn, 6)$distance
  bt <- c(0.5, 0.2, -0.1)
  sb <- spatial_bias(D, bt, unreachable_bias = -7)
  ref <- ifelse(D == -1, -7, bt[pmin(pmax(D, 0), 2) + 1])
  expect_equal(sb$bias, ref, ignore_attr = TRUE)
  expect_equal(morie_grphmr_spatial(D, bt, -7)$bias, ref, ignore_attr = TRUE)
  expect_equal(spatial_bias(D, bt)$unreachable_bias, -10)
})

test_that("edge encodings average feature-weight dot products along the path", {
  ef <- list("(0, 1)" = c(1, 2), "(1, 2)" = c(0.5, -1), "(2, 3)" = c(3, 0))
  wt <- list(c(1, 0), c(0.5, 0.5))
  paths <- list("(0, 3)" = list(c(0, 1), c(1, 2), c(2, 3)), "(2, 1)" = list(c(2, 1)), "(4, 4)" = list())
  ee <- edge_encoding(paths, ef, wt)
  expect_equal(ee$edge_bias[["(0, 3)"]], (1 + (0.25 - 0.5) + 1.5) / 3, tolerance = 1e-12)
  expect_equal(ee$edge_bias[["(2, 1)"]], 0.5, tolerance = 1e-12)
  expect_identical(ee$edge_bias[["(4, 4)"]], 0)
  expect_error(edge_encoding(list("(0, 9)" = list(c(0, 9))), ef, wt), "no features for edge")
  ef2 <- list("0,1" = c(1, 2), "1,2" = c(0.5, -1), "2,3" = c(3, 0))
  ee2 <- morie_grphmr_edge(paths, ef2, wt)
  expect_equal(ee2$edge_bias, ee$edge_bias, tolerance = 1e-12)
})

test_that("Graphormer attention is softmax(QK'/sqrt(d) + B + E) V", {
  set.seed(3)
  H <- matrix(stats::rnorm(12), 4)
  WQ <- matrix(stats::rnorm(6), 2)
  WK <- matrix(stats::rnorm(6), 2)
  WV <- matrix(stats::rnorm(6), 2)
  B <- matrix(stats::rnorm(16), 4)
  E <- list()
  for (i in 0:3) for (j in 0:3) E[[sprintf("(%d, %d)", i, j)]] <- 0.1 * (i - j)
  a <- graphormer_attention(H, WQ, WK, WV, B, E)
  Q <- H %*% t(WQ)
  K <- H %*% t(WK)
  S <- Q %*% t(K) / sqrt(2) + B + 0.1 * outer(0:3, 0:3, "-")
  A <- exp(S - apply(S, 1, max))
  A <- A / rowSums(A)
  expect_equal(a$weights, A, tolerance = 1e-12)
  expect_equal(a$output, A %*% H %*% t(WV), tolerance = 1e-12)
  expect_equal(graphormer_attention(H, WQ, WK, WV, B)$weights, {
    S0 <- Q %*% t(K) / sqrt(2) + B
    A0 <- exp(S0 - apply(S0, 1, max))
    A0 / rowSums(A0)
  }, tolerance = 1e-12)
  expect_identical(graphormer, graphormer_attention)
  expect_identical(morie_grphmr, graphormer_attention)
})
