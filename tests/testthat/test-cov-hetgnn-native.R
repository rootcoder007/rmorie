# Coverage tests for R/hetgnn_native.R (HAN, Wang et al. 2019): meta-path
# neighbours, node-level and semantic attention, and the hierarchical
# forward pass.

hg_graph <- function() {
  types <- list(m1 = "M", m2 = "M", m3 = "M", a1 = "A", a2 = "A", d1 = "D")
  und <- rbind(c("m1", "a1"), c("m2", "a1"), c("m2", "a2"), c("m3", "a2"), c("m1", "d1"), c("m3", "d1"))
  edges <- list()
  for (k in seq_len(nrow(und))) {
    edges[[und[k, 1]]] <- c(edges[[und[k, 1]]], und[k, 2])
    edges[[und[k, 2]]] <- c(edges[[und[k, 2]]], und[k, 1])
  }
  H <- list(m1 = c(1, 0, 0.5), m2 = c(0, 1, -0.5), m3 = c(0.5, 0.5, 1), a1 = c(1, 1, 0), a2 = c(-1, 0, 1), d1 = c(0, 0, 2))
  list(types = types, edges = edges, H = H)
}
hg_W <- rbind(c(0.5, -0.2, 0.1), c(0.3, 0.4, -0.6))
hg_a <- c(0.7, -0.3, 0.2, 0.9)

test_that("meta-path neighbours follow typed walks", {
  g <- hg_graph()
  mam <- metapath_neighbours(g$edges, g$types, c("M", "A", "M"))$neighbours
  expect_equal(mam, list(m1 = "m2", m2 = c("m1", "m3"), m3 = "m2"))
  mdm <- metapath_neighbours(g$edges, g$types, c("M", "D", "M"))$neighbours
  expect_equal(mdm, list(m1 = "m3", m2 = character(0), m3 = "m1"))
  expect_equal(metapath_neighbours(g$edges, g$types, c("A", "M"))$neighbours$a2, c("m2", "m3"))
  expect_error(metapath_neighbours(g$edges, g$types, "M"), "at least 2 types")
})

test_that("node-level attention: LeakyReLU scores, softmax, ELU output", {
  g <- hg_graph()
  nb <- c("m1", "m2", "m3")
  r <- node_attention(g$H$m2, nb, g$H, hg_a, hg_W)
  P <- sapply(g$H[nb], function(h) as.numeric(hg_W %*% h))
  hi <- as.numeric(hg_W %*% g$H$m2)
  raw <- as.numeric(sum(hg_a[1:2] * hi) + hg_a[3:4] %*% P)
  soft <- function(sl) {
    s <- ifelse(raw >= 0, raw, sl * raw)
    exp(s) / sum(exp(s))
  }
  al <- soft(0.2)
  z <- as.numeric(P %*% al)
  expect_equal(r$alpha, al, tolerance = 1e-12)
  expect_equal(r$embedding, ifelse(z > 0, z, expm1(z)), tolerance = 1e-12)
  s5 <- node_attention(g$H$m2, nb, g$H, hg_a, hg_W, slope = 0.5)
  expect_equal(s5$alpha, soft(0.5), tolerance = 1e-12)
  expect_error(node_attention(g$H$m2, character(0), g$H, hg_a, hg_W), "no meta-path neighbours")
})

test_that("semantic attention averages q . tanh(W z + b) over nodes", {
  Z <- list(b = list(c(0.1, 0.4), c(-0.3, 0.2)), a = list(c(1, 0), c(0.5, 0.5)))
  Ws <- rbind(c(0.2, -0.5), c(0.7, 0.1))
  bs <- c(0.05, -0.1)
  q <- c(1, -0.4)
  r <- semantic_attention(Z, Ws, bs, q)
  sc <- vapply(c("a", "b"), function(nm) mean(vapply(Z[[nm]], function(z) sum(q * tanh(bs + Ws %*% z)), 0)), 0)
  expect_equal(r$metapaths, c("a", "b"))
  expect_equal(unlist(r$scores), sc, tolerance = 1e-12)
  expect_equal(unlist(r$beta), exp(sc) / sum(exp(sc)), tolerance = 1e-12)
  expect_error(semantic_attention(list(), Ws, bs, q), "no meta-path embeddings")
})

test_that("the HAN forward pass combines the two attentions", {
  g <- hg_graph()
  mps <- list(MAM = c("M", "A", "M"), MDM = c("M", "D", "M"))
  Ws <- rbind(c(0.2, -0.5), c(0.7, 0.1))
  bs <- c(0.05, -0.1)
  q <- c(1, -0.4)
  r <- han_forward(g$H, g$edges, g$types, mps, hg_a, hg_W, Ws, bs, q)
  expect_equal(unname(r$target_nodes), 1:3)
  per <- lapply(mps, function(mp) {
    nb <- metapath_neighbours(g$edges, g$types, mp)$neighbours
    lapply(c("m1", "m2", "m3"), function(v) node_attention(g$H[[v]], sort(union(nb[[v]], v)), g$H, hg_a, hg_W)$embedding)
  })
  expect_equal(lapply(r$per_metapath, unname), per, tolerance = 1e-12)
  sem <- semantic_attention(per, Ws, bs, q)
  fin <- t(vapply(1:3, function(i) sem$beta$MAM * per$MAM[[i]] + sem$beta$MDM * per$MDM[[i]], numeric(2)))
  expect_equal(r$embeddings[1:3, ], fin, tolerance = 1e-12)
  expect_equal(r$embeddings[4:6, ], matrix(0, 3, 2))
  expect_equal(morie_hetgnn(g$H, g$edges, g$types, mps, hg_a, hg_W, Ws, bs, q), r)
  expect_identical(heterogeneousattention, han_forward)
  expect_identical(heterogeneous_gnn, han_forward)
  Hm <- do.call(rbind, g$H)
  tm <- setNames(g$types, as.character(1:6))
  em <- setNames(lapply(g$edges[names(g$types)], function(v) as.character(match(v, names(g$types)))), as.character(1:6))
  expect_equal(han_forward(Hm, em, tm, mps, hg_a, hg_W, Ws, bs, q)$embeddings, r$embeddings, tolerance = 1e-12)
  expect_match(.hetgnn_cheatsheet(), "META-PATH")
  expect_error(han_forward(g$H, g$edges, g$types, list(x = c("M", "A"), y = c("A", "M")), hg_a, hg_W, Ws, bs, q), "same target node type")
  expect_error(han_forward(g$H, g$edges, g$types, list(x = c("Z", "A")), hg_a, hg_W, Ws, bs, q), "no node has")
})
