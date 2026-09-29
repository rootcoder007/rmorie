# Coverage for the network files (assort.R, avglen.R, barabsi.R,
# clstcoef.R, clusca.R, comemb.R, comlou.R, comspr.R, confgg.R, ecccen.R,
# eigcen.R, erdosg.R, ergmod.R, flowmin.R, clocen.R, deepw.R, deepwk.R):
# graph statistics are recomputed from shortest-path matrices, matrix
# powers and brute-force enumeration.

gr_edges <- rbind(c(1, 2), c(1, 3), c(2, 3), c(3, 4), c(4, 5), c(4, 6), c(5, 6), c(6, 7))
gr_A <- matrix(0, 7, 7)
gr_A[gr_edges] <- 1
gr_A[gr_edges[, 2:1]] <- 1
sp_ref <- function(A) {
  n <- nrow(A)
  D <- ifelse(A != 0, 1, Inf)
  diag(D) <- 0
  for (k in 1:n) D <- pmin(D, outer(D[, k], D[k, ], "+"))
  D
}

test_that("Assort is Newman's degree assortativity", {
  r <- Assort(A = gr_A)
  deg <- rowSums(gr_A)
  j <- c(deg[gr_edges[, 1]], deg[gr_edges[, 2]]) - 1
  k <- c(deg[gr_edges[, 2]], deg[gr_edges[, 1]]) - 1
  expect_equal(r$r, cor(j, k), tolerance = 1e-12)
  expect_equal(Assort(A = gr_A, excess = FALSE)$r, cor(j + 1, k + 1), tolerance = 1e-12)
  expect_error(Assort(), "adjacency matrix is required")
  expect_error(Assort(A = matrix(0, 0, 0)), "empty")
  expect_error(Assort(A = matrix(0, 2, 3)), "not square")
  expect_error(Assort(A = matrix(0, 3, 3)), "no edges")
  ring <- matrix(0, 4, 4)
  ring[cbind(1:4, c(2:4, 1))] <- 1
  expect_error(Assort(A = ring + t(ring)), "undefined")
})

test_that("Avgpathlen, Clocent and ecccen use geodesic distances", {
  D <- sp_ref(gr_A)
  off <- D[row(D) != col(D)]
  r <- Avgpathlen(gr_A)
  expect_equal(r$estimate, mean(off), tolerance = 1e-12)
  expect_equal(r$harmonic, 42 / sum(1 / off), tolerance = 1e-12)
  expect_equal(r$diameter, as.integer(max(D)))
  dir <- matrix(0, 3, 3)
  dir[1, 2] <- dir[2, 3] <- 1
  rd <- Avgpathlen(dir, directed = TRUE)
  expect_equal(c(rd$estimate, rd$reachable), c(4 / 3, 3L), tolerance = 1e-12)
  expect_error(Avgpathlen(matrix(0, 1, 1)), "n >= 2")
  cl <- Clocent(gr_A)
  expect_equal(cl$closeness, 6 / rowSums(D), tolerance = 1e-12)
  iso <- gr_A
  iso <- cbind(rbind(iso, 0), 0)
  expect_true(is.na(Clocent(iso)$closeness[8]))
  ec <- ecccen(gr_A)
  e <- apply(D, 1, max)
  expect_equal(ec$eccentricity, e)
  expect_equal(ec$estimate, mean(1 / e), tolerance = 1e-12)
  expect_equal(ec$centre, which(e == min(e)) - 1L)
  expect_equal(ecccen(gr_A, node = 3)$ecc, e[4])
  expect_same_function(morie_eccentricity_centrality, ecccen)
})

test_that("clustering coefficients: Clstcoef and Clustcoef", {
  r <- Clstcoef(gr_A)
  A3 <- gr_A %*% gr_A %*% gr_A
  deg <- rowSums(gr_A)
  loc <- ifelse(deg >= 2, diag(A3) / (deg * (deg - 1)), NaN)
  expect_equal(r$local, loc, tolerance = 1e-12)
  expect_equal(r$estimate, mean(loc[!is.nan(loc)]), tolerance = 1e-12)
  c2 <- Clustcoef(gr_A)
  expect_equal(c2$local, ifelse(is.nan(loc), 0, loc), tolerance = 1e-12)
  expect_equal(c2$transitivity, sum(diag(A3)) / sum(deg * (deg - 1)), tolerance = 1e-12)
  expect_equal(Clustcoef(NULL, A = gr_A, node = 2)$estimate, loc[3], tolerance = 1e-12)
})

test_that("Eigcent is the Perron vector", {
  r <- Eigcent(gr_A)
  e <- eigen(gr_A, symmetric = TRUE)
  v <- abs(e$vectors[, 1])
  expect_equal(r$eigenvalue, e$values[1], tolerance = 1e-12)
  expect_equal(r$centrality, v / max(v), tolerance = 1e-10)
  expect_equal(as.numeric(gr_A %*% r$unit), r$eigenvalue * r$unit, tolerance = 1e-10)
})

test_that("Specclus embeds with the normalised Laplacian", {
  r <- Specclus(gr_A, k = 2)
  d <- rowSums(gr_A)
  L <- diag(7) - diag(1 / sqrt(d)) %*% gr_A %*% diag(1 / sqrt(d))
  ev <- sort(eigen(L, symmetric = TRUE)$values)
  expect_equal(r$values, ev[1:2], tolerance = 1e-10)
  expect_equal(length(unique(r$cluster[1:3])), 1L)
  expect_false(r$cluster[1] == r$cluster[5])
  un <- Specclus(gr_A, k = 2, normalized = FALSE)
  Lu <- diag(d) - gr_A
  expect_equal(un$values, sort(eigen(Lu, symmetric = TRUE)$values)[1:2], tolerance = 1e-10)
  expect_error(Specclus(gr_A[, 1:3]), "square")
  expect_error(Specclus(gr_A, k = 1), "2 <= k")
})

test_that("Comlou finds the modularity-maximising split", {
  r <- Comlou(gr_A)
  m2 <- sum(gr_A)
  deg <- rowSums(gr_A)
  Qf <- function(z) sum((gr_A - outer(deg, deg) / m2) * outer(z, z, "==")) / m2
  expect_equal(r$Q, Qf(r$z), tolerance = 1e-12)
  # exhaustive search over the 2^6 two-community splits
  best <- max(vapply(0:63, function(b) Qf(c(0, bitwAnd(b, 2^(0:5)) > 0)), 0))
  expect_gte(r$Q, best - 1e-12)
  expect_equal(Comlou(gr_A, resolution = 0.5)$Q,
               sum((gr_A - 0.5 * outer(deg, deg) / m2) *
                     outer(Comlou(gr_A, resolution = 0.5)$z, Comlou(gr_A, resolution = 0.5)$z,
                           "==")) / m2, tolerance = 1e-12)
  expect_error(Comlou(matrix(0, 0, 0)), "empty")
  expect_error(Comlou(gr_A[, 1:3]), "square")
  expect_error(Comlou(gr_A, resolution = 0), "strictly positive")
})

test_that("random graph generators: Bamodel, Configmodel, Erdosg", {
  r <- Bamodel(8, m = 2, seed = 3)
  s <- 3
  un <- function() {
    s <<- (48271 * s) %% 2147483647
    s / 2147483647
  }
  deg <- rep(2, 3)
  ed <- t(combn(3, 2))
  for (v in 4:8) {
    w <- deg
    tg <- integer(0)
    for (e in 1:2) {
      u <- un() * sum(w)
      pk <- which(u < cumsum(w))[1]
      if (is.na(pk)) pk <- v - 1
      tg <- c(tg, pk)
      w[pk] <- 0
    }
    deg <- c(deg, 2)
    deg[tg] <- deg[tg] + 1
    ed <- rbind(ed, cbind(tg, v))
  }
  expect_equal(r$degree, deg)
  expect_equal(unname(r$edges), unname(ed - 1L))
  expect_equal(r$mean_degree, sum(deg) / 8, tolerance = 1e-12)
  expect_error(Bamodel(3, m = 4), "1 <= m")
  cm <- Configmodel(c(2, 2, 1, 1, 2), seed = 5)
  expect_equal(cm$degree, c(2, 2, 1, 1, 2))
  expect_equal(nrow(cm$edges), 4L)
  expect_equal(cm$self_loops, sum(cm$edges[, 1] == cm$edges[, 2]))
  expect_error(Configmodel(c(1, -1)), "non-negative")
  expect_error(Configmodel(c(1, 2)), "even")
  eg <- Erdosg(6, 0.4)
  PR <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37)
  vdc <- function(i, b) {
    f <- 1
    rr <- 0
    k <- i + 1
    while (k > 0) {
      f <- f / b
      rr <- rr + f * (k %% b)
      k <- k %/% b
    }
    rr
  }
  u <- vapply(0:14, function(k) vdc(k %/% 12 + 1, PR[k %% 12 + 1]), 0)
  expect_equal(eg$edges, sum(u < 0.4))
  expect_equal(eg$density, sum(u < 0.4) / 15, tolerance = 1e-12)
  expect_equal(eg$expected_edges, 6, tolerance = 1e-12)
  expect_equal(Erdosg(4, 0)$n_components, 4L)
  expect_equal(Erdosg(4, 1)$largest_component, 4L)
  expect_error(Erdosg(0, 0.5), "positive")
  expect_error(Erdosg(3, 2), "\\[0, 1\\]")
})

test_that("Ergmod is the pseudo-likelihood logistic fit on change statistics", {
  r <- Ergmod(gr_A, statistics = c("edges", "triangle"))
  dy <- which(upper.tri(gr_A), arr.ind = TRUE)
  y <- gr_A[dy]
  tri <- apply(dy, 1, function(ij) sum(gr_A[ij[1], -ij] * gr_A[ij[2], -ij]))
  fit <- glm(y ~ tri, family = binomial(), control = list(epsilon = 1e-14, maxit = 100))
  expect_equal(r$theta, unname(coef(fit)), tolerance = 1e-8)
  expect_equal(r$se, unname(sqrt(diag(vcov(fit)))), tolerance = 1e-6)
  expect_equal(r$observed_stats, c(8, sum(diag(gr_A %*% gr_A %*% gr_A)) / 6))
  e1 <- Ergmod(gr_A)
  expect_equal(e1$theta, qlogis(8 / 21), tolerance = 1e-10)
  ts <- Ergmod(gr_A, c("edges", "twostar"))
  deg <- rowSums(gr_A)
  expect_equal(ts$observed_stats[2], sum(choose(deg, 2)))
  expect_error(Ergmod(matrix(0, 0, 0)), "no nodes")
  expect_error(Ergmod(gr_A[, 1:3]), "square")
  expect_error(Ergmod(gr_A + diag(7)), "zero diagonal")
  expect_error(Ergmod(gr_A * 2), "0/1")
  expect_error(Ergmod(upper.tri(gr_A) * gr_A), "symmetric")
  expect_error(Ergmod(gr_A, "kstar"), "unsupported")
  expect_error(Ergmod(gr_A, character(0)), "at least one")
  expect_error(Ergmod(gr_A, theta_init = c(1, 2)), "one entry per")
})

test_that("Mincutsw is the Stoer-Wagner global minimum cut", {
  W <- gr_A * outer(1:7, 1:7, function(i, j) 1 + (i + j) %% 3)
  r <- Mincutsw(W)
  cuts <- vapply(1:(2^6 - 1), function(b) {
    s <- c(TRUE, bitwAnd(b, 2^(0:5)) > 0)
    if (all(s)) return(Inf)
    sum(W[s, !s])
  }, 0)
  expect_equal(r$weight, min(cuts), tolerance = 1e-12)
  s <- r$partition == 1
  expect_equal(sum(W[s, !s]), r$weight, tolerance = 1e-12)
})

test_that("Deepw, Deepwk and Comemb: random walks and skip-gram embeddings", {
  r <- Deepw(gr_A, walk_len = 5, dim = 3, n_walks = 1, seed = 4)
  nb <- lapply(1:7, function(i) which(gr_A[i, ] != 0))
  e <- .ghc_rng(4)
  walks <- lapply(1:7, function(v) {
    w <- v
    for (s in 1:4) {
      d <- length(nb[[w[length(w)]]])
      k <- min(floor(.ghc_unif(e, 1L) * d) + 1, d)
      w <- c(w, nb[[w[length(w)]]][k])
    }
    w
  })
  expect_equal(lapply(r$walks, as.numeric), lapply(walks, as.numeric))
  W <- r$embedding
  cs <- unlist(lapply(1:7, function(i) vapply(nb[[i]], function(j)
    sum(W[i, ] * W[j, ]) / sqrt(sum(W[i, ]^2) * sum(W[j, ]^2)), 0)))
  expect_equal(r$estimate, mean(cs), tolerance = 1e-12)
  expect_equal(Deepwk(gr_A, walk_len = 5, dim = 3, n_walks = 1, seed = 4)$embedding, W)
  expect_error(Deepw(gr_A, walk_len = 1), "at least 2")
  expect_error(Deepw(gr_A, dim = 0), "at least 1")
  expect_error(Deepw(gr_A, n_walks = 0), "at least 1")
  expect_error(Deepw(gr_A[, 1:3]), "square")
  n2 <- Comemb(gr_A, p = 0.5, q = 2, walk_len = 4, dim = 3, n_walks = 1, seed = 2)
  for (w in n2$walks) expect_true(all(gr_A[cbind(w[-length(w)], w[-1])] == 1))
  W2 <- n2$embedding
  cs2 <- unlist(lapply(1:7, function(i) vapply(nb[[i]], function(j)
    sum(W2[i, ] * W2[j, ]) / sqrt(sum(W2[i, ]^2) * sum(W2[j, ]^2)), 0)))
  expect_equal(n2$estimate, mean(cs2), tolerance = 1e-12)
  expect_error(Comemb(gr_A, p = 0), "strictly positive")
  expect_error(Comemb(gr_A, dim = 0), "at least 1")
  expect_error(Comemb(gr_A, walk_len = 1), "at least 2")
})
