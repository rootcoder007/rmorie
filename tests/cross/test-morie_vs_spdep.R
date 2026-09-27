# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native SKATER vs spdep::skater.

lattice <- function() {
  nr <- 7
  n <- 49
  U <- .morie_random_uniform(3 * n, seed = 21, stream = 0)
  i <- 0:(n - 1)
  X <- cbind(sin((i %% nr + 1) / 2) + 0.3 * U[3 * i + 1], cos((i %/% nr + 1) / 3) + 0.3 * U[3 * i + 2],
             ifelse(i %% nr + 1 > 4, 1.5, 0) + 0.2 * U[3 * i + 3])
  nb <- lapply(i, function(j) as.integer(sort(c(if (j %% nr) j - 1, if (j %% nr != nr - 1) j + 1, if (j >= nr) j - nr, if (j + nr < n) j + nr)) + 1))
  list(X = X, nb = nb, pop = 1 + (i * 7) %% 5)
}

test_that("Skater reproduces spdep::skater", {
  skip_if_not_installed("spdep")
  L <- lattice()
  nbo <- structure(L$nb, class = "nb")
  tr <- spdep::mstree(spdep::nb2listw(nbo, spdep::nbcosts(nbo, L$X), style = "B"), ini = 1)
  for (k in c(2, 4, 6)) {
    for (ms in c(1, 5)) {
      ref <- spdep::skater(tr[, 1:2], L$X, ncuts = k - 1, crit = ms)$groups
      o <- Skater(L$X, L$nb, k, min_size = ms)$labels
      expect_identical(length(unique(paste(o, ref))), length(unique(ref)))
    }
  }
})

test_that("SpatialWeights equals spdep neighbour graphs and nb2mat styles", {
  skip_if_not_installed("spdep")
  U <- .morie_random_uniform(160, seed = 54, stream = 0)
  P <- cbind(U[1:60], U[61:120])
  nbs <- list(knn = spdep::knn2nb(spdep::knearneigh(P, 5)), distance = suppressWarnings(spdep::dnearneigh(P, 0, 0.22)),
              gabriel = spdep::graph2nb(spdep::gabrielneigh(P), sym = TRUE),
              relative = spdep::graph2nb(spdep::relativeneigh(P), sym = TRUE))
  args <- list(knn = list(k = 5), distance = list(threshold = 0.22), gabriel = list(), relative = list())
  for (nm in names(nbs)) {
    for (st in c("B", "W", "C", "U", "S", "minmax")) {
      R <- spdep::nb2mat(nbs[[nm]], style = st, zero.policy = TRUE)
      M <- do.call(SpatialWeights, c(list(P, nm), args[[nm]], list(style = st)))$W
      expect_lt(max(abs(unname(R) - M)), 1e-12)
    }
  }
})

test_that("local Geary, correlogram, k-colour join counts, bivariate LISA and Moran scatter match spdep", {
  skip_if_not_installed("spdep")
  nr <- 6
  nc <- 7
  n <- nr * nc
  nb <- spdep::cell2nb(nr, nc, type = "rook")
  A <- spdep::nb2mat(nb, style = "B")
  lw <- spdep::nb2listw(nb, style = "W")
  W <- A / rowSums(A)
  u <- .morie_random_uniform(3 * n, seed = 7, stream = 0)
  x <- u[1:n] * 10 + (0:(n - 1)) %% nc
  y <- u[n + 1:n] * 5 + (0:(n - 1)) %/% nc
  lab <- c("a", "b", "c", "d")[floor(u[2 * n + 1:n] * 4) + 1]
  expect_equal(Localgeary(x, W)$local, as.numeric(spdep::localC(x, lw)), tolerance = 1e-12)
  expect_equal(Localgeary(cbind(x, y), W)$local, as.numeric(spdep::localC(list(x, y), lw)),
               tolerance = 1e-12)
  for (m in c("I", "C", "corr")) for (st in c("W", "B")) for (rnd in c(TRUE, FALSE)) {
    r <- SpatialCorrelogram(A, x, order = 4, method = m, style = st, randomisation = rnd)
    s <- spdep::sp.correlogram(nb, x, order = 4, method = m, style = st, randomisation = rnd)
    if (m == "corr") {
      expect_equal(r$estimate, unname(s$res), tolerance = 1e-12)
    } else {
      expect_equal(cbind(r$estimate, r$expectation, r$variance), unname(s$res), tolerance = 1e-12)
    }
  }
  for (lst in list(lw, spdep::nb2listw(nb, style = "B"))) {
    jc <- spdep::joincount.multi(factor(lab), lst)
    ours <- JoinCountMulti(lab, spdep::listw2mat(lst))
    expect_equal(ours$rows, rownames(jc))
    expect_equal(cbind(ours$joincount, ours$expected, ours$variance), unname(unclass(jc)[, 1:3]),
                 tolerance = 1e-12)
  }
  bv <- LocalMoranBivariate(x, y, W, nsim = 99)
  ref <- spdep::localmoran_bv(x, y, lw, nsim = 99)
  expect_equal(bv$local_values, unname(ref[, 1]), tolerance = 1e-12)
  expect_equal(bv$quadrant, as.character(attr(ref, "quadr")$mean))
  mp <- spdep::moran.plot(x, lw, plot = FALSE)
  ms <- MoranScatter(x, W)
  expect_equal(cbind(ms$wx, ms$dfb_1, ms$dfb_x, ms$dffit, ms$cov_r, ms$cook_d, ms$hat),
               unname(as.matrix(mp[, c("wx", "dfb.1_", "dfb.x", "dffit", "cov.r", "cook.d", "hat")])),
               tolerance = 1e-12)
  expect_identical(ms$is_inf, mp$is_inf)
})

test_that("PolygonContiguity, RhoBounds, ErrorOperator, cardinality and diffnb match spdep and spatialreg", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("sf")
  skip_if_not_installed("spatialreg")
  u <- .morie_random_uniform(2 * 6 * 7, seed = 3, stream = 0)
  V <- array(0, c(6, 7, 2))
  k <- 1
  for (i in 0:5) for (j in 0:6) {
    V[i + 1, j + 1, ] <- c(j + 0.3 * (u[k] - 0.5), i + 0.3 * (u[k + 1] - 0.5))
    k <- k + 2
  }
  polys <- list()
  for (i in 1:5) for (j in 1:6) {
    polys[[length(polys) + 1]] <- rbind(V[i, j, ], V[i, j + 1, ], V[i + 1, j + 1, ], V[i + 1, j, ], V[i, j, ])
  }
  g <- sf::st_sfc(lapply(polys, function(p) sf::st_polygon(list(p))))
  for (q in c(TRUE, FALSE)) {
    nb <- spdep::poly2nb(g, queen = q)
    expect_equal(PolygonContiguity(polys, queen = q), unname(spdep::nb2mat(nb, style = "B")), ignore_attr = TRUE)
  }
  nbr <- spdep::poly2nb(g, queen = FALSE)
  lw <- spdep::nb2listw(nbr, style = "W")
  W <- spdep::listw2mat(lw)
  e <- Re(spatialreg::eigenw(lw))
  b <- RhoBounds(W)
  expect_equal(c(b$lower, b$upper), c(1 / min(e), 1 / max(e)), tolerance = 1e-12)
  expect_equal(ErrorOperator(W, 0.4), unname(as.matrix(spatialreg::invIrW(lw, 0.4))), tolerance = 1e-12,
               ignore_attr = TRUE)
  expect_equal(NeighbourCardinality(W)$cardinality, spdep::card(nbr))
  nbq <- spdep::poly2nb(g, queen = TRUE)
  d <- CompareNeighbours(spdep::nb2mat(nbq, style = "B"), W)$difference
  ref <- lapply(spdep::diffnb(nbq, nbr), function(v) as.integer(v[v > 0]))
  expect_equal(d, ref)
  grp <- rep(1:3, length.out = 30)
  blk <- spdep::nb2mat(spdep::nb2blocknb(NULL, as.character(grp)), style = "B", zero.policy = TRUE)
  expect_equal(BlockWeights(grp), unname(blk), ignore_attr = TRUE)
})
