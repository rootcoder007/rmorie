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
