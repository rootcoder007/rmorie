# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The TPS DBSCAN callers (morie_tps_dbscan_clusters,
# mrm_tps_moran_clustering, morie_tps_render_points,
# morie_tps_render_dbscan) now run the native DBSCAN unconditionally.
# Parity is checked against dbscan::dbscan on the same coordinates.

tps_df <- function(n_per = 60, seed = 2026) {
  set.seed(seed)
  data.frame(
    LAT_WGS84 = c(rnorm(n_per, 43.65, 0.004), rnorm(n_per, 43.70, 0.004),
                  runif(n_per %/% 2, 43.60, 43.85)),
    LONG_WGS84 = c(rnorm(n_per, -79.40, 0.004), rnorm(n_per, -79.38, 0.004),
                   runif(n_per %/% 2, -79.60, -79.15))
  )
}

test_that("native DBSCAN labels equal dbscan::dbscan (incl. border points)", {
  skip_if_not_installed("dbscan")
  dbn <- getFromNamespace(".morie_dbscan_native", "rmorie")
  set.seed(9)
  for (it in 1:20) {
    k <- sample(2:5, 1)
    ctr <- matrix(runif(2 * k, 0, 10), k, 2)
    x <- do.call(rbind, lapply(seq_len(k), function(i)
      cbind(rnorm(40, ctr[i, 1], 0.4), rnorm(40, ctr[i, 2], 0.4))))
    x <- rbind(x, matrix(runif(40, 0, 10), 20, 2))
    x <- x[sample(nrow(x)), ]
    eps <- runif(1, 0.2, 0.8)
    mp <- sample(3:8, 1)
    # Same core rule (<= eps, self counted), same discovery order and the
    # same first-come border assignment: labels identical after the
    # 0-noise / 1-based shift, not merely up to permutation.
    expect_identical(dbn(x, eps, mp)$labels + 1L,
                     as.integer(dbscan::dbscan(x, eps, mp)$cluster))
  }
  # a border point within eps of two clusters goes to the first found
  x <- rbind(c(0, 0), c(0.1, 0), c(-0.1, 0), c(0, 0.1), c(0.55, 0),
             c(1.1, 0), c(1.2, 0), c(1.0, 0), c(1.1, 0.1))
  for (o in list(seq_len(9), 9:1)) {
    expect_identical(dbn(x[o, ], 0.5, 4)$labels + 1L,
                     as.integer(dbscan::dbscan(x[o, ], 0.5, 4)$cluster))
  }
})

test_that("morie_tps_dbscan_clusters matches dbscan::dbscan", {
  skip_if_not_installed("dbscan")
  df <- tps_df()
  res <- morie_tps_dbscan_clusters(df, eps_km = 0.5, min_samples = 5L)
  co <- as.matrix(df)
  km <- cbind(co[, 1] * 111, co[, 2] * 111 * cos(mean(co[, 1]) * pi / 180))
  ref <- dbscan::dbscan(km, eps = 0.5, minPts = 5L)$cluster
  expect_identical(as.integer(res$labels), as.integer(ref))
  expect_identical(res$n_clusters, length(setdiff(unique(ref), 0L)))
  expect_identical(res$n_noise, sum(ref == 0L))
  expect_identical(res$largest_cluster, as.integer(max(table(ref[ref > 0]))))
  expect_gte(res$n_clusters, 2L)
})

test_that("mrm_tps_moran_clustering DBSCAN summary matches dbscan::dbscan", {
  skip_if_not_installed("dbscan")
  df <- tps_df(seed = 3)
  res <- mrm_tps_moran_clustering(df, dbscan_eps = 0.4, dbscan_minpts = 5L)
  lat <- df$LAT_WGS84
  lon <- df$LONG_WGS84
  pts <- cbind(lat * 111, lon * 111 * cos(mean(lat) * pi / 180))
  cl <- dbscan::dbscan(pts, eps = 0.4, minPts = 5L)$cluster
  expect_identical(res$dbscan_n_clusters, length(unique(cl[cl != 0L])))
  expect_identical(res$dbscan_n_noise, sum(cl == 0L))
  expect_identical(res$dbscan_largest, as.integer(max(table(cl[cl != 0L]))))
})

test_that("morie_tps_render_* colour by the same clusters as dbscan::dbscan", {
  skip_if_not_installed("dbscan")
  skip_if_not_installed("ggplot2")
  df <- tps_df(seed = 5)
  pp <- morie_tps_project_xy(df$LAT_WGS84, df$LONG_WGS84)
  ref <- as.integer(dbscan::dbscan(cbind(pp$x, pp$y), eps = 0.6,
                                   minPts = 5L)$cluster)
  p1 <- morie_tps_render_dbscan(df, eps_km = 0.6, min_samples = 5L)
  expect_identical(as.integer(as.character(p1$data$cluster)), ref)
  p2 <- morie_tps_render_points(df, eps_km = 0.6, min_samples = 5L,
                                show_top = 100L)
  expect_identical(as.integer(as.character(p2$data$cluster)), ref - 1L)
})

test_that("the grid neighbour search (n > 300) gives the all-pairs neighbours", {
  within <- rmorie:::.morie_dbscan_within
  brute <- function(x, eps, m) {
    lapply(seq_len(nrow(x)), function(i) {
      which(within(lapply(seq_len(ncol(x)), function(j) x[i, j] - x[, j]), eps, m))
    })
  }
  set.seed(11)
  for (d in 1:3) for (m in c("euclidean", "manhattan", "chebyshev")) {
    # rounded coordinates put many points exactly eps apart, the boundary case
    x <- matrix(round(stats::rnorm(400 * d), 1), ncol = d)
    expect_identical(rmorie:::.morie_dbscan_grid_nbrs(x, 0.3, m), brute(x, 0.3, m))
  }
})

test_that("labels equal dbscan::dbscan's, also with points exactly eps apart", {
  skip_if_not_installed("dbscan")
  set.seed(12)
  # dbscan's distance sum is fused (a * a + s in one rounding) where its C++ is built for arm64,
  # so a pair exactly eps apart can fall on either side of eps there; the exact-tie cases are
  # compared on machines whose build rounds each step, as R does
  exact_ties <- !grepl("^(aarch64|arm64)", R.version$arch)
  if (exact_ties) for (d in 1:3) for (n in c(200L, 1200L)) for (eps in c(0.1, 0.3)) {
    # one-decimal coordinates: many pairs are exactly eps apart
    x <- matrix(round(stats::rnorm(n * d), 1), ncol = d)
    a <- rmorie:::.morie_dbscan_native(x, eps = eps, min_samples = 4L)
    b <- dbscan::dbscan(x, eps = eps, minPts = 4L)
    expect_identical(ifelse(a$labels == -1L, 0L, a$labels + 1L), as.integer(b$cluster))
  }
  x <- cbind(stats::runif(2000, 0, 20), stats::runif(2000, 0, 20))
  a <- rmorie:::.morie_dbscan_native(x, eps = 0.4, min_samples = 5L)
  b <- dbscan::dbscan(x, eps = 0.4, minPts = 5L)
  expect_identical(ifelse(a$labels == -1L, 0L, a$labels + 1L), as.integer(b$cluster))
})
