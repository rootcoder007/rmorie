# SPDX-License-Identifier: AGPL-3.0-or-later

# Realistic corridor-policy DGP with a true ATE of 2: treatment is assigned at
# the CORRIDOR level (a VFR ban covers a whole corridor) via an observed
# corridor covariate z; individual covariate x is within-corridor; a corridor
# random effect u enters the outcome. Cluster-level treatment guarantees strong
# within-cluster correlation, so the cluster-robust SE must exceed the iid SE.
.make_clustered <- function(G = 80L, ng = 15L, seed = 1L) {
  set.seed(seed)
  n <- G * ng
  g <- rep(seq_len(G), each = ng)
  z_g <- stats::rnorm(G)                              # corridor covariate
  d_g <- stats::rbinom(G, 1, stats::plogis(0.8 * z_g))  # corridor-level treatment
  u_g <- stats::rnorm(G, 0, 1.0)                      # corridor random effect
  z <- z_g[g]
  d <- d_g[g]
  u <- u_g[g]
  x <- stats::rnorm(n)
  y <- 2 * d + 0.5 * z + x + u + stats::rnorm(n, 0, 0.5)   # true ATE = 2
  data.frame(y = y, d = d, x = x, z = z, corridor = g)
}

test_that("clustered DML recovers the known ATE", {
  df <- .make_clustered()
  res <- morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = "corridor")
  expect_s3_class(res, "morie_dml_clustered")
  expect_equal(res$ate, 2, tolerance = 0.5)
  expect_true(res$se > 0)
  expect_identical(res$n_clusters, 80L)
  expect_true(res$ci95[1] < res$ate && res$ate < res$ci95[2])
})

test_that("cluster-robust SE exceeds the iid SE under within-cluster correlation", {
  df <- .make_clustered()
  iid <- morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = NULL)
  cl <- morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = "corridor")
  expect_identical(iid$se_kind, "iid")
  expect_match(cl$se_kind, "cluster-robust")
  expect_gt(cl$se, iid$se)
})

test_that("two-way clustering runs and reports CGM", {
  df <- .make_clustered()
  df$week <- rep(rep(seq_len(6L), each = 2L), length.out = nrow(df))
  res <- morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = c("corridor", "week"))
  expect_match(res$se_kind, "2-way")
  expect_true(is.finite(res$se))
})

test_that("externally supplied propensity is honoured", {
  df <- .make_clustered()
  ps <- rep(mean(df$d), nrow(df))          # trivial constant PS
  res <- morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = "corridor", ps = ps)
  expect_true(is.finite(res$ate))
})

test_that("input validation", {
  df <- .make_clustered()
  expect_error(morie_dml_clustered(df, "d", "y", c("x", "z"), cluster = c("a", "b", "c")),
               "at most two-way")
  expect_error(morie_dml_clustered(df, "d", "y", "nope"), "not found")
})
