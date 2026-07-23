# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Direct execution of the two 2026-07-22 prep-batch fixes that no other
# test reaches: the cmdscale MDS init in the Bayesian spatial-voting
# sampler, and the node-size lookup in the criminal-network graph.

test_that(".morie_sv_bayes_mds initialises from cmdscale and samples", {
  set.seed(7)
  X0 <- matrix(rnorm(12), nrow = 6L)
  D <- as.matrix(stats::dist(X0))
  r <- rmorie:::.morie_sv_bayes_mds(D, n_dims = 2L, n_samples = 20L,
                                    burn_in = 5L)
  expect_type(r, "list")

  expect_error(
    rmorie:::.morie_sv_bayes_mds(matrix(0, 3L, 3L), n_dims = 2L,
                                 n_samples = 5L, burn_in = 1L),
    "positive"
  )
})

test_that("criminal-network graph computes node sizes from premises freq", {
  df <- data.frame(
    HOOD_158 = rep(sprintf("H%02d", 1:6), times = 4L),
    PREMISES_TYPE = rep(c("House", "Apartment", "Outside", "Commercial"),
                        each = 6L),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    morie_tps_load_tps_dataset = function(category, nrows = NULL, ...) df,
    .package = "rmorie"
  )
  r <- morie_tps_criminal_network_graph(category = "Assault",
                                        sample_rows = 100L,
                                        top_n_premises = 4L,
                                        save_fig = FALSE)
  expect_type(r, "list")
  expect_true(grepl("Criminal network", r$title))
})
