# Coverage tests for R/clusmd_native.R (Butina 1999): fingerprint
# parsing, Tanimoto neighbour lists, exclusion-sphere clustering with and
# without recounting, and the summary.

# a graph encoded as fingerprints: each node holds the ids of its edges
# plus a private bit, so two nodes share a bit exactly when adjacent
cm_graph <- function() {
  E <- rbind(c(1, 2), c(1, 3), c(1, 4), c(2, 5), c(3, 5), c(5, 6), c(6, 7), c(6, 8))
  lapply(1:8, function(v) c(which(E[, 1] == v | E[, 2] == v), 100 + v))
}

test_that("fingerprint formats and Tanimoto neighbour lists", {
  expect_equal(.clusmd_fp(c(5, 2, 2)), c(2L, 5L))
  expect_equal(.clusmd_fp(c(0, 1, 1, 0)), c(1L, 2L))
  expect_equal(.clusmd_fp(c(FALSE, TRUE, TRUE)), c(1L, 2L))
  expect_equal(.clusmd_fp(list(3, 1)), c(1L, 3L))
  expect_equal(.clusmd_fp(matrix(c(0, 1, 1, 0), 1)), c(1L, 2L))
  expect_error(.clusmd_fp(c(-1, 3)), "cannot be negative")
  expect_error(.clusmd_fp(c(3, 9), n_bits = 8), "outside the 8-bit")
  expect_error(.clusmd_fp(matrix(c(0, 2, 1, 0), 1)), "only 0/1")
  expect_equal(.clusmd_tanimoto(c(1, 2, 3), c(2, 3, 4, 5)), 2 / 5)
  expect_error(.clusmd_tanimoto(integer(0), integer(0)), "both fingerprints are empty")
  fps <- list(c(1, 2, 3), c(1, 2, 3, 4), c(1, 2), c(7, 8), c(7, 8, 9), 20)
  nb <- morie_clusmd_neighbour_lists(fps, 0.6)
  ref <- lapply(1:6, function(i) which(vapply(1:6, function(j) j != i && length(intersect(fps[[i]], fps[[j]])) / length(union(fps[[i]], fps[[j]])) >= 0.6, TRUE)))
  expect_equal(nb, ref)
  expect_equal(nb[[1]], c(2L, 3L))
  expect_identical(morie_clusmd, morie_clusmd_neighbour_lists)
  expect_error(morie_clusmd_neighbour_lists(fps, 1.5), "threshold must lie")
  expect_error(morie_clusmd_neighbour_lists(list()), "no compounds")
  expect_error(morie_clusmd_neighbour_lists(list(integer(0), integer(0))), "both fingerprints are empty")
})

test_that("Butina clustering: fixed and recounted neighbour counts", {
  fps <- list(c(1, 2, 3), c(1, 2, 3, 4), c(1, 2), c(7, 8), c(7, 8, 9), 20)
  cl <- morie_clusmd_butina_clusters(fps, 0.6)
  expect_equal(lapply(cl, `[[`, "members"), list(1:3, 4:5, 6L))
  expect_equal(vapply(cl, `[[`, 1L, "centroid"), c(1L, 4L, 6L))
  g <- cm_graph()
  # the hub 1 takes {1,2,3,4}; without recounting node 5 keeps its full
  # degree 3 and wins the tie with 6, with recounting 6 has the most live
  # neighbours and absorbs 5, 7 and 8
  fixed <- morie_clusmd_butina_clusters(g, 0.05)
  expect_equal(lapply(fixed, `[[`, "members"), list(1:4, 5:6, 7L, 8L))
  rec <- morie_clusmd_butina_clusters(g, 0.05, recount = TRUE)
  expect_equal(lapply(rec, `[[`, "members"), list(1:4, 5:8))
  expect_equal(rec[[2]]$centroid, 6L)
})

test_that("cluster summary and the rich result", {
  g <- cm_graph()
  cl <- morie_clusmd_butina_clusters(g, 0.05)
  s <- morie_clusmd_cluster_summary(cl)
  expect_equal(s$assignment, c(1L, 1L, 1L, 1L, 2L, 2L, 3L, 4L))
  expect_equal(s$sizes, c(4L, 2L, 1L, 1L))
  expect_equal(s$n_singletons, 2L)
  expect_equal(s$centroids, c(1L, 5L, 7L, 8L))
  r <- morie_clusmd_butina_clustering(g, 0.05, recount = TRUE)
  expect_s3_class(r, "morie_clusmd_result")
  expect_equal(r$clusters, morie_clusmd_butina_clusters(g, 0.05, TRUE))
  expect_equal(r$assignment, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_equal(r$n_clusters, 2L)
  expect_true(r$recount)
  expect_match(r$interpretation, "2 clusters covering 8 compounds, 0 singletons")
})
