# SPDX-License-Identifier: AGPL-3.0-or-later
# Regression: .otis_cluster_se must ignore empty cluster levels. A factor
# `cluster` can carry levels with no rows in a subset (e.g. per-year analysis
# of an individual-clustered panel); tapply() then emits NA for those empty
# clusters. Before the fix, sum(grp^2) became NA -> NA SE -> downstream
# `if (se > 0)` errored ("missing value where TRUE/FALSE needed").

test_that(".otis_cluster_se ignores empty cluster levels (no NA SE)", {
  scores   <- c(1.0, 0.5, 2.0, 1.0, 0.3, 0.2)  # non-cancelling within clusters
  cl_empty <- factor(c("a", "a", "b", "b", "c", "c"),
                     levels = c("a", "b", "c", "z"))  # "z" present but unused
  cl_clean <- factor(c("a", "a", "b", "b", "c", "c"))

  se_empty <- .otis_cluster_se(scores, cl_empty)
  se_clean <- .otis_cluster_se(scores, cl_clean)

  expect_true(is.finite(se_empty))
  expect_gt(se_empty, 0)
  expect_equal(se_empty, se_clean)  # the empty level must not change the SE
})
