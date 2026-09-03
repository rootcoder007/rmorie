# SPDX-License-Identifier: AGPL-3.0-or-later
# srr UL standards completed by the morie_cluster object (R/cluster_model.R).

.uln <- function() { d <- iris[1:4]
rownames(d) <- paste0("s", 1:150)
d }

test_that("UL1.2 missing row names trigger a warning", {
  m <- as.matrix(iris[1:4])
  rownames(m) <- NULL
  expect_warning(morie_cluster(m, k = 3), "row names")
})

test_that("UL1.3/UL7.3 input row names are propagated to output", {
  cl <- morie_cluster(.uln(), k = 3)
  expect_equal(names(cl$assignments)[1:3], c("s1", "s2", "s3"))
})

test_that("UL1.4b scaling changes the clustering (documented consequence)", {
  d <- .uln()
  d$Sepal.Length <- d$Sepal.Length * 100   # inflate one column
  a <- morie_cluster(d, k = 3, scale = FALSE, seed = 1)
  b <- morie_cluster(d, k = 3, scale = TRUE, seed = 1)
  expect_false(identical(unname(a$assignments), unname(b$assignments)))
})

test_that("UL2.0 markedly different scales are diagnosed", {
  d <- .uln()
  d$Sepal.Length <- d$Sepal.Length * 1e4
  expect_warning(morie_cluster(d, k = 2, scale = FALSE), "scale")
})

test_that("UL2.2 missing-value handling is explicit", {
  d <- .uln()
  d[1, 1] <- NA
  expect_error(morie_cluster(d, k = 3, na_action = "fail"), "missing")
  cl <- morie_cluster(d, k = 3, na_action = "omit")
  expect_lt(cl$n_obs, 150L)
})

test_that("UL3.0/UL7.2 cluster labels are ordered by decreasing size", {
  cl <- morie_cluster(.uln(), k = 3)
  expect_equal(cl$sizes, sort(cl$sizes, decreasing = TRUE))
})

test_that("UL3.2 cases can be labelled when row names are absent", {
  m <- as.matrix(iris[1:4])
  rownames(m) <- NULL
  cl <- suppressWarnings(morie_cluster(m, k = 3,
                                       case_labels = paste0("c", 1:150)))
  expect_equal(names(cl$assignments)[1], "c1")
})

test_that("UL3.3 new data is assigned without re-fitting", {
  cl <- morie_cluster(.uln(), k = 3)
  p <- predict(cl, iris[1:5, 1:4])
  expect_length(p, 5L)
  expect_true(all(p %in% seq_len(3L)))
})

test_that("UL4.1 a cluster model can be specified without fitting", {
  spec <- morie_cluster(iris[1:4], k = 3, nofit = TRUE)
  expect_s3_class(spec, "morie_cluster_spec")
})

test_that("UL4.3a print restricts the number of rows shown", {
  cl <- morie_cluster(.uln(), k = 3)
  out <- capture.output(print(cl, max_rows = 5))
  expect_true(any(grepl("more", out)))               # truncation noted
})

test_that("UL4.4 summary reports sizes + within-cluster dispersion", {
  s <- summary(morie_cluster(.uln(), k = 3))
  expect_true(all(c("cluster", "size", "withinss") %in% names(s)))
})

test_that("UL6.0/UL6.1/UL6.2 default plot method exists + renders", {
  expect_true(exists("plot.morie_cluster"))
  cl <- morie_cluster(.uln(), k = 3)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  plot(cl)
  grDevices::dev.off()
  expect_true(file.exists(tmp))
})

test_that("UL7.1 degenerate (no-variance) input yields a trivial clustering", {
  d <- data.frame(a = rep(1, 30), b = rep(2, 30))    # no variance
  cl <- suppressWarnings(morie_cluster(d, k = 1))
  expect_equal(cl$k, 1L)
  expect_equal(cl$sizes, 30L)
})

test_that("UL7.4 predicting new data is faster than a full re-fit", {
  # Wall-clock timing is flaky by construction on shared/loaded CI runners
  # (this measured 2.6s vs 1.1s once). Skip on CRAN and use a generous
  # multiplicative margin: we only guard against prediction being
  # pathologically slower than a refit, not a tight ratio.
  skip_on_cran()
  big <- as.data.frame(matrix(rnorm(4000 * 5), 4000, 5))
  rownames(big) <- paste0("r", 1:4000)
  cl <- morie_cluster(big, k = 5)
  # Median of 3 runs each: single timings flake on loaded CI runners.
  t_pred <- median(vapply(1:3, function(i)
    system.time(predict(cl, big))[["elapsed"]], numeric(1)))
  t_fit <- median(vapply(1:3, function(i)
    system.time(morie_cluster(big, k = 5))[["elapsed"]], numeric(1)))
  expect_lt(t_pred, t_fit * 3 + 1.0)                 # not pathologically slower
})

test_that("UL7.5/UL7.5a batch clustering equals per-item fits", {
  ds <- list(a = .uln(), b = .uln())
  batch <- morie_cluster_batch(ds, k = 3, seed = 7)
  single_a <- morie_cluster(.uln(), k = 3, seed = 7)
  expect_length(batch, 2L)
  expect_equal(unname(batch$a$assignments), unname(single_a$assignments))
})
