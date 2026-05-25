# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_temporal_records <- function(n_per_cell = 30, seed = 1) {
  set.seed(seed)
  periods <- c("2020","2021","2022")
  cities <- c("C1","C2")
  rows <- list()
  for (p in periods) for (c in cities) {
    grp <- sample(c("X","Y"), n_per_cell, replace = TRUE)
    base_p <- ifelse(grp == "X", 0.7, 0.4)
    y_pred <- rbinom(n_per_cell, 1, base_p)
    rows[[length(rows) + 1L]] <- data.frame(
      period = p, city = c, y_pred = y_pred, group = grp,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

test_that("morie_fairness_predpol_temporal_audit returns rich result", {
  d <- make_temporal_records()
  r <- morie_fairness_predpol_temporal_audit(
    d$period, d$city, d$y_pred, d$group, privileged = "X"
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_true(is.finite(r$payload$worst_dir_range))
  expect_true("C1" %in% names(r$payload$per_city))
  expect_true("C2" %in% names(r$payload$per_city))
})

test_that("morie_fairness_predpol_temporal_audit infers privileged globally", {
  d <- make_temporal_records()
  r <- morie_fairness_predpol_temporal_audit(
    d$period, d$city, d$y_pred, d$group
  )
  expect_true(length(r$warnings) >= 1L)
  expect_true(nzchar(r$payload$privileged))
})

test_that("morie_fairness_predpol_temporal_audit errors on misaligned", {
  expect_error(
    morie_fairness_predpol_temporal_audit(c("p"), c("c"), c(1, 0),
                                          c("X","Y")),
    "must all align"
  )
})

test_that("morie_fairness_predpol_temporal_audit errors on empty", {
  expect_error(
    morie_fairness_predpol_temporal_audit(character(0), character(0),
                                          integer(0), character(0)),
    "empty"
  )
})

test_that("morie_fairness_predpol_temporal_audit errors when no cell has 2 groups", {
  expect_error(
    morie_fairness_predpol_temporal_audit(
      rep("p1", 6), rep("c1", 6), rep(1, 6), rep("X", 6),
      privileged = "X"
    ),
    "no .* cell"
  )
})

test_that("morie_fairness_predpol_temporal_audit per-city aggregates structure", {
  d <- make_temporal_records()
  r <- morie_fairness_predpol_temporal_audit(
    d$period, d$city, d$y_pred, d$group, privileged = "X"
  )
  pc <- r$payload$per_city$C1
  expect_true(all(c("n_periods","mean_dir","mean_parity_gap",
                    "mean_gini","mean_bas","dir_range") %in% names(pc)))
})