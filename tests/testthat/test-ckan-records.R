# SPDX-License-Identifier: AGPL-3.0-or-later
# datastore/dump?format=json sends records as arrays with the names in `fields`; every live OTIS
# table came back as V1..V4 with CKAN's _id as a column.

test_that("array records take their names from fields, and _id is dropped", {
  f <- get(".morie_ckan_records_frame", envir = environment(morie_datasets_otis_b05_seg_consecutive_lengths))
  rec <- matrix(c("1", "2023", "1 day", "31692", "2", "2023", "2 days", "8605"), nrow = 2, byrow = TRUE)
  fields <- data.frame(id = c("_id", "EndFiscalYear", "Consecutive_Duration", "Number_SegregationPlacements"))
  df <- f(rec, fields)
  expect_named(df, c("EndFiscalYear", "Consecutive_Duration", "Number_SegregationPlacements"))
  expect_identical(df$Consecutive_Duration, c("1 day", "2 days"))
  # fields as a list of objects, the other JSON decoding
  df2 <- f(rec, lapply(fields$id, function(i) list(id = i, type = "text")))
  expect_identical(df2, df)
  # records already named (object form) keep their names
  obj <- data.frame(`_id` = 1:2, a = c("x", "y"), check.names = FALSE)
  expect_named(f(obj, NULL), "a")
})
