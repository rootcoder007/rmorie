# SPDX-License-Identifier: AGPL-3.0-or-later
# Native Parquet reader: three-level LIST columns (the A2AJ citation fields).

set.seed(1)

.pq_fixture <- function(name) {
  testthat::test_path("fixtures", "parquet", paste0(name, ".parquet"))
}

.pq_expected <- list(
  c("2020 SCC 5", "2019 ONCA 1"), character(0), NULL, c("x", NA), "2001 FC 3"
)

test_that("a LIST column round-trips null list, empty list and null element", {
  set.seed(1)
  for (fx in c("list_snappy", "list_dict", "list_plain")) {
    df <- morie_read_parquet(.pq_fixture(fx))
    expect_equal(names(df), c("id", "name", "cites"), info = fx)
    expect_equal(df$id, 1:5, info = fx)
    expect_equal(df$name, c("a", "b", NA, "d", "e"), info = fx)
    expect_s3_class(df$cites, "AsIs")
    expect_equal(unclass(df$cites), .pq_expected, info = fx)
  }
})

test_that("a LIST column survives column selection", {
  set.seed(1)
  df <- morie_read_parquet(.pq_fixture("list_snappy"), columns = c("cites", "id"))
  expect_equal(names(df), c("cites", "id"))
  expect_equal(unclass(df$cites), .pq_expected)
})

test_that("flat columns still decode after the schema walk change", {
  set.seed(1)
  flat <- data.frame(a = 1:3, b = c("x", "y", NA), c = c(1.5, NA, 3),
    stringsAsFactors = FALSE
  )
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf))
  morie_write_parquet(flat, tf)
  back <- morie_read_parquet(tf)
  expect_equal(back$a, 1:3)
  expect_equal(back$b, c("x", "y", NA))
  expect_equal(back$c, c(1.5, NA, 3))
})
