# SPDX-License-Identifier: AGPL-3.0-or-later
# Every table rmoriedata ships twice, CSV and Parquet: this package's reader
# must give the same typed frame from the Parquet copy as rmoriedata's CSV.

test_that("the Parquet copy of every rmoriedata table equals the CSV copy", {
  skip_if_not_installed("rmoriedata")
  cat <- rmoriedata::mdcat()
  t <- cat[cat$kind == "table", ]
  expect_true(all(nzchar(t$parquet_path)))
  for (slug in t$slug) {
    a <- rmoriedata::mdload(slug)
    b <- morie_read_parquet(rmoriedata::mdpath(slug))
    expect_identical(dim(b), dim(a), label = slug)
    expect_identical(names(b), names(a), label = slug)
    expect_identical(vapply(b, function(z) class(z)[1L], ""),
                     vapply(a, function(z) class(z)[1L], ""), label = slug)
    expect_equal(b, a, ignore_attr = TRUE, label = slug)
  }
})
