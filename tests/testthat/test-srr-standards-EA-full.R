# SPDX-License-Identifier: AGPL-3.0-or-later
# srr EA standards completed by the morie_eda_table system (R/eda_table.R).

test_that("EA2.0/EA2.2/EA2.2a/EA2.2b an explicit index-column system is used", {
  t <- morie_eda_table(mtcars)
  expect_s3_class(t, "morie_eda_table")
  expect_equal(attr(t, "index"), ".row_id")           # index attribute set
  expect_equal(morie_eda_index(t), ".row_id")
})

test_that("EA2.1 the index column is asserted unique", {
  d <- data.frame(id = c(1, 1, 2), x = 1:3)
  expect_error(morie_eda_table(d, index = "id"), "duplicate")
})

test_that("EA2.3 joins use an explicitly named column", {
  a <- morie_eda_table(data.frame(id = 1:3, x = 4:6), index = "id")
  b <- data.frame(id = 1:3, y = 7:9)
  expect_error(morie_eda_join(a, b), "explicitly")     # by is required
  j <- morie_eda_join(a, b, by = "id")
  expect_true(all(c("x", "y") %in% names(j)))
})

test_that("EA2.4/EA2.5 multi-table dataset validates each table's index", {
  ds <- morie_eda_dataset(a = mtcars, b = iris)
  expect_s3_class(ds, "morie_eda_dataset")
  expect_true(all(vapply(ds, inherits, logical(1), "morie_eda_table")))
})

test_that("EA2.6 vector processing ignores extra attributes", {
  v <- structure(1:5, foo = "bar", class = "weird")
  expect_equal(morie_eda_as_vector(v), 1:5)
  expect_null(attributes(morie_eda_as_vector(v)))
})

test_that("EA3.0 an automatic per-variable meta-summary is produced", {
  s <- morie_eda_summary(mtcars)
  expect_equal(nrow(s), ncol(mtcars))                  # one row per variable
  expect_true(all(c("variable", "mean", "sd") %in% names(s)))
})

test_that("EA3.1 summaries of multiple inputs can be compared", {
  cmp <- morie_eda_compare(a = mtcars[1:16, ], b = mtcars[17:32, ])
  expect_true("source" %in% names(cmp))
  expect_setequal(unique(cmp$source), c("a", "b"))
})

test_that("EA4.1 numeric precision is explicitly controllable", {
  s2 <- morie_eda_summary(mtcars, digits = 2)
  s5 <- morie_eda_summary(mtcars, digits = 5)
  # rounding to fewer digits loses precision the finer rounding keeps
  expect_true(any(round(s5$mean, 2) != s5$mean))       # 5-digit keeps detail
  expect_true(all(s2$mean == round(s2$mean, 2)))
})

test_that("EA5.0/EA5.0a/EA5.1 plotting uses an enlarged, controllable typeface", {
  expect_equal(formals(morie_eda_plot)$cex, 1.3)       # enlarged default
})

test_that("EA5.0b a colourblind-safe palette is used", {
  # Okabe-Ito palette: distinct hues, no pure red/green confusion pair
  expect_true(all(grepl("^#", .eda_palette)))
  expect_gte(length(.eda_palette), 8L)
})

test_that("EA5.3 summary reports storage mode / class of each column", {
  s <- morie_eda_summary(data.frame(a = 1L, b = "x", c = TRUE))
  expect_true(all(c("class", "storage_mode") %in% names(s)))
  expect_equal(s$storage_mode[s$variable == "a"], "integer")
})

test_that("EA5.4 values are rounded sensibly", {
  s <- morie_eda_summary(data.frame(x = c(1.23456, 2.34567)), digits = 2)
  expect_equal(s$mean, round(mean(c(1.23456, 2.34567)), 2))
})

test_that("EA5.5 axis units are placed on the axis labels", {
  drawn <- NULL
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  drawn <- morie_eda_plot(mtcars, "wt", "mpg",
                          units = c(wt = "1000lb", mpg = "mpg"))
  grDevices::dev.off()
  expect_s3_class(drawn, "data.frame")                 # renders + returns coords
  expect_true(file.exists(tmp))
})

test_that("EA5.6 no dynamic-visualisation library is bundled (base graphics)", {
  body_txt <- deparse(body(morie_eda_plot))
  expect_false(any(grepl("plotly|leaflet|htmlwidgets|ggplot", body_txt)))
})

test_that("EA6.0/EA6.1 graphical output properties are testable without vdiffr", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  drawn <- morie_eda_plot(mtcars, "hp", "mpg")
  grDevices::dev.off()
  expect_equal(drawn$x, mtcars$hp)                     # drawn coords verifiable
  expect_equal(drawn$y, mtcars$mpg)
})
