# R/datasets_vic.R: the native .xlsx reader on a workbook served from strings (no zip, no
# network), the table and sheet entry points with the download and bundle replaced, and the
# three analyses recomputed.

.vic_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.vic <- function(name) get(name, envir = environment(morie_datasets_vic_table))

# a two-sheet workbook: shared strings, a boolean, an inline string, a numeric, a gap
.vic_parts <- function(with_rels = TRUE) {
  parts <- list(
    "xl/workbook.xml" = paste0(
      "<workbook xmlns:r=\"rel\"><sheets>",
      "<sheet name=\"Table 01\" sheetId=\"1\" r:id=\"rId2\"/>",
      "<sheet name=\"Table 02\" sheetId=\"2\" r:id=\"rId1\"/>",
      "</sheets></workbook>"),
    "xl/sharedStrings.xml" = paste0(
      "<sst><si><t>Year</t></si><si><t>Local Government Area</t></si>",
      "<si><r><t>Al</t></r><r><t>pha</t></r></si><si><t>Beta</t></si></sst>"),
    "xl/worksheets/sheet1.xml" = paste0(
      "<worksheet><sheetData>",
      "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" t=\"s\"><v>1</v></c>",
      "<c r=\"C1\" t=\"inlineStr\"><is><t>Flag</t></is></c><c r=\"D1\"/></row>",
      "<row r=\"2\"><c r=\"A2\"><v>2024</v></c><c r=\"B2\" t=\"s\"><v>2</v></c>",
      "<c r=\"C2\" t=\"b\"><v>1</v></c><c r=\"D2\"><v>1.5</v></c></row>",
      "<row r=\"3\"><c r=\"A3\"><v>2025</v></c><c r=\"B3\" t=\"s\"><v>3</v></c>",
      "<c r=\"C3\" t=\"b\"><v>0</v></c></row>",
      "<row r=\"4\"><c r=\"A4\"><v>2026</v></c><c r=\"B4\" t=\"s\"><v>9</v></c></row>",
      "</sheetData></worksheet>"),
    "xl/worksheets/sheet2.xml" = "<worksheet><sheetData></sheetData></worksheet>")
  if (with_rels) {
    parts[["xl/_rels/workbook.xml.rels"]] <- paste0(
      "<Relationships><Relationship Id=\"rId1\" Target=\"/xl/worksheets/sheet2.xml\"/>",
      "<Relationship Id=\"rId2\" Target=\"worksheets/sheet1.xml\"/>",
      "<Relationship Id=\"rId3\"/></Relationships>")
  }
  parts
}

.vic_mock_zip <- function(parts, env) {
  testthat::local_mocked_bindings(
    .morie_vic_zip_text = function(path, member) if (is.null(parts[[member]])) "" else parts[[member]],
    .morie_vic_zip_has = function(path, member) !is.null(parts[[member]]),
    .package = .vic_pkg, .env = env)
  testthat::local_mocked_bindings(
    unzip = function(zipfile, ..., list = FALSE) data.frame(Name = names(parts), stringsAsFactors = FALSE),
    .package = "utils", .env = env)
}

test_that("the workbook helpers strip namespaces, index columns, and map sheets through the relationships", {
  expect_equal(.vic(".morie_vic_local")("ns0:sheet"), "sheet")
  expect_equal(.vic(".morie_vic_local")("sheet"), "sheet")
  ci <- .vic(".morie_vic_col_index")
  expect_equal(ci("A1"), 1L)
  expect_equal(ci("Z9"), 26L)
  expect_equal(ci("AA1"), 27L)
  expect_equal(ci("BC12"), 55L)
  .vic_mock_zip(.vic_parts(), environment())
  sm <- .vic(".morie_vic_sheet_map")("book.xlsx")
  expect_equal(sm$names, c("Table 01", "Table 02"))
  # the relationship ids, not the part numbers, decide which sheet is which
  expect_equal(sm$parts, c("xl/worksheets/sheet1.xml", "xl/worksheets/sheet2.xml"))
  expect_equal(.vic(".morie_vic_sheet_names")("book.xlsx"), c("Table 01", "Table 02"))
  expect_equal(.vic(".morie_vic_shared_strings")("book.xlsx"), c("Year", "Local Government Area", "Alpha", "Beta"))
})

test_that("without relationships the sheet parts are taken in numeric order", {
  parts <- .vic_parts(with_rels = FALSE)
  parts[["xl/worksheets/sheet10.xml"]] <- "<worksheet/>"
  .vic_mock_zip(parts, environment())
  sm <- .vic(".morie_vic_sheet_map")("book.xlsx")
  expect_equal(sm$parts, c("xl/worksheets/sheet1.xml", "xl/worksheets/sheet2.xml"))
  parts[["xl/sharedStrings.xml"]] <- NULL
  .vic_mock_zip(parts, environment())
  expect_equal(.vic(".morie_vic_shared_strings")("book.xlsx"), character(0))
})

test_that("a sheet is read cell by cell: shared and inline strings, booleans, numbers, gaps", {
  .vic_mock_zip(.vic_parts(), environment())
  rs <- .vic(".morie_vic_read_sheet")
  df <- rs("book.xlsx", "Table 01")
  expect_equal(names(df), c("Year", "Local Government Area", "Flag", "V4"))
  expect_equal(df$Year, c(2024, 2025, 2026))
  expect_equal(df[["Local Government Area"]], c("Alpha", "Beta", NA))
  expect_equal(df$Flag, c("TRUE", "FALSE", NA))
  expect_equal(df$V4, c(1.5, NA, NA))
  expect_equal(nrow(rs("book.xlsx", "Table 02")), 0L)
  expect_error(rs("book.xlsx", "Table 03"), "no sheet named 'Table 03'; the workbook has: Table 01, Table 02")
  two <- rs("book.xlsx", "Table 01", header = 2L)
  expect_equal(names(two), c("2024", "Alpha", "TRUE", "1.5"))
  expect_equal(two[[1]], c(2025, 2026))
})

test_that("the table entry point resolves keys, caches, falls back to the bundle, and names its sheet", {
  dir <- withr::local_tempdir()
  withr::local_envvar(MORIE_VIC_CACHE = NA)
  expect_equal(morie_datasets_vic_cache_dir(dir), dir)
  withr::local_envvar(MORIE_VIC_CACHE = file.path(dir, "fromenv"))
  expect_equal(morie_datasets_vic_cache_dir(), file.path(dir, "fromenv"))
  expect_true(dir.exists(file.path(dir, "fromenv")))
  expect_error(morie_datasets_vic_table(1), "must be a single catalog key")
  expect_error(morie_datasets_vic_table("nope"), "unknown key 'nope'")
  expect_error(morie_datasets_vic_sheets("nope"), "unknown key 'nope'")
  key <- morie_datasets_vic_catalog()$key[1]
  file <- morie_datasets_vic_catalog()$file[1]
  testthat::local_mocked_bindings(.morie_vic_bundled = function(key) NULL, .package = .vic_pkg)
  expect_equal(morie_datasets_vic_table(key, cache_dir = dir), data.frame())
  expect_equal(morie_datasets_vic_sheets(key, cache_dir = dir), character(0))
  testthat::local_mocked_bindings(.morie_vic_bundled = function(key) data.frame(sample = key), .package = .vic_pkg)
  expect_equal(morie_datasets_vic_table(key, cache_dir = dir), data.frame(sample = key))
  # online: the workbook is downloaded once, then read by sheet name
  seen <- character()
  testthat::local_mocked_bindings(
    .morie_dl = function(url, dest, ...) {
      seen <<- c(seen, url)
      writeLines("fake", dest)
      invisible(dest)
    },
    .morie_vic_read_sheet = function(path, sheet, header = 1L) data.frame(path = basename(path), sheet = sheet),
    .morie_vic_sheet_names = function(path) c("Table 01", "Extra"),
    .package = .vic_pkg)
  got <- morie_datasets_vic_table(key, table = 2, cache_dir = dir, offline = FALSE)
  expect_equal(got, data.frame(path = file, sheet = "Table 02"))
  expect_length(seen, 1)
  expect_equal(morie_datasets_vic_table(key, table = "Extra", cache_dir = dir)$sheet, "Extra")
  expect_length(seen, 1)
  expect_equal(morie_datasets_vic_sheets(key, cache_dir = dir), c("Table 01", "Extra"))
})

test_that("the bundle lookup degrades to NULL without rmoriedata, a failing loader or an empty frame", {
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_null(.vic(".morie_vic_bundled")("x"))
})

test_that("the bundle lookup returns the rmoriedata sample when there is one", {
  skip_if_not_installed("rmoriedata")
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) TRUE, .package = "base")
  testthat::local_mocked_bindings(mdload = function(slug) stop("no such slug"), .package = "rmoriedata")
  expect_null(.vic(".morie_vic_bundled")("x"))
  testthat::local_mocked_bindings(mdload = function(slug) data.frame(), .package = "rmoriedata")
  expect_null(.vic(".morie_vic_bundled")("x"))
  testthat::local_mocked_bindings(mdload = function(slug) data.frame(slug = slug), .package = "rmoriedata")
  expect_equal(.vic(".morie_vic_bundled")("x"), data.frame(slug = "vic_x"))
})

test_that("the offence trend, LGA ranking and Indigenous count ratio recompute", {
  d <- data.frame(Year = c(2020, 2020, 2021, 2021, 2022, 2022),
                  `Offence Division` = c("A", "B", "A", "B", "A", "B"),
                  `Incidents Recorded` = c(100, 50, 120, 40, 150, NA), check.names = FALSE)
  tr <- morie_vic_offence_trend(d)
  expect_equal(tr$division, c("A", "B"))
  expect_equal(tr$first_count, c(100, 50))
  expect_equal(tr$last_count, c(150, 0))
  expect_equal(tr$abs_change, c(50, -50))
  expect_equal(tr$pct_change, c(50, -100))
  expect_equal(tr$first_year, c(2020, 2020))
  expect_equal(tr$last_year, c(2022, 2022))
  z <- morie_vic_offence_trend(data.frame(Year = 1, `Offence Division` = "Z", `Incidents Recorded` = 0,
                                          check.names = FALSE))
  expect_true(is.na(z$pct_change))
  expect_equal(nrow(morie_vic_offence_trend(d[0, ])), 0L)
  expect_error(morie_vic_offence_trend(d[, 1:2]), "missing column\\(s\\): Incidents Recorded")
  l <- data.frame(Year = c(2024, 2025, 2025, 2025), `Local Government Area` = c("Old", "Alpha", "Beta", "Gamma"),
                  `Incidents Recorded` = c(1, 500, 300, 900), `Rate per 100,000 population` = c(9, 1200, 800, 2500),
                  check.names = FALSE)
  r <- morie_vic_lga_rates(l, top = 2)
  expect_equal(r$lga, c("Gamma", "Alpha"))
  expect_equal(r$rank, 1:2)
  expect_equal(r$rate, c(2500, 1200))
  expect_equal(morie_vic_lga_rates(l, year = 2024)$lga, "Old")
  expect_equal(nrow(morie_vic_lga_rates(l[0, ])), 0L)
  expect_error(morie_vic_lga_rates(l[, 1:3]), "missing column\\(s\\): Rate per 100,000 population")
  i <- data.frame(Year = c(2023, 2023, 2023, 2024, 2024, 2024),
                  `Offence Division` = c("A", "A", "A", "A", "A", "B"),
                  `Indigenous Status` = c("Aboriginal and/or Torres Strait Islander", "Non-Indigenous", "Total People",
                                          "Aboriginal and/or Torres Strait Islander", "Non-Indigenous",
                                          "Aboriginal and/or Torres Strait Islander"),
                  `Victim Reports` = c(8, 179, 187, 10, 200, 5), check.names = FALSE)
  rr <- morie_vic_indigenous_ratio(i)
  expect_equal(rr$group, c("A", "B"))
  expect_equal(rr$indigenous, c(10, 5))
  expect_equal(rr$non_indigenous, c(200, 0))
  expect_equal(rr$count_ratio, c(0.05, NA))
  r23 <- morie_vic_indigenous_ratio(i, year = 2023)
  expect_equal(r23$count_ratio, 8 / 179)
  expect_equal(nrow(morie_vic_indigenous_ratio(i, year = 1999)), 0L)
  expect_equal(nrow(morie_vic_indigenous_ratio(i[0, ])), 0L)
  noyear <- morie_vic_indigenous_ratio(i[, -1])
  expect_equal(noyear$indigenous, c(18, 5))
  expect_error(morie_vic_indigenous_ratio(i, by = "Region"), "missing column\\(s\\): Region")
})
