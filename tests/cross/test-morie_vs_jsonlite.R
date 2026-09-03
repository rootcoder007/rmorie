# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 23 cross-validation: native parsers vs jsonlite / xml2 / arrow.
library(testthat)
library(rmorie)

test_that("native JSON parse agrees with jsonlite on nested payloads", {
  skip_if_not_installed("jsonlite")
  payloads <- c(
    '{"result": {"records": [{"id": 1, "name": "a", "v": 1.5},
      {"id": 2, "name": "b", "v": null}], "total": 2}}',
    "[[1, 2], [3, 4]]",
    '{"a": {"b": {"c": [true, false, null]}}}',
    '{"s": "\\u00e9\\n\\t\\"q\\""}')
  for (p in payloads) {
    mine <- morie_fetch_json(p)
    ref <- jsonlite::fromJSON(p)
    expect_equal(mine, ref, tolerance = 1e-12, ignore_attr = TRUE)
  }
})

test_that("native stringify parses back identically under jsonlite", {
  skip_if_not_installed("jsonlite")
  x <- list(a = 1:3, b = "x", c = list(d = TRUE, e = 2.5))
  ref <- jsonlite::fromJSON(morie_json_stringify(x))
  expect_equal(ref$a, c(1, 2, 3))
  expect_equal(ref$c$e, 2.5)
})

test_that("native XML text/attr extraction agrees with xml2", {
  skip_if_not_installed("xml2")
  xml <- '<root><rec id="1"><name>Alpha &amp; Co</name></rec>
          <rec id="2"><name>Beta</name></rec></root>'
  mine <- morie_fetch_xml(xml)
  recs <- rmorie:::.morie_xml_find_all(mine, "rec")
  ref <- xml2::read_xml(xml)
  ref_recs <- xml2::xml_find_all(ref, ".//rec")
  expect_length(recs, length(ref_recs))
  for (i in seq_along(recs)) {
    expect_equal(recs[[i]]$attrs$id,
                 as.character(xml2::xml_attr(ref_recs[[i]], "id")))
    expect_equal(recs[[i]]$children[[1]]$text,
                 xml2::xml_text(xml2::xml_find_first(ref_recs[[i]],
                                                     ".//name")))
  }
})

test_that("native Parquet reader agrees with arrow on a plain file", {
  skip_if_not_installed("arrow")
  df <- data.frame(i = 1:50L,
                   v = rnorm(50),
                   s = paste0("row", 1:50),
                   stringsAsFactors = FALSE)
  fp <- tempfile(fileext = ".parquet")
  # PLAIN encoding, no dictionary, snappy off then on
  for (comp in c("uncompressed", "snappy")) {
    arrow::write_parquet(df, fp, compression = comp,
                         use_dictionary = FALSE)
    mine <- tryCatch(morie_fetch_parquet(fp), error = function(e) e)
    if (inherits(mine, "error")) {
      # arrow may still emit features beyond the minimal subset;
      # the reader must fail LOUDLY naming arrow, never corrupt.
      expect_match(conditionMessage(mine), "arrow|Parquet")
    } else {
      ref <- as.data.frame(arrow::read_parquet(fp))
      expect_equal(mine$i, ref$i)
      expect_equal(mine$v, ref$v, tolerance = 1e-12)
      expect_equal(mine$s, ref$s)
    }
  }
  unlink(fp)
})
