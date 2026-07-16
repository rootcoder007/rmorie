# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 23 — native parsers: JSON, XML, HTML, Parquet, dispatcher.
#' @srrstats {G5.4} Round-trips and hand-built fixtures pin the
#'   parsers; the cross tier compares against jsonlite/xml2/arrow.

test_that("native JSON parses scalars, arrays, objects, escapes", {
  expect_equal(morie_fetch_json("42"), 42)
  expect_equal(morie_fetch_json('"a\\nb"'), "a\nb")
  expect_true(morie_fetch_json("true"))
  expect_null(morie_fetch_json("null"))
  expect_equal(morie_fetch_json("[1, 2, 3]"), c(1, 2, 3))
  expect_equal(morie_fetch_json('["x", "y"]'), c("x", "y"))
  obj <- morie_fetch_json('{"a": 1, "b": [true, false], "c": null}')
  expect_equal(obj$a, 1)
  expect_equal(obj$b, c(TRUE, FALSE))
  expect_true(is.null(obj$c))
  expect_equal(morie_fetch_json('"\\u00e9"'), "é")
  # array of objects -> data frame
  df <- morie_fetch_json('[{"x": 1, "y": "a"}, {"x": 2, "y": "b"}]')
  expect_s3_class(df, "data.frame")
  expect_equal(df$x, c(1, 2))
  expect_equal(df$y, c("a", "b"))
  # errors on malformed input
  expect_error(morie_fetch_json('{"a": }'), "parse error")
  expect_error(morie_fetch_json('[1, 2'), "parse error")
  expect_error(morie_fetch_json('[1] trailing'), "trailing")
})

test_that("native JSON stringify round-trips through the parser", {
  x <- list(name = "otis", n = 3, ok = TRUE,
            values = c(1.5, 2.5), tags = c("a", "b"))
  txt <- morie_json_stringify(x)
  back <- morie_fetch_json(txt)
  expect_equal(back$name, "otis")
  expect_equal(back$values, c(1.5, 2.5))
  df <- data.frame(id = 1:2, y = c("p", "q"))
  back2 <- morie_fetch_json(morie_json_stringify(df))
  expect_equal(back2$id, c(1, 2))
  expect_equal(back2$y, c("p", "q"))
})

test_that("native XML tree: attributes, nesting, entities, CDATA", {
  xml <- '<catalog n="2"><item id="a">One &amp; two</item>
          <item id="b"><![CDATA[<raw>]]></item><empty/></catalog>'
  root <- morie_fetch_xml(xml)
  expect_equal(root$tag, "catalog")
  expect_equal(root$attrs$n, "2")
  expect_length(root$children, 3)
  expect_equal(root$children[[1]]$text, "One & two")
  expect_equal(root$children[[2]]$text, "<raw>")
  items <- rmorie:::.morie_xml_find_all(root, "item")
  expect_length(items, 2)
})

test_that("native HTML tolerates void elements + unclosed tags", {
  html <- "<html><body><p>first<p>second<br><ul><li>a<li>b</ul>
           </body></html>"
  root <- morie_fetch_html(html)
  body <- rmorie:::.morie_xml_find_all(root, "body")[[1]]
  ps <- rmorie:::.morie_xml_find_all(root, "p")
  expect_length(ps, 2)
  expect_equal(ps[[1]]$text, "first")
  lis <- rmorie:::.morie_xml_find_all(root, "li")
  expect_length(lis, 2)
})

test_that("unified dispatcher handles csv + json files", {
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  fc <- tempfile(fileext = ".csv")
  write.csv(d, fc, row.names = FALSE)
  out <- morie_fetch_unified(fc)
  expect_s3_class(out, "morie_dataset")
  expect_equal(out$n_rows, 3L)
  fj <- tempfile(fileext = ".json")
  writeLines('[{"a": 1}, {"a": 2}]', fj)
  outj <- morie_fetch_unified(fj)
  expect_equal(outj$data$a, c(1, 2))
  unlink(c(fc, fj))
})

test_that("native Snappy decompressor round-trips literals + copies", {
  # hand-built snappy stream: len=11 varint, literal "hello ",
  # then copy of "hello" via 1-byte-offset copy (len 5, offset 6)
  lit <- charToRaw("hello ")
  stream <- as.raw(c(0x0b,
                     bitwShiftL(length(lit) - 1L, 2L), as.integer(lit),
                     bitwOr(1L, bitwShiftL(5L - 4L, 2L)), 6L))
  out <- rmorie:::.mpq_snappy(stream)
  expect_equal(rawToChar(out), "hello hello")
})

test_that("json shim honours simplifyVector when falling back", {
  raw_list <- rmorie:::.morie_from_json('[1, 2, 3]',
                                        simplifyVector = FALSE)
  expect_true(is.list(raw_list))
  simp <- rmorie:::.morie_from_json('[1, 2, 3]')
  expect_equal(as.numeric(unlist(simp)), c(1, 2, 3))
})
