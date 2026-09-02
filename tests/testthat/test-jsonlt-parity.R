# SPDX-License-Identifier: AGPL-3.0-or-later
# The native JSON codec must produce jsonlite's bytes, option for option,
# and simplify parsed JSON to the same R objects. jsonlite is Suggests-only;
# these tests are the executable specification whenever it is installed.

test_that("native encoder matches jsonlite::toJSON across the option grid", {
  skip_if_not_installed("jsonlite")
  df <- data.frame(id = 1:3, v = c(1.5, NA, -2), s = c("a", NA, "c\"q\\\n\té"),
                   f = factor(c("x", "y", "x")), b = c(TRUE, NA, FALSE), stringsAsFactors = FALSE)
  df$d <- as.Date(c("2024-01-02", NA, "1999-12-31"))
  df$t <- as.POSIXct(c("2024-01-02 03:04:05", NA, "1999-12-31 23:59:59"), tz = "UTC")
  objs <- list(
    scalar_num = 1, scalar_chr = "x", na_lgl = NA,
    num_vec = c(1, 2.5, NA, NaN, Inf, -Inf, 1e-20, 123456789.123456789, 1e15, 1e16, 0.1 + 0.2),
    int_vec = c(1L, NA, .Machine$integer.max), chr_vec = c("a", NA, "", "é中", "\U0001F600"),
    chr_ctrl = c("tab\t", "nl\n", "cr\r", "bs\\", "q\"", "", "</x>", "\001"),
    lgl_vec = c(TRUE, NA, FALSE), null = NULL, empty_list = list(), empty_named = setNames(list(), character()),
    named = list(a = 1, b = "x", c = list(d = 1:3, e = NULL), f = list()),
    unnamed = list(1, "a", TRUE, NULL, list(2)), df = df,
    mat = matrix(1:6, 2), mat_num = matrix(c(1.5, NA, 3, 4), 2), arr3 = array(1:24, c(2, 3, 4)),
    dates = as.Date(c("2024-05-06", NA)),
    posix = as.POSIXct("2024-05-06 07:08:09", tz = "UTC"),
    posix_local = as.POSIXct("2024-05-06 07:08:09", tz = "America/Toronto"),
    factor = factor(c("b", "a", NA, "b")), complex = c(1 + 2i, NA), raw = charToRaw("hello"),
    nested_df = data.frame(x = 1:2, y = I(list(list(a = 1), list(a = 2)))),
    df_in_list = list(rows = df[1:2, c("id", "v")], k = 3L),
    neg_zero = -0, tiny = 5e-324, big = 1.7976931348623157e308,
    zero_row_df = df[0, ], ts = ts(1:4, start = 2000), int_named = c(a = 1L, b = 2L),
    df_with_rownames = { d <- df[1:2, 1:2]; rownames(d) <- c("r1", "r2"); d }
  )
  opts <- list(
    list(), list(auto_unbox = TRUE), list(pretty = TRUE), list(digits = NA), list(digits = I(3)),
    list(na = "string"), list(na = "null"), list(null = "null"), list(dataframe = "columns"),
    list(dataframe = "values"), list(matrix = "columnmajor"), list(Date = "epoch"),
    list(POSIXt = "epoch"), list(POSIXt = "mongo"), list(factor = "integer"), list(complex = "list"),
    list(raw = "hex"), list(raw = "mongo"), list(force = TRUE), list(rownames = TRUE),
    list(always_decimal = TRUE), list(auto_unbox = TRUE, na = "string", null = "null", pretty = TRUE)
  )
  run <- function(f, x, o) tryCatch(as.character(do.call(f, c(list(x), o))),
                                    error = function(e) "<<error>>")
  for (on in names(objs)) for (o in opts) {
    theirs <- run(jsonlite::toJSON, objs[[on]], o)
    ours <- run(morie_jsonlt_to_json, objs[[on]], o)
    expect_identical(ours, theirs, label = paste0(on, " / ", paste(names(o), unlist(o), collapse = ",")))
  }
  expect_identical(as.character(morie_jsonlt_to_json(list(a = morie_jsonlt_unbox(1), b = 1:2))),
                   as.character(jsonlite::toJSON(list(a = jsonlite::unbox(1), b = 1:2))))
})

test_that("native parser + simplifier match jsonlite::fromJSON", {
  skip_if_not_installed("jsonlite")
  texts <- c(
    '1', '"a"', 'true', 'null', '[]', '{}', '[1,2,3]', '[1,null,3]', '["a",null]', '[true,false,null]',
    '[1,"a"]', '[[1,2],[3,4]]', '[[1,2],[3]]', '[[1,2],[3,null]]', '[{"a":1,"b":"x"},{"a":2,"b":"y"}]',
    '[{"a":1},{"b":2}]', '[{"a":1,"b":{"c":1}},{"a":2,"b":{"c":2}}]', '[{"a":[1,2]},{"a":[3]}]',
    '[{"a":{"b":[1,2]}},{"a":{"b":[]}}]', '{"a":[1,2],"b":{"c":[{"d":1},{"d":2}]}}', '{"a":null,"b":[null]}',
    '[1.5,2,1e3,-0,1E-2,123456789012345678]', '"\\u00e9\\ud83d\\ude00\\n\\t\\"\\\\\\/"', '[[[1,2],[3,4]],[[5,6],[7,8]]]',
    '[[1,"a"],[2,"b"]]', '{"a":{"b":{"c":1}}}', '[{"a":1,"b":null},{"a":null,"b":2}]', '[{"x":[1,2,3]},{"x":[4,5,6]}]',
    '[[true,1],[false,0]]', '[{"a":[{"b":1}]},{"a":[{"b":2}]}]', '{"a":[],"b":{}}', '[[],[]]', '[{},{}]',
    '[[1],[2]]', '[1,2.0]', '[9007199254740993]', '  [ 1 , 2 ]  ', '{"a":1,"a":2}', '[{"a":1},{"a":"x"}]',
    '[{"a":[1,2]},{"a":3}]', '[1,[2,3]]', '{"a":[[1,2],[3,4]]}', '[{"$date":1700000000000},{"$date":1700000001000}]',
    '{"_row":"r1","a":1}', '[{"_row":"r1","a":1},{"_row":"r2","a":2}]', '["NA","NaN","Inf","-Inf"]', '["NA","x"]'
  )
  dopts <- list(list(), list(simplifyVector = FALSE), list(simplifyDataFrame = FALSE), list(simplifyMatrix = FALSE),
                list(flatten = TRUE), list(simplifyVector = FALSE, simplifyDataFrame = TRUE))
  run <- function(f, tx, o) tryCatch(do.call(f, c(list(tx), o)), error = function(e) "<<error>>")
  for (tx in texts) for (o in dopts) {
    expect_equal(run(morie_jsonlt_from_json, tx, o), run(jsonlite::fromJSON, tx, o),
                 label = paste0(tx, " / ", paste(names(o), unlist(o), collapse = ",")))
  }
  for (bad in c("[1,]", "{a:1}", "[1 2]", "\"abc", "[1]x", "", "{\"a\":1,}", "[01]", "[.5]", "nul"))
    expect_false(isTRUE(morie_jsonlt_validate(bad)), label = bad)
  expect_true(morie_jsonlt_validate('{"a":[1,2,{"b":null}]}'))
})

test_that("prettify / minify / base64 / serialize agree with jsonlite", {
  skip_if_not_installed("jsonlite")
  for (tx in c('{"a":[1,2,{"b":null}],"c":"x"}', "[]", "{}", "[1]", "[[]]", '{"a":{}}',
               '[{"a":[1,[2,3]],"b":"\\u00e9\\/<\\/x"}]', '"s"', "1", "null")) {
    expect_identical(as.character(morie_jsonlt_prettify(tx)), as.character(jsonlite::prettify(tx)), label = tx)
    expect_identical(as.character(morie_jsonlt_prettify(tx, 2)), as.character(jsonlite::prettify(tx, 2)), label = tx)
    expect_identical(as.character(morie_jsonlt_minify(jsonlite::prettify(tx))),
                     as.character(jsonlite::minify(jsonlite::prettify(tx))), label = tx)
  }
  b <- as.raw(0:255)
  expect_identical(morie_jsonlt_base64_enc(b), jsonlite::base64_enc(b))
  expect_identical(morie_jsonlt_base64_dec(jsonlite::base64_enc(b)), b)
  expect_identical(morie_jsonlt_base64url_dec(morie_jsonlt_base64url_enc(as.raw(250:255))), as.raw(250:255))
  expect_identical(morie_jsonlt_base64_enc(c("ab", "c")), jsonlite::base64_enc(c("ab", "c")))
  for (x in list(data.frame(a = 1:2, b = c("x", NA)), list(a = 1, b = list(c = "z")), matrix(1:6, 2),
                 factor(c("u", "v", "u")), as.Date("2024-05-06"), as.POSIXct("2024-05-06 07:08:09", tz = "UTC"),
                 c(1 + 2i, NA), charToRaw("hi"), c(1, NA, NaN, Inf), NULL, TRUE, c(a = 1L))) {
    theirs <- as.character(jsonlite::serializeJSON(x))
    expect_identical(as.character(morie_jsonlt_serialize(x)), theirs)
    expect_equal(morie_jsonlt_unserialize(theirs), jsonlite::unserializeJSON(theirs))
    # complex NA round-trips as NA+0i through jsonlite too; compare with its own result
    if (!is.complex(x)) expect_equal(morie_jsonlt_unserialize(morie_jsonlt_serialize(x)), x)
  }
  nested <- jsonlite::fromJSON('[{"a":{"b":1,"c":{"d":2}}},{"a":{"b":3,"c":{"d":4}}}]', flatten = FALSE)
  expect_identical(morie_jsonlt_flatten(nested), jsonlite::flatten(nested))
})

test_that("read/write/stream round-trip through files and connections", {
  x <- data.frame(id = 1:3, name = c("a", "b", NA), stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".json")
  morie_jsonlt_write_json(x, p, pretty = TRUE)
  expect_equal(morie_jsonlt_read_json(p, simplifyVector = TRUE), x)
  expect_type(morie_jsonlt_read_json(p), "list")
  nd <- tempfile(fileext = ".ndjson")
  morie_jsonlt_stream_out(x, file(nd), verbose = FALSE)
  expect_identical(length(readLines(nd)), 3L)
  back <- morie_jsonlt_stream_in(file(nd), verbose = FALSE)
  expect_equal(back$id, 1:3)
  expect_equal(back$name, c("a", "b", NA))
  pages <- list(data.frame(a = 1:2, b = c("x", "y")), NULL, data.frame(a = 3L, c = TRUE))
  rb <- morie_jsonlt_rbind_pages(pages)
  expect_identical(names(rb), c("a", "b", "c"))
  expect_identical(nrow(rb), 3L)
})
