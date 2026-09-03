# SPDX-License-Identifier: AGPL-3.0-or-later
# Codec branches that the jsonlite parity grid does not reach: option paths
# with side effects, error paths of the parser and reformatter, the
# serializer's non-vector storage modes, and streaming verbosity. No
# jsonlite needed; expectations are jsonlite's documented behaviour.

to <- function(x, ...) as.character(morie_jsonlt_to_json(x, ...))

test_that("encoder option paths: keep_vec_names, json_verbatim, force, digits = NULL", {
  expect_message(out <- to(c(a = 1, b = 2), keep_vec_names = TRUE), "keep_vec_names")
  expect_identical(out, '{"a":1,"b":2}')
  expect_message(to(c(a = "x"), keep_vec_names = TRUE), "keep_vec_names")
  expect_message(to(c(a = TRUE), keep_vec_names = TRUE), "keep_vec_names")
  expect_message(out <- to(factor(c(a = "u")), keep_vec_names = TRUE), "keep_vec_names")
  expect_identical(out, '{"a":"u"}')
  j <- morie_jsonlt_to_json(list(k = 1))
  expect_identical(to(list(inner = j), json_verbatim = TRUE), '{"inner":{"k":[1]}}')
  expect_identical(to(list(inner = j)), '{"inner":["{\\"k\\":[1]}"]}')
  expect_identical(to(0.1 + 0.2, digits = NULL), "[0.30000000000000004]")
  expect_identical(to(1e-20, digits = NULL), "[9.9999999999999995e-21]")
  odd <- structure(1:3, class = "odd")
  expect_identical(to(odd, force = TRUE), "[1,2,3]")
  expect_identical(to(structure(list(a = 1), class = c("odd", "list"))), '{"a":[1]}')
  expect_error(to(function(x) x), "No method")
  expect_identical(to(function(x) x, force = TRUE), "{}")
  setClass("rmblTestS4", representation(a = "numeric"), where = environment())
  obj <- new("rmblTestS4", a = 1)
  expect_error(to(obj), "S4")
  expect_match(to(obj, force = TRUE), '"a":\\[1\\]')
  expect_identical(to(setNames(list(1, 2), c("", NA)), auto_unbox = TRUE), '{"1":1,"2":2}')
  expect_identical(to(list(a.b = 1), no_dots = TRUE), '{"a_b":[1]}')
})

test_that("encoder class paths: POSIXlt, hms, blob, mongo dates, matrix in data.frame", {
  lt <- as.POSIXlt("2024-05-06 07:08:09", tz = "UTC")
  expect_identical(to(lt), '["2024-05-06 07:08:09"]')
  expect_identical(to(lt, POSIXt = "mongo"), '[{"$date":1714979289000}]')
  expect_identical(to(as.POSIXct(c("2024-05-06 07:08:09", NA), tz = "UTC"), POSIXt = "mongo"),
                   '[{"$date":1714979289000},null]')
  expect_identical(to(lt, POSIXt = "ISO8601", UTC = TRUE), '["2024-05-06T07:08:09Z"]')
  expect_identical(to(lt, time_format = "%H:%M"), '["07:08"]')
  df <- data.frame(t = as.POSIXlt(c("2024-05-06 07:08:09", "2024-05-07 00:00:00"), tz = "UTC"))
  expect_identical(to(df), '[{"t":"2024-05-06 07:08:09"},{"t":"2024-05-07 00:00:00"}]')
  hms <- structure(c(3661, NA), class = "hms")
  expect_identical(to(hms, hms = "secs"), '[3661,"NA"]')
  blob <- structure(list(charToRaw("hi"), raw(0)), class = "blob")
  expect_identical(to(blob), '["aGk=",""]')
  expect_identical(to(blob, raw = "int"), '[[104,105],[]]')
  dfm <- data.frame(id = 1:2)
  dfm$m <- matrix(1:4, 2)
  expect_identical(to(dfm), '[{"id":1,"m":[1,3]},{"id":2,"m":[2,4]}]')
  expect_identical(to(dfm, matrix = "columnmajor"), '[{"id":1,"m":[1,3]},{"id":2,"m":[2,4]}]')
  dfr <- data.frame(id = 1:2)
  dfr$r <- as.raw(c(1, 255))
  expect_identical(to(dfr), '[{"id":1,"r":"01"},{"id":2,"r":"ff"}]')
  dfc <- data.frame(id = 1:2)
  dfc$z <- c(1 + 2i, 3 - 1i)
  expect_identical(to(dfc, complex = "list"), '[{"id":1,"z":{"real":1,"imaginary":2}},{"id":2,"z":{"real":3,"imaginary":-1}}]')
  dfl <- data.frame(id = 1:2)
  dfl$l <- list(list(a = 1), list(b = 2))
  expect_identical(to(dfl), '[{"id":1,"l":{"a":[1]}},{"id":2,"l":{"b":[2]}}]')
  expect_identical(to(as.pairlist(list(a = 1, b = "x"))), '{"a":[1],"b":["x"]}')
  expect_identical(to(array(1:8, c(2, 2, 2)), matrix = "columnmajor"), "[[[1,2],[3,4]],[[5,6],[7,8]]]")
  expect_identical(to(array(1:4, c(1, 2, 2))), "[[[1,3],[2,4]]]")
  expect_identical(to(structure(1:3, dim = 3L)), "[1,2,3]")
})

test_that("number formatting reproduces modp_dtoa2 rounding and the sprintf fallbacks", {
  expect_identical(to(c(0.125, 0.375, 2.5, 3.5, 0.995, 0.9999, 1.5), digits = 2), "[0.12,0.38,2.5,3.5,1,1,1.5]")
  expect_identical(to(c(0.5, 1.5, 2.5), digits = 0), "[0,2,2]")
  expect_identical(to(c(0.9999999, 123.456789), digits = 6), "[1,123.456789]")
  expect_identical(to(2147483648, digits = 2), "[2147483648]")
  expect_identical(to(c(1e-6, 3e-5), digits = 4), "[1e-06,0]")  # modp_dtoa2: 3e-5 has no digits at 4 decimals
  expect_identical(to(123456.789, digits = I(4)), "[1.235e+05]")
  expect_identical(to(c(1, 2.5), always_decimal = TRUE), "[1.0,2.5]")
  expect_identical(to(NA_real_, na = "string"), '["NA"]')
})

test_that("base64 edge cases", {
  expect_identical(morie_jsonlt_base64_enc(NULL), NA_character_)
  expect_identical(morie_jsonlt_base64_enc(raw(0)), "")
  expect_identical(morie_jsonlt_base64_dec(""), raw(0))
  expect_identical(morie_jsonlt_base64_dec(charToRaw("aGk=")), charToRaw("hi"))
  expect_identical(morie_jsonlt_base64_dec("@@@@"), raw(0))  # non-alphabet bytes are skipped, as jsonlite does
  expect_identical(morie_jsonlt_base64url_enc(as.raw(c(251, 255))), "-_8")
  expect_identical(morie_jsonlt_base64url_dec("-_8"), as.raw(c(251, 255)))
  expect_identical(morie_jsonlt_base64url_dec("aGk"), charToRaw("hi"))
})

test_that("parser rejects what yajl rejects, with a position", {
  bad <- c("-", "1.", "1e", "01", ".5", "[1 2]", "[1,]", "{\"a\" 1}", "{\"a\":1 \"b\":2}", "{1:2}",
           "\"abc", "\"a\\qb\"", "\"a\\u12G4\"", "\"a\\u0000\"", "\"tab\tin\"", "tru", "nul", "[1]x", "", "   ")
  for (b in bad) expect_error(morie_jsonlt_from_json(b, simplifyVector = FALSE), "at character|end of input", label = b)
  expect_warning(v <- morie_jsonlt_from_json("\ufeff[1]"), "byte-order-mark")
  expect_identical(v, 1L)
  expect_identical(morie_jsonlt_from_json("\x1e[1]"), 1L)
  expect_identical(morie_jsonlt_from_json("\"\\ud83d\\ude00\""), "\U0001F600")
  expect_identical(morie_jsonlt_from_json("\"\\ud83d x\""), paste0(intToUtf8(0xD83D), " x"))
  expect_identical(morie_jsonlt_from_json("[1,2]", simplify = FALSE), list(1L, 2L))
  expect_identical(morie_jsonlt_from_json("[-9007199254740993]", bigint_as_char = TRUE), "-9007199254740993")
  expect_identical(morie_jsonlt_from_json("[123456789012]"), 123456789012)
  expect_identical(morie_jsonlt_from_json("[{\"$date\":\"2024-05-06T07:08:09Z\"}]"),
                   as.POSIXct("2024-05-06 07:08:09", tz = "UTC"))
  d2 <- morie_jsonlt_from_json('[{"$date":"2024-05-06T07:08:09"},{"$date":"2024-05-07T00:00:00"}]')
  expect_s3_class(d2, "POSIXct")
  expect_identical(format(d2, "%Y-%m-%d %H:%M:%S"), c("2024-05-06 07:08:09", "2024-05-07 00:00:00"))  # local-time strings, tz-agnostic
  # a scalar list of dates collapses to a POSIXct vector (jsonlite's mongo-date rule)
  expect_identical(morie_jsonlt_from_json('{"a":{"$date":1714979289000}}'), structure(1714979289, class = c("POSIXct", "POSIXt")))
  expect_identical(morie_jsonlt_from_json('[{"$date":1714979289000},null]'), structure(c(1714979289, NA), class = c("POSIXct", "POSIXt")))
})

test_that("simplifier edge cases: _row handling, ragged records, empty-record lists", {
  expect_warning(df <- morie_jsonlt_from_json('[{"_row":"a","x":1},{"_row":"a","x":2}]'), "Duplicate names")
  expect_identical(row.names(df), c("a", "a.1"))
  expect_warning(df2 <- morie_jsonlt_from_json('[{"_row":1,"x":1},{"_row":1,"x":2}]'), "Duplicate names")
  expect_identical(row.names(df2), c("1", "2"))
  df3 <- morie_jsonlt_from_json('[{"_row":2.0,"x":1},{"_row":null,"x":2}]')
  expect_identical(row.names(df3), c("2", "NA_1"))
  expect_identical(morie_jsonlt_from_json("[[1,2],[]]"), list(1:2, integer(0)))
  expect_identical(morie_jsonlt_from_json("[{\"a\":1},[]]"), list(list(a = 1L), list()))
  expect_identical(morie_jsonlt_from_json("[[1,2],[[1,2],[3,4]]]", simplifyMatrix = FALSE), list(1:2, list(1:2, 3:4)))
  expect_identical(dim(morie_jsonlt_from_json("[[[1,2]],[[3,4]]]")), c(2L, 1L, 2L))
  expect_identical(morie_jsonlt_from_json('[{"a":1},{"a":"NA"}]')$a, c(1L, NA))
  expect_identical(morie_jsonlt_from_json('[{"a":1},{"a":[1,2]}]')$a, list(1L, 1:2))
})

test_that("prettify/minify report yajl-style errors and handle escapes", {
  for (b in c("[1,", "{\"a\":}", "{\"a\" \"b\"}", "[1]]", "{,}", "[\"x", "\"\\u12G4\"", "{\"a\":1,}", "[1 2]", "@")) {
    expect_error(morie_jsonlt_prettify(b), "at character", label = b)
    expect_error(morie_jsonlt_minify(b), "at character", label = b)
  }
  expect_identical(as.character(morie_jsonlt_minify('["\\u00e9\\u0001\\/<\\/x"]')), '["\u00e9\\u0001/<\\/x"]')
  expect_identical(as.character(morie_jsonlt_minify("\ufeff{\"a\":true,\"b\":false,\"c\":null,\"d\":-1.5e3}")),
                   '{"a":true,"b":false,"c":null,"d":-1.5e3}')
  expect_identical(as.character(morie_jsonlt_prettify('{"a":[]}', indent = 2)), "{\n  \"a\": [\n\n  ]\n}\n")
  v <- morie_jsonlt_validate("\ufeff[1]")
  expect_false(isTRUE(v))
  expect_match(attr(v, "err"), "byte-order-mark")
})

test_that("serializer covers every storage mode digest of an R object needs", {
  rt <- function(x) morie_jsonlt_unserialize(morie_jsonlt_serialize(x))
  f <- function(a, b = 2) a + b
  g <- rt(f)
  expect_identical(g(1), 3)
  expect_identical(rt(as.pairlist(list(a = 1))), as.pairlist(list(a = 1)))
  expect_identical(rt(quote(x + 1)), quote(x + 1))
  expect_identical(rt(as.name("sym")), as.name("sym"))
  expect_identical(rt(expression(a * 2)), expression(a * 2))
  expect_identical(rt(sum), sum)
  expect_true(is.environment(rt(new.env())))
  expect_identical(rt(structure(1:3, dim = 3L, myattr = "k")), structure(1:3, dim = 3L, myattr = "k"))
  setClass("rmblSerS4", representation(a = "numeric", b = "character"), where = environment())
  s4 <- new("rmblSerS4", a = 1, b = "x")
  s4j <- morie_jsonlt_serialize(s4)
  expect_match(as.character(s4j), '"type":"S4"')
  expect_warning(morie_jsonlt_serialize(quote(`{`)), NA)
  expect_identical(as.character(morie_jsonlt_serialize(NULL)), '{"type":"NULL"}')
  expect_identical(as.character(morie_jsonlt_serialize(list(1, "a"), pretty = TRUE)),
                   "{\n  \"type\": \"list\",\n  \"attributes\": {},\n  \"value\": [\n    {\n      \"type\": \"double\",\n      \"attributes\": {},\n      \"value\": [1]\n    },\n    {\n      \"type\": \"character\",\n      \"attributes\": {},\n      \"value\": [\"a\"]\n    }\n  ]\n}")
  expect_error(morie_jsonlt_unserialize('{"type":"weird","attributes":{},"value":[]}'), "encode.mode")
})

test_that("unbox and streaming edge cases", {
  expect_null(morie_jsonlt_unbox(NULL))
  expect_s3_class(morie_jsonlt_unbox(data.frame(a = 1)), "scalar")
  expect_error(morie_jsonlt_unbox(data.frame(a = 1:2)), "2 rows")
  expect_s3_class(morie_jsonlt_unbox(as.POSIXct("2024-05-06", tz = "UTC")), "scalar")
  expect_identical(to(list(t = morie_jsonlt_unbox(as.POSIXct("2024-05-06 07:08:09", tz = "UTC")))), '{"t":"2024-05-06 07:08:09"}')
  expect_identical(to(list(d = morie_jsonlt_unbox(data.frame(a = 1, b = "x")))), '{"d":{"a":1,"b":"x"}}')
  expect_error(morie_jsonlt_stream_in("not a connection"), "connection")
  expect_error(morie_jsonlt_stream_out(data.frame(a = 1), "not a connection"), "connection")
  x <- data.frame(a = 1:3)
  nd <- tempfile()
  expect_message(morie_jsonlt_stream_out(x, file(nd), pagesize = 2, verbose = TRUE), "output connection")
  expect_message(back <- morie_jsonlt_stream_in(file(nd), pagesize = 2, verbose = TRUE), "input connection")
  expect_identical(back$a, 1:3)
  seen <- 0L
  expect_message(morie_jsonlt_stream_in(file(nd), handler = function(d) seen <<- seen + nrow(d), verbose = TRUE), "custom handler")
  expect_identical(seen, 3L)
  con <- textConnection(c('{"a":1}', '{"a":2}'))
  expect_identical(morie_jsonlt_stream_in(con, verbose = FALSE, simplifyDataFrame = FALSE), list(list(a = 1L), list(a = 2L)))
})
