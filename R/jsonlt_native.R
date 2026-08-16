# jsonlt -- jsonlite's JSON mapping, implemented natively.
#
# aaa_helpers_s03.R covers the four functions the package actually called.
# This is the rest of jsonlite's documented surface, and it exists in both
# languages so the mapping can be checked by running it rather than by
# asserting it.
#
# The hard part is not brackets and commas. jsonlite is a MAPPING between
# R's data model and JSON's, and R's model does not fit: there are no
# scalars, so every length-one value is ambiguous; a data.frame is a list
# of columns that people want written as an array of rows; NA is neither
# null nor a number; a factor is an integer vector wearing a string coat.
# Each of those has a documented, switchable rule here.
#
# Reference
#   Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
#     Mapping Between JSON Data and R Objects." arXiv:1403.2805.
#   Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
#     Interchange Format." RFC 8259.

# ---------------------------------------------------------------- options

#' .jsonlt_opts
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser_opts}, \code{morie_jsonlt_to_json}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param dataframe Carried through into a list the body builds. Defaults to \code{"rows"}.
#' @param matrix Carried through into a list the body builds. Defaults to \code{"rowmajor"}.
#' @param Date Carried through into a list the body builds. Defaults to \code{"ISO8601"}.
#' @param POSIXt Carried through into a list the body builds. Defaults to \code{"string"}.
#' @param factor Carried through into a list the body builds. Defaults to \code{"string"}.
#' @param complex Carried through into a list the body builds. Defaults to \code{"string"}.
#' @param raw Carried through into a list the body builds. Defaults to \code{"base64"}.
#' @param null Carried through into a list the body builds. Defaults to \code{"list"}.
#' @param na Carried through into a list the body builds.
#' @param auto_unbox A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param digits Carried through into a list the body builds. Defaults to \code{4}.
#' @param force A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{o}, as built in the body.
#' @export
.jsonlt_opts <- function(dataframe = "rows", matrix = "rowmajor",
                         Date = "ISO8601", POSIXt = "string",
                         factor = "string", complex = "string",
                         raw = "base64", null = "list", na = NULL,
                         auto_unbox = FALSE, digits = 4, force = FALSE) {
  o <- list(dataframe = dataframe, matrix = matrix, Date = Date,
            POSIXt = POSIXt, factor = factor, complex = complex,
            raw = raw, null = null, na = na, auto_unbox = isTRUE(auto_unbox),
            digits = digits, force = isTRUE(force))
  chk <- function(nm, allowed) {
    v <- o[[nm]]
    if (is.null(v)) return(invisible(NULL))
    if (!(length(v) == 1L && v %in% allowed))
      stop(sprintf("jsonlt: %s = %s; expected one of %s", nm,
                   paste(v, collapse = ","),
                   paste(allowed, collapse = ", ")), call. = FALSE)
    invisible(NULL)
  }
  chk("dataframe", c("rows", "columns", "values"))
  chk("matrix", c("rowmajor", "columnmajor"))
  chk("Date", c("ISO8601", "epoch"))
  chk("POSIXt", c("string", "ISO8601", "epoch", "mongo"))
  chk("factor", c("string", "integer"))
  chk("complex", c("string", "list"))
  chk("raw", c("base64", "hex", "int", "mongo"))
  chk("null", c("list", "null"))
  chk("na", c("null", "string"))
  o
}

# I(n) as a digits argument means n SIGNIFICANT digits, matching
# jsonlite's digits = I(n). Anything else is decimal places.
#' I(n) as a digits argument means n SIGNIFICANT digits, matching
#'
#' jsonlite\'s digits = I(n). Anything else is decimal places.
#'
#' @param digits See Usage.
#' @return The value of \code{inherits}.
#' @export
.jsonlt_sig <- function(digits) inherits(digits, "AsIs")

# ---------------------------------------------------------------- numbers

# The one place a double becomes text. Both arms go through C's printf
# with the same format string, so the bytes match; each language's own
# float-to-string would put them one ulp apart and call it a failure.
#' The one place a double becomes text. Both arms go through C\'s printf
#'
#' with the same format string, so the bytes match; each language\'s own
#' float-to-string would put them one ulp apart and call it a failure.
#'
#' @param x See Usage.
#' @param digits Optional; may be \code{NULL}. Passed to \code{.jsonlt_sig}.
#' @return The value of \code{.jsonlt_tidy}.
#' @export
.jsonlt_num <- function(x, digits) {
  if (is.nan(x)) return("\"NaN\"")
  if (is.infinite(x)) return(if (x > 0) "\"Inf\"" else "\"-Inf\"")
  s <- if (is.null(digits)) sprintf("%.17g", x)
       else if (.jsonlt_sig(digits)) sprintf("%.*g", max(1L, as.integer(digits)), x)
       else sprintf("%.*f", max(0L, as.integer(digits)), x)
  .jsonlt_tidy(s)
}

#' .jsonlt_tidy
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_num}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Compared against \code{"-0"}.
#' @return The value of \code{s}, as built in the body.
#' @export
.jsonlt_tidy <- function(s) {
  if (grepl("[eE]", s)) {
    parts <- strsplit(s, "[eE]")[[1]]
    mant <- parts[1]
    expo <- parts[2]
    if (grepl(".", mant, fixed = TRUE))
      mant <- sub("\\.$", "", sub("0+$", "", mant))
    sign <- ""
    if (substr(expo, 1L, 1L) %in% c("+", "-")) {
      if (substr(expo, 1L, 1L) == "-") sign <- "-"
      expo <- substring(expo, 2L)
    }
    expo <- sub("^0+", "", expo)
    if (!nzchar(expo)) expo <- "0"
    return(paste0(mant, "e", sign, expo))
  }
  if (grepl(".", s, fixed = TRUE))
    s <- sub("\\.$", "", sub("0+$", "", s))
  if (s == "-0" || !nzchar(s)) s <- "0"
  s
}

# ---------------------------------------------------------------- strings

#' .jsonlt_esc
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_df_columns}, \code{.jsonlt_df_rows}, \code{.jsonlt_encode} and 5 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{grepl}.
#' @return A character value.
#' @export
.jsonlt_esc <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\b", "\\b", s, fixed = TRUE)
  s <- gsub("\f", "\\f", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  # Control characters must be escaped, not passed through. The table
  # starts at 1: R strings cannot hold a NUL, so there is nothing at 0 to
  # match and gsub rejects the zero-length pattern it would be given.
  for (cc in c(1:7, 11L, 14:31)) {
    ch <- rawToChar(as.raw(cc))
    if (grepl(ch, s, fixed = TRUE))
      s <- gsub(ch, sprintf("\\u%04x", cc), s, fixed = TRUE)
  }
  paste0("\"", s, "\"")
}

.JSONLT_B64 <- strsplit(paste0("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                               "abcdefghijklmnopqrstuvwxyz",
                               "0123456789+/"), "")[[1]]

# Base64 without a library, so both arms produce the same string.
#' Base64 without a library, so both arms produce the same string
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_raw_enc}, \code{.jsonlt_ser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param data Coerced to integer by the body, with \code{as.integer}.
#' @return A character value.
#' @export
.jsonlt_b64 <- function(data) {
  b <- as.integer(data)
  n <- length(b)
  out <- character(0)
  i <- 1L
  while (i + 2L <= n) {
    v <- b[i] * 65536L + b[i + 1L] * 256L + b[i + 2L]
    out <- c(out, paste0(.JSONLT_B64[bitwAnd(v %/% 262144L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v %/% 4096L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v %/% 64L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v, 63L) + 1L]))
    i <- i + 3L
  }
  rem <- n - i + 1L
  if (rem == 1L) {
    v <- b[i] * 65536L
    out <- c(out, paste0(.JSONLT_B64[bitwAnd(v %/% 262144L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v %/% 4096L, 63L) + 1L], "=="))
  } else if (rem == 2L) {
    v <- b[i] * 65536L + b[i + 1L] * 256L
    out <- c(out, paste0(.JSONLT_B64[bitwAnd(v %/% 262144L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v %/% 4096L, 63L) + 1L],
                         .JSONLT_B64[bitwAnd(v %/% 64L, 63L) + 1L], "="))
  }
  paste(out, collapse = "")
}

#' .jsonlt_unb64
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_unser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{gsub}.
#' @return The value of \code{as.raw}.
#' @export
.jsonlt_unb64 <- function(s) {
  ch <- strsplit(gsub("[[:space:]=]", "", s), "")[[1]]
  acc <- 0
  bits <- 0L
  out <- integer(0)
  for (c0 in ch) {
    acc <- acc * 64 + (match(c0, .JSONLT_B64) - 1L)
    bits <- bits + 6L
    if (bits >= 8L) {
      bits <- bits - 8L
      out <- c(out, bitwAnd(as.integer(acc %/% (2^bits)), 255L))
      acc <- acc %% (2^bits)
    }
  }
  as.raw(out)
}

# as.data.frame() flattens a nested data.frame column into outer.inner
# columns. That is morie_jsonlt_flatten's job, opt-in, and doing it here
# would make a nested structure impossible to represent at all.
#' As.data.frame() flattens a nested data.frame column into outer.inner
#'
#' columns. That is morie_jsonlt_flatten\'s job, opt-in, and doing it
#' here would make a nested structure impossible to represent at all.
#'
#' @param cols A vector; its length is taken and its elements indexed.
#' @param nms See Usage.
#' @return The value of \code{structure}.
#' @export
.jsonlt_df <- function(cols, nms) {
  names(cols) <- nms
  n <- if (length(cols)) NROW(cols[[1]]) else 0L
  structure(cols, class = "data.frame", row.names = seq_len(n))
}

# ---------------------------------------------------------------- encoder

#' .jsonlt_na_token
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_atomic}, \code{.jsonlt_cell}, \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param o A list; the body reads \code{$na} from it.
#' @return One of two values, depending on the branch taken.
#' @export
.jsonlt_na_token <- function(o) if (identical(o$na, "string")) "\"NA\"" else "null"

.JSONLT_EPOCH <- as.Date("1970-01-01")

#' .jsonlt_posix
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_scalar}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Coerced to numeric by the body, with \code{as.numeric}.
#' @param o A list; the body reads \code{$digits}, \code{$POSIXt} from it.
#' @return The value of \code{.jsonlt_esc}.
#' @export
.jsonlt_posix <- function(v, o) {
  ms <- as.numeric(v) * 1000
  if (identical(o$POSIXt, "epoch")) return(.jsonlt_num(ms, o$digits))
  if (identical(o$POSIXt, "mongo"))
    return(paste0("{\"$date\":", .jsonlt_num(ms, o$digits), "}"))
  fmt <- if (identical(o$POSIXt, "ISO8601")) "%Y-%m-%dT%H:%M:%SZ"
         else "%Y-%m-%d %H:%M:%S"
  .jsonlt_esc(format(v, fmt, tz = "UTC"))
}

# One element of an atomic vector, already known to be non-NA.
#' One element of an atomic vector, already known to be non-NA
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_atomic}, \code{.jsonlt_cell}, \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A flag; the body branches on it.
#' @param o A list; the body reads \code{$Date}, \code{$digits} from it.
#' @return The value of \code{.jsonlt_esc}.
#' @export
.jsonlt_scalar <- function(v, o) {
  if (inherits(v, "POSIXt")) return(.jsonlt_posix(v, o))
  if (inherits(v, "Date")) {
    if (identical(o$Date, "epoch"))
      return(.jsonlt_num(as.numeric(v), o$digits))
    return(.jsonlt_esc(format(v, "%Y-%m-%d")))
  }
  if (is.logical(v)) return(if (v) "true" else "false")
  if (is.complex(v)) {
    re <- .jsonlt_num(Re(v), o$digits)
    im <- .jsonlt_num(abs(Im(v)), o$digits)
    return(.jsonlt_esc(paste0(re, if (Im(v) < 0) "-" else "+", im, "i")))
  }
  if (is.integer(v)) return(sprintf("%d", v))
  if (is.numeric(v)) return(.jsonlt_num(as.numeric(v), o$digits))
  .jsonlt_esc(as.character(v))
}

#' .jsonlt_atomic
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_encode}, \code{.jsonlt_ser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param o A list; the body reads \code{$auto_unbox}, \code{$complex}, \code{$digits} from it.
#' @param unbox_ok See Usage.
#' @return A character value.
#' @export
.jsonlt_atomic <- function(x, o, unbox_ok) {
  if (is.complex(x) && identical(o$complex, "list"))
    return(paste0("{\"r\":", .jsonlt_atomic(Re(x), o, FALSE),
                  ",\"i\":", .jsonlt_atomic(Im(x), o, FALSE), "}"))
  n <- length(x)
  parts <- character(n)
  for (i in seq_len(n)) {
    # NaN is a number, not a missing value. R's is.na() answers TRUE for
    # it, so this has to ask the narrower question first or every NaN
    # leaves as null.
    parts[i] <- if (is.numeric(x[i]) && is.nan(x[i]))
                  .jsonlt_num(as.numeric(x[i]), o$digits)
                else if (is.na(x[i])) .jsonlt_na_token(o)
                else .jsonlt_scalar(x[i], o)
  }
  if (n == 1L && unbox_ok && o$auto_unbox) return(parts[1])
  paste0("[", paste(parts, collapse = ","), "]")
}

#' .jsonlt_raw_enc
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rv Passed to \code{.jsonlt_b64}.
#' @param o A list; the body reads \code{$raw} from it.
#' @return The value of \code{.jsonlt_esc}.
#' @export
.jsonlt_raw_enc <- function(rv, o) {
  if (identical(o$raw, "int"))
    return(paste0("[", paste(sprintf("%d", as.integer(rv)), collapse = ","), "]"))
  if (identical(o$raw, "hex"))
    return(.jsonlt_esc(paste(sprintf("%02x", as.integer(rv)), collapse = "")))
  if (identical(o$raw, "mongo"))
    return(paste0("{\"$binary\":", .jsonlt_esc(.jsonlt_b64(rv)),
                  ",\"$type\":\"00\"}"))
  .jsonlt_esc(.jsonlt_b64(rv))
}

# One data.frame cell. NULL means "leave this field out", which is what
# jsonlite does for NA inside rows when na is unset.
#' One data.frame cell. NULL means "leave this field out", which is what
#'
#' jsonlite does for NA inside rows when na is unset.
#'
#' @param col A matrix; indexed by row and column.
#' @param i See Usage.
#' @param o A list; the body reads \code{$factor}, \code{$na} from it.
#' @return The value of \code{.jsonlt_scalar}.
#' @export
.jsonlt_cell <- function(col, i, o) {
  if (is.data.frame(col)) return(.jsonlt_df_rows(col[i, , drop = FALSE], o))
  if (is.matrix(col)) return(.jsonlt_atomic(as.vector(col[i, ]), o, FALSE))
  if (is.factor(col)) {
    v <- if (identical(o$factor, "integer")) as.integer(col)[i]
         else as.character(col)[i]
    if (is.na(v) && is.null(o$na)) return(NULL)
    return(if (is.na(v)) .jsonlt_na_token(o) else .jsonlt_scalar(v, o))
  }
  if (is.raw(col)) return(.jsonlt_raw_enc(col[i], o))
  if (is.list(col)) return(.jsonlt_encode(col[[i]], o, TRUE))
  v <- col[i]
  if (is.na(v) && is.null(o$na)) return(NULL)
  if (is.na(v)) return(.jsonlt_na_token(o))
  .jsonlt_scalar(v, o)
}

#' .jsonlt_df_rows
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param df A matrix; passed to \code{nrow}.
#' @param o Passed to \code{.jsonlt_cell}.
#' @return A character value.
#' @export
.jsonlt_df_rows <- function(df, o) {
  nm <- names(df)
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    parts <- character(0)
    for (j in seq_along(nm)) {
      cell <- .jsonlt_cell(df[[j]], i, o)
      if (is.null(cell)) next
      parts <- c(parts, paste0(.jsonlt_esc(nm[j]), ":", cell))
    }
    rows[i] <- paste0("{", paste(parts, collapse = ","), "}")
  }
  paste0("[", paste(rows, collapse = ","), "]")
}

#' .jsonlt_df_columns
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param df A vector; indexed elementwise.
#' @param o Passed to \code{.jsonlt_encode}.
#' @return A character value.
#' @export
.jsonlt_df_columns <- function(df, o) {
  nm <- names(df)
  parts <- vapply(seq_along(nm), function(j)
    paste0(.jsonlt_esc(nm[j]), ":", .jsonlt_encode(df[[j]], o, FALSE)),
    character(1))
  paste0("{", paste(parts, collapse = ","), "}")
}

#' .jsonlt_df_values
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param df A matrix; passed to \code{nrow}.
#' @param o Passed to \code{.jsonlt_cell}.
#' @return A character value.
#' @export
.jsonlt_df_values <- function(df, o) {
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    parts <- vapply(seq_along(df), function(j) {
      cell <- .jsonlt_cell(df[[j]], i, o)
      if (is.null(cell)) "null" else cell
    }, character(1))
    rows[i] <- paste0("[", paste(parts, collapse = ","), "]")
  }
  paste0("[", paste(rows, collapse = ","), "]")
}

#' .jsonlt_encode
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_df_columns}, \code{morie_jsonlt_to_json}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @param o A list; the body reads \code{$dataframe}, \code{$factor}, \code{$force}, \code{$matrix}, \code{$null} from it.
#' @param unbox_ok Passed to \code{.jsonlt_atomic}.
#' @return Nothing; this branch always raises.
#' @export
.jsonlt_encode <- function(x, o, unbox_ok) {
  if (is.null(x)) return(if (identical(o$null, "list")) "{}" else "null")
  if (inherits(x, "jsonlt_scalar")) {
    v <- unclass(x)
    return(if (is.na(v)) .jsonlt_na_token(o) else .jsonlt_scalar(v, o))
  }
  if (inherits(x, "AsIs") && !is.data.frame(x)) {
    y <- x
    class(y) <- setdiff(class(y), "AsIs")
    if (is.atomic(y)) return(.jsonlt_atomic(y, o, FALSE))
    return(.jsonlt_encode(y, o, FALSE))
  }
  if (is.data.frame(x)) {
    if (identical(o$dataframe, "columns")) return(.jsonlt_df_columns(x, o))
    if (identical(o$dataframe, "values")) return(.jsonlt_df_values(x, o))
    return(.jsonlt_df_rows(x, o))
  }
  if (is.matrix(x)) {
    if (identical(o$matrix, "columnmajor"))
      return(paste0("[", paste(vapply(seq_len(ncol(x)), function(j)
        .jsonlt_atomic(as.vector(x[, j]), o, FALSE), character(1)),
        collapse = ","), "]"))
    return(paste0("[", paste(vapply(seq_len(nrow(x)), function(i)
      .jsonlt_atomic(as.vector(x[i, ]), o, FALSE), character(1)),
      collapse = ","), "]"))
  }
  if (is.factor(x)) {
    if (identical(o$factor, "integer"))
      return(.jsonlt_atomic(as.integer(x), o, unbox_ok))
    return(.jsonlt_atomic(as.character(x), o, unbox_ok))
  }
  if (is.raw(x)) return(.jsonlt_raw_enc(x, o))
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      parts <- vapply(seq_along(x), function(i)
        paste0(.jsonlt_esc(nm[i]), ":", .jsonlt_encode(x[[i]], o, TRUE)),
        character(1))
      return(paste0("{", paste(parts, collapse = ","), "}"))
    }
    if (!length(x)) return("[]")
    parts <- vapply(seq_along(x), function(i)
      .jsonlt_encode(x[[i]], o, TRUE), character(1))
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  if (is.atomic(x)) return(.jsonlt_atomic(x, o, unbox_ok))
  if (o$force) return(.jsonlt_esc(paste(format(x), collapse = " ")))
  stop(sprintf("jsonlt: no JSON mapping for class %s; pass force = TRUE to ",
               paste(class(x), collapse = "/")),
       "write it as a string", call. = FALSE)
}

#' Serialise an R object to JSON
#'
#' @param x object to serialise.
#' @param pretty FALSE, TRUE, or an indent width.
#' @param ... jsonlite's toJSON options: dataframe, matrix, Date, POSIXt,
#'   factor, complex, raw, null, na, auto_unbox, digits, force.
#' @return a length-one character vector of JSON.
#' @export
morie_jsonlt_to_json <- function(x, pretty = FALSE, ...) {
  o <- .jsonlt_opts(...)
  s <- .jsonlt_encode(x, o, TRUE)
  if (isTRUE(pretty)) return(morie_jsonlt_prettify(s, 4L))
  if (is.numeric(pretty)) return(morie_jsonlt_prettify(s, as.integer(pretty)))
  s
}

#' Mark a value as a JSON scalar
#'
#' @param x a length-one value.
#' @return x, tagged so it is never written as an array.
#' @export
morie_jsonlt_unbox <- function(x) {
  if (length(x) != 1L)
    stop(sprintf("jsonlt: unbox() needs length 1, got %d", length(x)),
         call. = FALSE)
  structure(x, class = c("jsonlt_scalar", class(x)))
}

# ---------------------------------------------------------------- parser

#' .jsonlt_parse
#'
#' A step of the jsonlt_native implementation. Called by \code{morie_jsonlt_from_json}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param txt See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
.jsonlt_parse <- function(txt) {
  s <- paste(txt, collapse = "\n")
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  i <- 1L

  bad <- function(msg)
    stop(sprintf("jsonlt: %s at character %d", msg, i), call. = FALSE)
  ws <- function() {
    while (i <= n && (ch[i] == " " || ch[i] == "\t" || ch[i] == "\n" ||
                      ch[i] == "\r")) i <<- i + 1L
  }

  str_ <- function() {
    if (i > n || ch[i] != "\"") bad("expected a string")
    i <<- i + 1L
    out <- character(0)
    while (i <= n && ch[i] != "\"") {
      if (ch[i] == "\\") {
        i <<- i + 1L
        e <- ch[i]
        out <- c(out,
                 if (e == "n") "\n" else if (e == "t") "\t"
                 else if (e == "r") "\r" else if (e == "b") "\b"
                 else if (e == "f") "\f"
                 else if (e == "u") {
                   cp <- strtoi(paste(ch[(i + 1L):(i + 4L)], collapse = ""), 16L)
                   i <<- i + 4L
                   if (cp >= 0xD800 && cp <= 0xDBFF &&
                       i + 2L <= n && ch[i + 1L] == "\\" && ch[i + 2L] == "u") {
                     lo <- strtoi(paste(ch[(i + 3L):(i + 6L)], collapse = ""), 16L)
                     i <<- i + 6L
                     cp <- 0x10000 + bitwShiftL(cp - 0xD800, 10L) + (lo - 0xDC00)
                   }
                   intToUtf8(cp)
                 } else e)
      } else out <- c(out, ch[i])
      i <<- i + 1L
    }
    i <<- i + 1L
    paste(out, collapse = "")
  }

  num_ <- function() {
    st <- i
    if (i <= n && ch[i] == "-") i <<- i + 1L
    while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    isint <- TRUE
    if (i <= n && ch[i] == ".") {
      isint <- FALSE
      i <<- i + 1L
      while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    }
    if (i <= n && (ch[i] == "e" || ch[i] == "E")) {
      isint <- FALSE
      i <<- i + 1L
      if (i <= n && (ch[i] == "+" || ch[i] == "-")) i <<- i + 1L
      while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    }
    txt0 <- paste(ch[st:(i - 1L)], collapse = "")
    if (!nzchar(txt0)) bad("not a number")
    v <- as.numeric(txt0)
    if (isint && abs(v) <= 2147483647) as.integer(v) else v
  }

  value <- function() {
    ws()
    if (i > n) bad("unexpected end of input")
    c0 <- ch[i]
    if (c0 == "{") {
      i <<- i + 1L
      out <- list()
      nms <- character(0)
      ws()
      if (i <= n && ch[i] == "}") {
        i <<- i + 1L
        return(structure(list(), names = character(0)))
      }
      repeat {
        ws()
        k <- str_()
        ws()
        if (i > n || ch[i] != ":") bad("expected ':'")
        i <<- i + 1L
        out[[length(out) + 1L]] <- list(value())
        nms <- c(nms, k)
        ws()
        if (i <= n && ch[i] == ",") { i <<- i + 1L; next }
        if (i <= n && ch[i] == "}") { i <<- i + 1L; break }
        bad("expected ',' or '}'")
      }
      out <- lapply(out, function(w) w[[1]])
      names(out) <- nms
      return(out)
    }
    if (c0 == "[") {
      i <<- i + 1L
      out <- list()
      ws()
      if (i <= n && ch[i] == "]") { i <<- i + 1L; return(list()) }
      repeat {
        out[[length(out) + 1L]] <- list(value())
        ws()
        if (i <= n && ch[i] == ",") { i <<- i + 1L; next }
        if (i <= n && ch[i] == "]") { i <<- i + 1L; break }
        bad("expected ',' or ']'")
      }
      return(lapply(out, function(w) w[[1]]))
    }
    if (c0 == "\"") return(str_())
    if (substr(s, i, i + 3L) == "true") { i <<- i + 4L; return(TRUE) }
    if (substr(s, i, i + 4L) == "false") { i <<- i + 5L; return(FALSE) }
    if (substr(s, i, i + 3L) == "null") { i <<- i + 4L; return(NULL) }
    num_()
  }

  v <- value()
  ws()
  if (i <= n) bad("trailing content")
  v
}

#' .jsonlt_is_scalar
#'
#' A step of the jsonlt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A vector; its length is taken.
#' @return A logical value.
#' @export
.jsonlt_is_scalar <- function(x)
  is.null(x) || (is.atomic(x) && length(x) == 1L && !is.list(x))

#' .jsonlt_simplify
#'
#' A step of the jsonlt_native implementation. Called by \code{morie_jsonlt_from_json}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken.
#' @param sv A flag; the body branches on it.
#' @param sdf A flag; the body branches on it.
#' @param sm A flag; the body branches on it.
#' @param flat A flag; the body branches on it.
#' @return The value of \code{kids}, as built in the body.
#' @export
.jsonlt_simplify <- function(x, sv, sdf, sm, flat) {
  if (!is.list(x)) return(x)
  if (!is.null(names(x)))
    return(lapply(x, .jsonlt_simplify, sv, sdf, sm, flat))
  if (!length(x)) return(x)
  kids <- lapply(x, .jsonlt_simplify, sv, sdf, sm, flat)
  if (sdf && all(vapply(kids, function(k)
      is.list(k) && !is.data.frame(k) && !is.null(names(k)), logical(1)))) {
    nms <- character(0)
    for (k in kids) nms <- union(nms, names(k))
    cols <- list()
    for (nm in nms) {
      col <- lapply(kids, function(k) if (is.null(k[[nm]])) NA else k[[nm]])
      cols[[nm]] <- if (all(vapply(col, .jsonlt_is_scalar, logical(1))))
        unlist(lapply(col, function(v) if (is.null(v)) NA else v),
               use.names = FALSE) else col
    }
    df <- .jsonlt_df(cols, nms)
    return(if (flat) morie_jsonlt_flatten(df) else df)
  }
  # A JSON scalar and a one-element JSON array both land as a length-one
  # atomic vector, so "every child is an equal-length vector" is true of
  # [1,2,3] as well as [[1],[2],[3]] -- and only the second is a matrix.
  # The distinction is in the source text, so keep it: was_arr records
  # whether the child arrived as an array.
  was_arr <- vapply(x, is.list, logical(1))
  if (sm && all(was_arr) && all(vapply(kids, function(k)
      is.atomic(k) && !is.list(k), logical(1))) &&
      length(unique(vapply(kids, length, integer(1)))) == 1L &&
      length(kids[[1]]) > 0L) {
    return(do.call(rbind, kids))
  }
  # Same distinction on the vector side: [1,[2]] must not collapse to a
  # two-element vector just because [2] happens to hold one value.
  if (sv && !any(was_arr) && all(vapply(kids, .jsonlt_is_scalar, logical(1))))
    return(unlist(lapply(kids, function(k) if (is.null(k)) NA else k),
                  use.names = FALSE))
  kids
}

#' Parse JSON into R objects
#'
#' @param txt JSON text, or a path to a file holding it.
#' @param simplifyVector collapse a flat array to an atomic vector.
#' @param simplifyDataFrame collapse an array of objects to a data.frame.
#' @param simplifyMatrix collapse an array of equal-length arrays to a matrix.
#' @param flatten expand nested data.frame columns to outer.inner columns.
#' @param simplify FALSE turns all three simplifications off.
#' @param ... ignored, for call compatibility.
#' @return an R object.
#' @export
morie_jsonlt_from_json <- function(txt, simplifyVector = TRUE,
                                   simplifyDataFrame = TRUE,
                                   simplifyMatrix = TRUE, flatten = FALSE,
                                   simplify = NULL, ...) {
  if (identical(simplify, FALSE))
    simplifyVector <- simplifyDataFrame <- simplifyMatrix <- FALSE
  if (length(txt) == 1L && !grepl("^\\s*[\\[{\"[:digit:]-]", txt) &&
      file.exists(txt))
    txt <- readLines(txt, warn = FALSE)
  .jsonlt_simplify(.jsonlt_parse(txt), simplifyVector, simplifyDataFrame,
                   simplifyMatrix, flatten)
}

# ---------------------------------------------------------------- text ops

# Structure only: a comma inside a string stays a comma, which is why this
# walks characters instead of running a regex over the text.
#' Structure only: a comma inside a string stays a comma, which is why
#' this
#'
#' walks characters instead of running a regex over the text.
#'
#' @param txt See Usage.
#' @param open_pad See Usage.
#' @param close_pad See Usage.
#' @param comma_pad See Usage.
#' @param colon_txt See Usage.
#' @param keep_ws A flag; the body branches on it.
#' @return A character value.
#' @export
.jsonlt_walk <- function(txt, open_pad, close_pad, comma_pad, colon_txt,
                         keep_ws) {
  ch <- strsplit(paste(txt, collapse = ""), "", fixed = TRUE)[[1]]
  out <- character(0)
  depth <- 0L
  instr <- FALSE
  esc <- FALSE
  for (c0 in ch) {
    if (instr) {
      out <- c(out, c0)
      if (esc) esc <- FALSE
      else if (c0 == "\\") esc <- TRUE
      else if (c0 == "\"") instr <- FALSE
      next
    }
    if (c0 == "\"") { instr <- TRUE; out <- c(out, c0); next }
    if (c0 == "{" || c0 == "[") {
      depth <- depth + 1L
      out <- c(out, c0, open_pad(depth))
    } else if (c0 == "}" || c0 == "]") {
      depth <- depth - 1L
      out <- c(out, close_pad(depth), c0)
    } else if (c0 == ",") {
      out <- c(out, ",", comma_pad(depth))
    } else if (c0 == ":") {
      out <- c(out, colon_txt)
    } else if (c0 %in% c(" ", "\t", "\n", "\r")) {
      if (keep_ws) out <- c(out, c0)
    } else out <- c(out, c0)
  }
  paste(out, collapse = "")
}

#' Indent JSON text
#'
#' @param txt JSON text.
#' @param indent spaces per level.
#' @return the same JSON, indented.
#' @export
morie_jsonlt_prettify <- function(txt, indent = 4L) {
  ind <- as.integer(indent)
  pad <- function(d) paste0("\n", strrep(" ", d * ind))
  s <- .jsonlt_walk(txt, pad, pad, pad, ": ", FALSE)
  repeat {
    s2 <- gsub("\\{[[:space:]]*\\}", "{}", s)
    s2 <- gsub("\\[[[:space:]]*\\]", "[]", s2)
    if (identical(s2, s)) break
    s <- s2
  }
  s
}

#' Strip whitespace from JSON text
#'
#' @param txt JSON text.
#' @return the same JSON with no whitespace outside strings.
#' @export
morie_jsonlt_minify <- function(txt)
  .jsonlt_walk(txt, function(d) "", function(d) "", function(d) "", ":", FALSE)

#' Expand nested data.frame columns
#'
#' @param df a data.frame.
#' @param recursive expand nested frames all the way down.
#' @return a data.frame whose nested columns became outer.inner columns.
#' @export
morie_jsonlt_flatten <- function(df, recursive = TRUE) {
  if (!is.data.frame(df)) return(df)
  cols <- list()
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.data.frame(col)) {
      inner <- if (recursive) morie_jsonlt_flatten(col) else col
      for (nm2 in names(inner))
        cols[[paste0(nm, ".", nm2)]] <- inner[[nm2]]
    } else cols[[nm]] <- col
  }
  .jsonlt_df(cols, names(cols))
}

# ---------------------------------------------------- serialize/unserialize

#' .jsonlt_ser_opts
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return The value of \code{.jsonlt_opts}.
#' @export
.jsonlt_ser_opts <- function()
  .jsonlt_opts(digits = NULL, na = "null", auto_unbox = FALSE)

#' .jsonlt_ser_attr
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs A vector; its length is taken and its elements indexed.
#' @return A character value.
#' @export
.jsonlt_ser_attr <- function(pairs) {
  if (!length(pairs)) return("{}")
  paste0("{", paste(vapply(names(pairs), function(k)
    paste0(.jsonlt_esc(k), ":", .jsonlt_ser(pairs[[k]])), character(1)),
    collapse = ","), "}")
}

#' .jsonlt_rtype
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; the body checks with \code{is.numeric}.
#' @return A character value.
#' @export
.jsonlt_rtype <- function(x) {
  if (is.logical(x)) return("logical")
  if (is.integer(x)) return("integer")
  if (is.complex(x)) return("complex")
  if (is.numeric(x)) return("double")
  "character"
}

#' .jsonlt_ser
#'
#' A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser_attr}, \code{morie_jsonlt_serialize}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.
#' @return A character value.
#' @export
.jsonlt_ser <- function(x) {
  o <- .jsonlt_ser_opts()
  if (is.null(x)) return("{\"type\":\"NULL\",\"attributes\":{},\"value\":{}}")
  if (is.data.frame(x)) {
    at <- list(names = names(x), class = "data.frame",
               row.names = seq_len(nrow(x)))
    return(paste0("{\"type\":\"list\",\"attributes\":", .jsonlt_ser_attr(at),
                  ",\"value\":[",
                  paste(vapply(x, .jsonlt_ser, character(1)), collapse = ","),
                  "]}"))
  }
  if (is.factor(x)) {
    at <- list(levels = levels(x), class = "factor")
    return(paste0("{\"type\":\"integer\",\"attributes\":",
                  .jsonlt_ser_attr(at), ",\"value\":",
                  .jsonlt_atomic(as.integer(x), o, FALSE), "}"))
  }
  if (is.raw(x))
    return(paste0("{\"type\":\"raw\",\"attributes\":{},\"value\":",
                  .jsonlt_esc(.jsonlt_b64(x)), "}"))
  if (is.matrix(x)) {
    at <- list(dim = c(nrow(x), ncol(x)))
    return(paste0("{\"type\":\"", .jsonlt_rtype(x), "\",\"attributes\":",
                  .jsonlt_ser_attr(at), ",\"value\":",
                  .jsonlt_atomic(as.vector(x), o, FALSE), "}"))
  }
  if (is.list(x)) {
    at <- if (is.null(names(x))) list() else list(names = names(x))
    return(paste0("{\"type\":\"list\",\"attributes\":", .jsonlt_ser_attr(at),
                  ",\"value\":[",
                  paste(vapply(x, .jsonlt_ser, character(1)), collapse = ","),
                  "]}"))
  }
  paste0("{\"type\":\"", .jsonlt_rtype(x), "\",\"attributes\":{},\"value\":",
         .jsonlt_atomic(x, o, FALSE), "}")
}

#' Serialise an R object losslessly
#'
#' Type and attributes travel with the value, so the round trip returns the
#' same object rather than something that merely prints the same.
#'
#' @param x object to serialise.
#' @param pretty indent the output.
#' @return a length-one character vector of JSON.
#' @export
morie_jsonlt_serialize <- function(x, pretty = FALSE) {
  s <- .jsonlt_ser(x)
  if (isTRUE(pretty)) morie_jsonlt_prettify(s, 4L) else s
}

#' .jsonlt_coerce
#'
#' A step of the jsonlt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Optional; may be \code{NULL}. One of \code{"-Inf"}, \code{"Inf"}, \code{"NaN"}.
#' @param t One of \code{"complex"}, \code{"double"}, \code{"integer"}, \code{"logical"}.
#' @return A character value.
#' @export
.jsonlt_coerce <- function(v, t) {
  if (is.null(v)) return(switch(t, logical = NA, integer = NA_integer_,
                                double = NA_real_, complex = NA_complex_,
                                NA_character_))
  if (t == "logical") return(as.logical(v))
  if (t == "integer") return(as.integer(v))
  if (t == "double") {
    if (is.character(v)) {
      if (v == "Inf") return(Inf)
      if (v == "-Inf") return(-Inf)
      if (v == "NaN") return(NaN)
      return(as.numeric(v))
    }
    return(as.numeric(v))
  }
  if (t == "complex") return(as.complex(v))
  as.character(v)
}

#' .jsonlt_unser
#'
#' A step of the jsonlt_native implementation. Called by \code{morie_jsonlt_unserialize}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node A list; the body reads \code{$attributes}, \code{$type}, \code{$value} from it.
#' @return The value of \code{vec}, as built in the body.
#' @export
.jsonlt_unser <- function(node) {
  if (!is.list(node) || is.null(node$type))
    stop("jsonlt: not a serialize document", call. = FALSE)
  t <- node$type
  # An attribute is itself a serialized value and needs the same walk;
  # reading its "value" field directly is how a names vector comes back
  # as the literal string "type".
  at <- lapply(node$attributes, .jsonlt_unser)
  val <- node$value
  if (t == "NULL") return(NULL)
  if (t == "raw") return(.jsonlt_unb64(val))
  if (t == "list") {
    kids <- lapply(val, .jsonlt_unser)
    nms <- if (is.null(at$names)) NULL else as.character(at$names)
    if (!is.null(at$class) && "data.frame" %in% as.character(at$class)) {
      return(.jsonlt_df(kids, nms))
    }
    if (!is.null(nms) && length(nms) == length(kids)) names(kids) <- nms
    return(kids)
  }
  vals <- if (is.list(val)) val else as.list(val)
  vec <- unlist(lapply(vals, .jsonlt_coerce, t), use.names = FALSE)
  if (!is.null(at$levels))
    return(factor(as.character(at$levels)[vec],
                  levels = as.character(at$levels)))
  if (!is.null(at$dim)) {
    d <- as.integer(at$dim)
    return(matrix(vec, nrow = d[1], ncol = d[2]))
  }
  vec
}

#' Restore an object written by morie_jsonlt_serialize
#'
#' @param txt JSON produced by morie_jsonlt_serialize.
#' @return the original R object.
#' @export
morie_jsonlt_unserialize <- function(txt)
  .jsonlt_unser(morie_jsonlt_from_json(txt, simplify = FALSE))

# ---------------------------------------------------------------- route

#' jsonlite's JSON mapping, natively
#'
#' @param x the value or JSON text the route acts on.
#' @param route one of to_json, from_json, prettify, minify, flatten,
#'   serialize, unserialize.
#' @param ... options for the chosen route.
#' @return a list with route, result and method.
#' @export
morie_jsonlt <- function(x = NULL, route = "to_json", ...) {
  routes <- c("to_json", "from_json", "prettify", "minify", "flatten",
              "serialize", "unserialize")
  if (!(length(route) == 1L && route %in% routes))
    stop(sprintf("jsonlt: route = %s; expected one of %s", route,
                 paste(routes, collapse = ", ")), call. = FALSE)
  dots <- list(...)
  res <- switch(route,
    to_json = do.call(morie_jsonlt_to_json, c(list(x), dots)),
    from_json = do.call(morie_jsonlt_from_json, c(list(x), dots)),
    prettify = morie_jsonlt_prettify(x, if (is.null(dots$indent)) 4L else dots$indent),
    minify = morie_jsonlt_minify(x),
    flatten = morie_jsonlt_flatten(x, if (is.null(dots$recursive)) TRUE else dots$recursive),
    serialize = morie_jsonlt_serialize(x, if (is.null(dots$pretty)) FALSE else dots$pretty),
    unserialize = morie_jsonlt_unserialize(x))
  list(route = route, result = res,
       method = "jsonlite mapping (Ooms 2014), RFC 8259")
}
