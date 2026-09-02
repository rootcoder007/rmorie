# jsonlt -- jsonlite's JSON mapping, implemented natively in base R.
#
# This is the complete public surface of jsonlite 2.0.0 (toJSON / fromJSON
# with every option, read_json / write_json / parse_json, prettify / minify /
# validate, flatten, unbox, serializeJSON / unserializeJSON, base64,
# stream_in / stream_out, rbind_pages), written so that the bytes match:
# the encoder reproduces jsonlite's asJSON dispatch and its C helpers
# (num_to_char / modp_dtoa2, escape_chars, collapse_*), the simplifier
# reproduces simplify() / simplifyDataFrame() / null_to_na(), and the text
# tools reproduce yajl's generator. A parity harness runs every option over
# a corpus of R objects against jsonlite itself and asserts identical
# output; the codec exists so nothing in the package needs jsonlite at run
# time.
#
# Reference
#   Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
#     Mapping Between JSON Data and R Objects." arXiv:1403.2805.
#   Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
#     Interchange Format." RFC 8259.
#   jsonlite 2.0.0 sources (R/ and src/), the executable specification.

# ================================================================ numbers

#' Format doubles and integers the way jsonlite's num_to_char() does
#'
#' `digits` is decimal places (jsonlite default 4), `I(n)` means n
#' significant digits, `NA` means 15 significant digits. Non-finite values
#' become "NA"/"NaN"/"Inf"/"-Inf" strings, null, or NA_character_
#' (dropped later) depending on `na_as_string`.
#' @noRd
.jsonlt_num_to_char <- function(x, digits = 4, na_as_string = NA,
                                use_signif = FALSE, always_decimal = FALSE) {
  n <- length(x)
  out <- character(n)
  if (is.integer(x)) {
    ok <- !is.na(x)
    out[ok] <- as.character(x[ok])
    out[!ok] <- if (is.na(na_as_string)) NA_character_
                else if (na_as_string) "\"NA\"" else "null"
    return(out)
  }
  # digits = NULL is this codec's own extension: full precision (%.17g), the
  # setting the package's internal JSON round trips rely on
  if (is.null(digits)) {
    digits <- 17L
    use_signif <- TRUE
  }
  precision <- if (is.na(digits)) NA_integer_ else as.integer(digits)
  sig_digits <- if (isTRUE(use_signif) && !is.na(precision)) ceiling(min(17, precision)) else 0L
  for (i in seq_len(n)) {
    val <- x[i]
    if (!is.finite(val)) {
      out[i] <- if (is.na(na_as_string)) {
        NA_character_
      } else if (na_as_string) {
        if (is.na(val) && !is.nan(val)) "\"NA\""
        else if (is.nan(val)) "\"NaN\""
        else if (val > 0) "\"Inf\"" else "\"-Inf\""
      } else {
        "null"
      }
      next
    }
    buf <- if (is.na(precision)) {
      sprintf("%.15g", val)
    } else if (isTRUE(use_signif)) {
      sprintf("%.*g", as.integer(sig_digits), val)
    } else if (precision > -1L && precision < 10L && abs(val) < 2147483647 &&
               abs(val) > 1e-5) {
      .jsonlt_dtoa2(val, precision)
    } else {
      decimals <- ceiling(min(17, max(1, log10(abs(val))) + precision))
      sprintf("%.*g", as.integer(decimals), val)
    }
    if (isTRUE(always_decimal) && grepl("^[0-9-]+$", buf)) buf <- paste0(buf, ".0")
    out[i] <- buf
  }
  out
}

# modp_dtoa2(): fixed decimal digits, trailing zeros removed, with its own
# rounding rule (round half to odd-frac / half-up on rollover). Reproduced
# step by step so the last digit matches jsonlite's.
#' @noRd
.jsonlt_dtoa2 <- function(value, prec) {
  prec <- max(0L, min(9L, as.integer(prec)))
  p10 <- 10^prec
  neg <- value < 0
  if (neg) value <- -value
  whole <- trunc(value)
  tmp <- (value - whole) * p10
  frac <- trunc(tmp)
  diff <- tmp - frac
  if (diff > 0.5) {
    frac <- frac + 1
    if (frac >= p10) {
      frac <- 0
      whole <- whole + 1
    }
  } else if (diff == 0.5 && prec > 0L && (frac %% 2 == 1)) {
    frac <- frac + 1
    if (frac >= p10) {
      frac <- 0
      whole <- whole + 1
    }
  } else if (diff == 0.5 && prec == 0L && (whole %% 2 == 1)) {
    frac <- frac + 1
    if (frac >= p10) {
      frac <- 0
      whole <- whole + 1
    }
  }
  if (value > 2147483647) return(sprintf("%e", if (neg) -value else value))
  count <- prec
  if (prec > 0L) while (count > 0L && (frac %% 10 == 0)) {
    count <- count - 1L
    frac <- frac %/% 10
  }
  digs <- character(0)
  while (count > 0L) {
    count <- count - 1L
    digs <- c(as.character(frac %% 10), digs)
    frac <- frac %/% 10
  }
  if (frac > 0) whole <- whole + 1
  s <- sprintf("%.0f", whole)
  if (length(digs)) s <- paste0(s, ".", paste(digs, collapse = ""))
  if (neg) s <- paste0("-", s)
  s
}

# ================================================================ strings

#' Escape a character vector as JSON string literals (jsonlite's C_escape_chars)
#' @noRd
.jsonlt_esc <- function(x) {
  if (!length(x)) return(character(0))
  x <- enc2utf8(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  x <- gsub("\b", "\\b", x, fixed = TRUE)
  x <- gsub("\f", "\\f", x, fixed = TRUE)
  x <- gsub("</", "<\\/", x, fixed = TRUE)
  for (cc in c(1:7, 11L, 14:31)) {
    ch <- rawToChar(as.raw(cc))
    hit <- grepl(ch, x, fixed = TRUE, useBytes = TRUE)
    if (any(hit)) x[hit] <- gsub(ch, sprintf("\\u%04x", cc), x[hit], fixed = TRUE, useBytes = TRUE)
  }
  paste0("\"", x, "\"")
}

# ================================================================ base64

.JSONLT_B64 <- strsplit(paste0("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                               "abcdefghijklmnopqrstuvwxyz",
                               "0123456789+/"), "")[[1]]

#' Base64 encode a raw vector or character string
#'
#' @param input raw vector, or character (joined by newlines first).
#' @return a base64 string; `NA_character_` for `NULL`.
#' @export
morie_jsonlt_base64_enc <- function(input) {
  if (is.null(input)) return(NA_character_)
  if (is.character(input)) input <- charToRaw(paste(input, collapse = "\n"))
  stopifnot(is.raw(input))
  b <- as.integer(input)
  n <- length(b)
  if (!n) return("")
  pad <- (3L - n %% 3L) %% 3L
  b <- c(b, integer(pad))
  m <- matrix(b, nrow = 3L)
  v <- m[1L, ] * 65536L + m[2L, ] * 256L + m[3L, ]
  chars <- rbind(.JSONLT_B64[v %/% 262144L %% 64L + 1L],
                 .JSONLT_B64[v %/% 4096L %% 64L + 1L],
                 .JSONLT_B64[v %/% 64L %% 64L + 1L],
                 .JSONLT_B64[v %% 64L + 1L])
  out <- paste(as.vector(chars), collapse = "")
  if (pad) substr(out, nchar(out) - pad + 1L, nchar(out)) <- strrep("=", pad)
  # jsonlite's encoder breaks lines every 72 characters
  if (nchar(out) > 72L) {
    starts <- seq(1L, nchar(out), by = 72L)
    out <- paste(substring(out, starts, pmin(starts + 71L, nchar(out))), collapse = "\n")
  }
  out
}

#' Base64 decode to a raw vector
#'
#' @param input base64 text (character, joined by newlines) or raw.
#' @return a raw vector.
#' @export
morie_jsonlt_base64_dec <- function(input) {
  if (is.character(input)) input <- charToRaw(paste(input, collapse = "\n"))
  stopifnot(is.raw(input))
  s <- rawToChar(input)
  ch <- strsplit(gsub("[^A-Za-z0-9+/]", "", s), "")[[1]]
  if (!length(ch)) return(raw(0))
  v <- match(ch, .JSONLT_B64) - 1L
  if (anyNA(v)) stop("Error in base64 decode", call. = FALSE)
  nfull <- length(v) %/% 4L
  rem <- length(v) %% 4L
  out <- integer(0)
  if (nfull) {
    m <- matrix(v[seq_len(nfull * 4L)], nrow = 4L)
    w <- m[1L, ] * 262144L + m[2L, ] * 4096L + m[3L, ] * 64L + m[4L, ]
    out <- as.vector(rbind(w %/% 65536L, w %/% 256L %% 256L, w %% 256L))
  }
  if (rem == 2L) {
    w <- v[nfull * 4L + 1L] * 262144L + v[nfull * 4L + 2L] * 4096L
    out <- c(out, w %/% 65536L)
  } else if (rem == 3L) {
    w <- v[nfull * 4L + 1L] * 262144L + v[nfull * 4L + 2L] * 4096L + v[nfull * 4L + 3L] * 64L
    out <- c(out, w %/% 65536L, w %/% 256L %% 256L)
  }
  as.raw(out)
}

#' @rdname morie_jsonlt_base64_enc
#' @export
morie_jsonlt_base64url_enc <- function(input) {
  sub("=+$", "", chartr("+/", "-_", morie_jsonlt_base64_enc(input)))
}

#' @rdname morie_jsonlt_base64_dec
#' @export
morie_jsonlt_base64url_dec <- function(input) {
  text <- gsub("[\r\n]", "", chartr("-_", "+/", input))[[1]]
  mod <- nchar(text) %% 4L
  if (mod > 0L) text <- paste0(text, strrep("=", 4L - mod))
  morie_jsonlt_base64_dec(text)
}

# ================================================================ collapse

# jsonlite's C collapse helpers. `indent` is NA for compact output, else an
# integer with attribute indent_spaces (negative = tabs).
#' @noRd
.jsonlt_indent_init <- function(pretty) {
  if (isTRUE(pretty)) pretty <- 2L
  if (is.numeric(pretty)) {
    stopifnot(abs(pretty) < 20)
    structure(0L, indent_spaces = as.integer(pretty))
  } else {
    NA_integer_
  }
}
#' @noRd
.jsonlt_indent_inc <- function(indent) {
  sp <- attr(indent, "indent_spaces")
  if (length(sp)) structure(indent + abs(sp), indent_spaces = sp) else NA_integer_
}
#' @noRd
.jsonlt_ws <- function(n, indent) {
  sp <- attr(indent, "indent_spaces")
  strrep(if (length(sp) && sp < 0) "\t" else " ", n)
}
#' @noRd
.jsonlt_collapse <- function(x, inner = TRUE, indent = NA_integer_) {
  if (is.na(indent)) return(paste0("[", paste(x, collapse = ","), "]"))
  if (isTRUE(inner)) return(paste0("[", paste(x, collapse = ", "), "]"))
  if (!length(x)) return("[]")
  ni <- as.integer(indent)
  sp <- abs(attr(indent, "indent_spaces"))
  paste0("[\n", paste0(.jsonlt_ws(ni + sp, indent), x, collapse = ",\n"),
         "\n", .jsonlt_ws(ni, indent), "]")
}
#' @noRd
.jsonlt_collapse_object <- function(keys, vals, indent = NA_integer_) {
  keep <- !is.na(vals)
  keys <- keys[keep]
  vals <- vals[keep]
  if (!length(vals)) return("{}")
  if (is.na(indent)) return(paste0("{", paste0(keys, ":", vals, collapse = ","), "}"))
  if (!length(vals)) return("{}")
  ni <- as.integer(indent)
  sp <- abs(attr(indent, "indent_spaces"))
  paste0("{\n", paste0(.jsonlt_ws(ni + sp, indent), keys, ": ", vals, collapse = ",\n"),
         "\n", .jsonlt_ws(ni, indent), "}")
}
#' @noRd
.jsonlt_row_collapse <- function(m, indent = NA_integer_) {
  vapply(seq_len(nrow(m)), function(i) .jsonlt_collapse(m[i, ], inner = TRUE, indent = indent),
         character(1))
}
#' @noRd
.jsonlt_row_collapse_object <- function(keys, m, indent = NA_integer_) {
  vapply(seq_len(nrow(m)), function(i) .jsonlt_collapse_object(keys, m[i, ], indent),
         character(1))
}
#' @noRd
.jsonlt_cleannames <- function(nms, no_dots = FALSE) {
  nms[nms == ""] <- NA_character_
  miss <- is.na(nms)
  nms[miss] <- as.character(seq_along(nms))[miss]
  if (isTRUE(no_dots)) nms <- gsub(".", "_", nms, fixed = TRUE)
  make.unique(nms)
}

# ================================================================ encoder

# The encoder is jsonlite's asJSON generic, one branch per class, with the
# same argument threading (na / oldna / collapse / auto_unbox / indent).
#' @noRd
.jsonlt_as <- function(x, o, collapse = TRUE, na = NULL, oldna = NULL,
                       auto_unbox = o$auto_unbox, indent = o$indent,
                       is_df = FALSE, keep_vec_names = o$keep_vec_names) {
  cls <- class(x)
  if (inherits(x, "scalar")) {
    y <- x
    if (length(cls) > 1L) class(y) <- cls[-1L] else y <- unclass(y)
    return(.jsonlt_as(y, o, collapse = FALSE, na = na, oldna = oldna,
                      auto_unbox = auto_unbox, indent = indent))
  }
  if (inherits(x, "json")) {
    if (isTRUE(o$json_verbatim)) return(as.character(x))
    return(.jsonlt_as(as.character(x), o, collapse, na, oldna, auto_unbox, indent))
  }
  if (inherits(x, "AsIs") && !is.data.frame(x)) {
    class(x) <- setdiff(cls, "AsIs")
    if (is.atomic(x) && length(x) == 1L) auto_unbox <- FALSE
    return(.jsonlt_as(x, o, collapse, na, oldna, auto_unbox, indent))
  }
  if (is.data.frame(x))
    return(.jsonlt_as_df(x, o, collapse, na, oldna, indent, keep_vec_names))
  if (is.factor(x)) {
    if (identical(o$factor, "integer"))
      return(.jsonlt_as(unclass(x), o, collapse, na, oldna, auto_unbox, indent))
    xc <- as.character(x)
    if (isTRUE(keep_vec_names)) names(xc) <- names(x)
    return(.jsonlt_as(xc, o, collapse, na, oldna, auto_unbox, indent, keep_vec_names = keep_vec_names))
  }
  if (inherits(x, "Date")) {
    out <- if (identical(o$Date, "epoch")) unclass(x) else format(x)
    return(.jsonlt_as(out, o, collapse, na, oldna, auto_unbox, indent))
  }
  if (inherits(x, "POSIXt")) {
    if (identical(o$POSIXt, "mongo")) {
      if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
      df <- data.frame("$date" = floor(unclass(x) * 1000), check.names = FALSE)
      o2 <- o
      o2$digits <- NA
      o2$always_decimal <- FALSE
      tmp <- .jsonlt_as(df, o2, collapse = FALSE, na = na, oldna = oldna, indent = indent)
      tmp[is.na(x)] <- .jsonlt_as(NA_character_, o, collapse = FALSE, na = na)
      return(if (isTRUE(collapse)) .jsonlt_collapse(tmp, inner = FALSE, indent = indent) else tmp)
    }
    if (identical(o$POSIXt, "epoch")) {
      o2 <- o
      o2$always_decimal <- FALSE
      return(.jsonlt_as(floor(unclass(as.POSIXct(x)) * 1000), o2, collapse, na, oldna, auto_unbox, indent))
    }
    fmt <- o$time_format
    if (is.null(fmt)) fmt <- if (identical(o$POSIXt, "string")) "" else if (isTRUE(o$UTC)) "%Y-%m-%dT%H:%M:%SZ" else "%Y-%m-%dT%H:%M:%S"
    s <- if (isTRUE(o$UTC)) format(x, format = fmt, tz = "UTC") else format(x, format = fmt)
    return(.jsonlt_as(s, o, collapse, na, oldna, auto_unbox, indent))
  }
  if (inherits(x, "ts")) return(.jsonlt_as(as.vector(x), o, collapse, na, oldna, auto_unbox, indent))
  if (inherits(x, "hms")) {
    out <- if (identical(o$hms, "secs")) as.numeric(x, units = "secs") else as.character(x)
    out[is.na(x)] <- NA
    return(.jsonlt_as(out, o, collapse, na, oldna, auto_unbox, indent))
  }
  if (inherits(x, "blob")) {
    if (identical(o$raw, "base64")) {
      str <- vapply(x, morie_jsonlt_base64_enc, character(1))
      return(.jsonlt_as(str, o, collapse, na, oldna, auto_unbox, indent))
    }
    return(.jsonlt_as(unclass(as.list(x)), o, collapse, na, oldna, auto_unbox, indent))
  }
  if (is.null(x)) return(if (identical(o$null, "null")) "null" else "{}")
  # an S3 class none of the branches above know: jsonlite's ANY method tries
  # the remaining classes, then unclasses under force = TRUE, else errors
  if (!is.null(oldClass(x)) && !is.data.frame(x)) {
    if (length(cls) > 1L) {
      class(x) <- cls[-1L]
      return(.jsonlt_as(x, o, collapse, na, oldna, auto_unbox, indent))
    }
    if (isTRUE(o$force)) {
      return(.jsonlt_as(unclass(x), o, collapse, na, oldna, auto_unbox, indent))
    }
    stop("No method asJSON S3 class: ", cls, call. = FALSE)
  }
  # base storage types (after S3 classes, the way jsonlite's S4 dispatch falls
  # through to the implicit class)
  if (is.raw(x)) return(.jsonlt_as_raw(x, o, collapse, na, oldna, auto_unbox, indent))
  if (is.matrix(x) || (is.array(x) && length(dim(x)) >= 2L))
    return(.jsonlt_as_array(x, o, collapse, na, oldna, auto_unbox, indent, keep_vec_names))
  if (is.list(x) || is.pairlist(x))
    return(.jsonlt_as_list(x, o, collapse, na, oldna, auto_unbox, indent, is_df))
  if (is.character(x)) return(.jsonlt_as_chr(x, o, collapse, na, auto_unbox, indent, keep_vec_names))
  if (is.logical(x)) return(.jsonlt_as_lgl(x, o, collapse, na, auto_unbox, indent, keep_vec_names))
  if (is.complex(x)) return(.jsonlt_as_cplx(x, o, collapse, na, oldna, auto_unbox, indent))
  if (is.numeric(x)) return(.jsonlt_as_num(x, o, collapse, na, auto_unbox, indent, keep_vec_names))
  if (length(cls) > 1L) {
    class(x) <- cls[-1L]
    return(.jsonlt_as(x, o, collapse, na, oldna, auto_unbox, indent))
  }
  if (isS4(x)) {
    if (isTRUE(o$force)) return(.jsonlt_as(attributes(x), o, collapse, na, oldna, auto_unbox, indent))
    stop("No method for S4 class:", cls, call. = FALSE)
  }
  if (isTRUE(o$force)) {
    y <- unclass(x)
    if (is.atomic(y) || is.list(y) || is.null(y))
      return(.jsonlt_as(y, o, collapse, na, oldna, auto_unbox, indent))
    return(.jsonlt_as(NULL, o))
  }
  stop("No method asJSON S3 class: ", cls, call. = FALSE)
}

#' @noRd
.jsonlt_as_num <- function(x, o, collapse, na, auto_unbox, indent, keep_vec_names) {
  if (isTRUE(keep_vec_names) && length(names(x))) {
    message("Input to asJSON(keep_vec_names=TRUE) is a named vector. In a future version of jsonlite, this option will not be supported, and named vectors will be translated into arrays instead of objects. If you want JSON object output, please use a named list instead. See ?toJSON.")
    return(.jsonlt_as(as.list(x), o, collapse, na = na, auto_unbox = TRUE, indent = indent))
  }
  na <- if (is.null(na)) "string" else match.arg(na, c("string", "null", "NA"))
  nas <- switch(na, string = TRUE, null = FALSE, "NA" = NA)
  if (!is.null(attr(x, "dim")) && length(dim(x)) < 2L) x <- c(x)
  tmp <- .jsonlt_num_to_char(x, o$digits, nas, o$use_signif, o$always_decimal)
  if (isTRUE(auto_unbox) && length(tmp) == 1L) return(tmp)
  if (collapse) .jsonlt_collapse(tmp, indent = indent) else tmp
}
#' @noRd
.jsonlt_as_chr <- function(x, o, collapse, na, auto_unbox, indent, keep_vec_names) {
  if (isTRUE(keep_vec_names) && length(names(x))) {
    message("Input to asJSON(keep_vec_names=TRUE) is a named vector. In a future version of jsonlite, this option will not be supported, and named vectors will be translated into arrays instead of objects. If you want JSON object output, please use a named list instead. See ?toJSON.")
    return(.jsonlt_as(as.list(x), o, collapse, na = na, auto_unbox = TRUE, indent = indent))
  }
  tmp <- .jsonlt_esc(as.character(x))
  miss <- which(is.na(x))
  if (length(miss)) {
    na <- if (is.null(na)) "null" else match.arg(na, c("null", "string", "NA"))
    tmp[miss] <- if (na == "null") "null" else if (na == "string") "\"NA\"" else NA_character_
  }
  if (isTRUE(auto_unbox) && length(tmp) == 1L) return(tmp)
  if (isTRUE(collapse)) .jsonlt_collapse(tmp, indent = indent) else tmp
}
#' @noRd
.jsonlt_as_lgl <- function(x, o, collapse, na, auto_unbox, indent, keep_vec_names) {
  if (isTRUE(keep_vec_names) && length(names(x))) {
    message("Input to asJSON(keep_vec_names=TRUE) is a named vector. In a future version of jsonlite, this option will not be supported, and named vectors will be translated into arrays instead of objects. If you want JSON object output, please use a named list instead. See ?toJSON.")
    return(.jsonlt_as(as.list(x), o, collapse, na = na, auto_unbox = TRUE, indent = indent))
  }
  na <- if (is.null(na)) "null" else match.arg(na, c("null", "string", "NA"))
  tmp <- ifelse(x, "true", "false")
  if (!identical(na, "NA")) {
    miss <- which(is.na(x))
    if (length(miss)) tmp[miss] <- if (identical(na, "string")) "\"NA\"" else "null"
  }
  if (!is.character(tmp)) tmp <- as.character(tmp)
  if (isTRUE(auto_unbox) && length(tmp) == 1L) return(tmp)
  if (collapse) .jsonlt_collapse(tmp, indent = indent) else tmp
}
#' @noRd
.jsonlt_as_cplx <- function(x, o, collapse, na, oldna, auto_unbox, indent) {
  na <- if (is.null(na)) "string" else match.arg(na, c("string", "null", "NA"))
  if (identical(o$complex, "string")) {
    mystring <- prettyNum(x = x, digits = if (is.null(o$digits)) 5 else o$digits)
    miss <- which(!is.finite(x))
    if (length(miss) && na %in% c("null", "NA")) mystring[miss] <- NA_character_
    return(.jsonlt_as_chr(mystring, o, collapse, na, auto_unbox, indent, FALSE))
  }
  if (na == "NA") na <- oldna
  .jsonlt_as(list(real = Re(x), imaginary = Im(x)), o, collapse, na = na, oldna = oldna,
             auto_unbox = auto_unbox, indent = indent)
}
#' @noRd
.jsonlt_as_raw <- function(x, o, collapse, na, oldna, auto_unbox, indent) {
  raw <- o$raw
  if (raw == "mongo") {
    type <- if (length(attr(x, "type"))) attr(x, "type") else 5
    return(.jsonlt_as(list(`$binary` = morie_jsonlt_unbox(morie_jsonlt_base64_enc(x)),
                           `$type` = morie_jsonlt_unbox(as.character(type))), o))
  }
  if (raw == "hex") return(.jsonlt_as(format(as.hexmode(as.integer(x)), width = 2L), o, collapse, na, oldna, auto_unbox, indent))
  if (raw == "int") return(.jsonlt_as(as.integer(x), o, collapse, na, oldna, auto_unbox, indent))
  if (raw == "js") return(paste0("(new Uint8Array(", .jsonlt_as(as.integer(x), o, collapse = TRUE), "))"))
  .jsonlt_as(morie_jsonlt_base64_enc(x), o, collapse, na, oldna, auto_unbox, indent)
}
#' @noRd
.jsonlt_as_array <- function(x, o, collapse, na, oldna, auto_unbox, indent, keep_vec_names) {
  if (identical(na, "NA")) na <- oldna
  if (length(dim(x)) < 2L)
    return(.jsonlt_as(c(x), o, collapse, na = na, indent = .jsonlt_indent_inc(indent)))
  columnmajor <- identical(o$matrix, "columnmajor")
  if (columnmajor && !collapse)
    return(apply(x, 1L, function(r) .jsonlt_as(r, o, na = na, indent = .jsonlt_indent_inc(indent))))
  m <- .jsonlt_as(c(x), o, collapse = FALSE, na = na, auto_unbox = FALSE)
  dim(m) <- dim(x)
  tmp <- if (length(dim(x)) == 2L && !columnmajor) {
    .jsonlt_row_collapse(m, indent = .jsonlt_indent_inc(indent))
  } else {
    .jsonlt_collapse_array(m, columnmajor = columnmajor, indent = indent)
  }
  if (collapse) .jsonlt_collapse(tmp, inner = FALSE, indent = indent) else tmp
}
#' @noRd
.jsonlt_collapse_array <- function(x, columnmajor = FALSE, indent) {
  n <- length(dim(x))
  dims <- 1:(n - 1) + as.numeric(columnmajor)
  x <- apply(x, dims, .jsonlt_collapse, inner = TRUE, indent = indent)
  for (i in rev(seq_along(dim(x)))[-1]) {
    dims <- 1:(length(dim(x)) - 1) + as.numeric(columnmajor)
    inc <- .jsonlt_indent_inc(indent)
    ind <- if (is.na(inc)) inc else structure(as.integer(inc) * i, indent_spaces = attr(inc, "indent_spaces"))
    x <- apply(x, dims, .jsonlt_collapse, inner = FALSE, indent = ind)
  }
  x
}
#' @noRd
.jsonlt_as_list <- function(x, o, collapse, na, oldna, auto_unbox, indent, is_df) {
  if (identical(na, "NA")) na <- oldna
  if (is.pairlist(x)) x <- as.vector(x, mode = "list")
  inc <- .jsonlt_indent_inc(indent)
  tmp <- if (is_df && auto_unbox) {
    vapply(x, function(y) .jsonlt_as(y, o, na = na, oldna = oldna, auto_unbox = is.list(y), indent = inc), character(1))
  } else {
    vapply(x, function(y) .jsonlt_as(y, o, na = na, oldna = oldna, auto_unbox = auto_unbox, indent = inc), character(1))
  }
  if (!is.null(names(x))) {
    keys <- .jsonlt_esc(.jsonlt_cleannames(names(x), o$no_dots))
    return(.jsonlt_collapse_object(keys, unname(tmp), indent))
  }
  if (collapse) .jsonlt_collapse(unname(tmp), inner = FALSE, indent = indent) else unname(tmp)
}
#' @noRd
.jsonlt_as_df <- function(x, o, collapse, na, oldna, indent, keep_vec_names) {
  dataframe <- o$dataframe
  has_names <- identical(length(names(x)), ncol(x))
  rn <- attr(x, "row.names")
  if (isTRUE(o$rownames) || (is.null(o$rownames) && is.character(rn) && !all(grepl("^\\d+$", rn)))) {
    if (has_names) x[["_row"]] <- rn
  }
  for (i in which(vapply(x, function(c) isTRUE(is.list(c) && !is.data.frame(c) && !is.null(names(c))), logical(1))))
    x[[i]] <- unname(x[[i]])
  for (i in which(vapply(x, inherits, logical(1), "POSIXlt"))) x[[i]] <- as.POSIXct(x[[i]])
  if (dataframe == "columns")
    return(.jsonlt_as_list(as.list(x), o, collapse = TRUE, na = na, oldna = oldna,
                           auto_unbox = o$auto_unbox, indent = indent, is_df = TRUE))
  if (is.null(na) || !length(na) || identical(na, "NA")) oldna <- NULL else oldna <- na
  if (dataframe == "rows" && has_names) na <- if (is.null(na)) "NA" else match.arg(na, c("NA", "null", "string"))
  if (!nrow(x)) return(.jsonlt_as(list(), o, collapse = collapse, indent = indent))
  for (i in which(vapply(x, is.raw, logical(1)))) x[[i]] <- format(as.hexmode(as.integer(x[[i]])), width = 2L)
  if (identical(o$complex, "list"))
    for (i in which(vapply(x, is.complex, logical(1))))
      x[[i]] <- data.frame(real = Re(x[[i]]), imaginary = Im(x[[i]]))
  dfnames <- .jsonlt_esc(.jsonlt_cleannames(names(x), o$no_dots))
  inc <- .jsonlt_indent_inc(indent)
  cols <- lapply(x, function(col) {
    .jsonlt_as(col, o, collapse = FALSE, na = na, oldna = oldna,
               auto_unbox = o$auto_unbox, indent = inc)
  })
  out <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(x))
  tmp <- if (dataframe == "rows" && length(dfnames) == ncol(out)) {
    .jsonlt_row_collapse_object(dfnames, out, indent = inc)
  } else {
    .jsonlt_row_collapse(out, indent = indent)
  }
  if (isTRUE(collapse)) .jsonlt_collapse(tmp, inner = FALSE, indent = indent) else tmp
}

#' Encode an R object as JSON (jsonlite's toJSON, natively)
#'
#' Every option of jsonlite's `toJSON()` with the same default and the same
#' bytes out: `dataframe`, `matrix`, `Date`, `POSIXt`, `factor`, `complex`,
#' `raw`, `null`, `na`, `auto_unbox`, `digits` (`I(n)` for significant
#' digits, `NA` for 15), `pretty` (TRUE = 2 spaces, or a width), `force`,
#' plus `rownames`, `keep_vec_names`, `json_verbatim`, `always_decimal`,
#' `time_format`, `UTC`, `no_dots`, `hms`.
#'
#' @param x the object to encode.
#' @param dataframe,matrix,Date,POSIXt,factor,complex,raw,null,na,auto_unbox,digits,pretty,force,... as in jsonlite.
#' @return a length-one character vector of class `json`.
#' @examples
#' morie_jsonlt_to_json(list(a = 1:3, b = "x"), auto_unbox = TRUE)
#' morie_jsonlt_to_json(data.frame(id = 1:2, v = c(1.5, NA)), pretty = TRUE)
#' @export
morie_jsonlt_to_json <- function(x, dataframe = c("rows", "columns", "values"),
                                 matrix = c("rowmajor", "columnmajor"),
                                 Date = c("ISO8601", "epoch"),
                                 POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                                 factor = c("string", "integer"),
                                 complex = c("string", "list"),
                                 raw = c("base64", "hex", "mongo", "int", "js"),
                                 null = c("list", "null"), na = c("null", "string"),
                                 auto_unbox = FALSE, digits = 4, pretty = FALSE,
                                 force = FALSE, ...) {
  dots <- list(...)
  o <- list(dataframe = match.arg(dataframe), matrix = match.arg(matrix),
            Date = match.arg(Date), POSIXt = match.arg(POSIXt), factor = match.arg(factor),
            complex = match.arg(complex), raw = match.arg(raw), null = match.arg(null),
            auto_unbox = isTRUE(auto_unbox), digits = digits,
            use_signif = if (!is.null(dots$use_signif)) isTRUE(dots$use_signif) else inherits(digits, "AsIs"),
            force = isTRUE(force), indent = .jsonlt_indent_init(pretty),
            rownames = dots$rownames, keep_vec_names = isTRUE(dots$keep_vec_names),
            json_verbatim = isTRUE(dots$json_verbatim), always_decimal = isTRUE(dots$always_decimal),
            time_format = dots$time_format, UTC = isTRUE(dots$UTC), no_dots = isTRUE(dots$no_dots),
            hms = if (is.null(dots$hms)) "string" else match.arg(dots$hms, c("string", "secs")))
  na <- if (!missing(na)) match.arg(na) else NULL
  ans <- .jsonlt_as(x, o, na = na, oldna = NULL, auto_unbox = o$auto_unbox, indent = o$indent)
  class(ans) <- "json"
  ans
}

#' Mark a value as a JSON scalar (jsonlite's unbox)
#'
#' @param x an atomic vector of length one, a one-row data.frame, or a
#'   length-one POSIXt.
#' @return `x` with class `scalar`, so it is written without brackets.
#' @export
morie_jsonlt_unbox <- function(x) {
  if (is.null(x)) return(x)
  if (is.data.frame(x)) {
    if (nrow(x) == 1L) return(.jsonlt_as_scalar(x))
    stop("Tried to unbox dataframe with ", nrow(x), " rows.", call. = FALSE)
  }
  if (length(x) == 1L && inherits(x, "POSIXt")) return(.jsonlt_as_scalar(x))
  if (!is.atomic(x) || length(dim(x)) > 1L)
    stop("Only atomic vectors of length 1 or data frames with 1 row can be unboxed.", call. = FALSE)
  if (identical(length(x), 1L)) return(.jsonlt_as_scalar(x))
  stop("Tried to unbox a vector of length ", length(x), call. = FALSE)
}
#' @noRd
.jsonlt_as_scalar <- function(obj) {
  class(obj) <- c("scalar", class(obj))
  obj
}

#' @param x See Usage.
#' @export
print.json <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

# ================================================================ parser

# A yajl-equivalent parser: one pass over the text with a byte cursor.
# Numbers follow yajl_tree: no '.'/'e' -> integer (double above 2^31-1,
# character above 2^53 when bigint_as_char); otherwise strtod.
#' @noRd
.jsonlt_parse <- function(txt, bigint_as_char = FALSE) {
  s <- paste(txt, collapse = "\n")
  s <- enc2utf8(s)
  if (startsWith(s, "\ufeff")) {
    warning("JSON string contains (illegal) UTF8 byte-order-mark!", call. = FALSE)
    s <- substring(s, 2L)
  }
  if (startsWith(s, "\x1e")) s <- substring(s, 2L)
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  i <- 1L
  bad <- function(msg) stop(sprintf("%s at character %d", msg, i), call. = FALSE)
  ws <- function() while (i <= n && (ch[i] == " " || ch[i] == "\t" || ch[i] == "\n" || ch[i] == "\r")) i <<- i + 1L
  str_ <- function() {
    i <<- i + 1L
    st <- i
    parts <- character(0)
    repeat {
      if (i > n) bad("unterminated string")
      c0 <- ch[i]
      if (c0 == "\"") break
      if (c0 == "\\") {
        if (i > st) parts <- c(parts, paste(ch[st:(i - 1L)], collapse = ""))
        i <<- i + 1L
        e <- ch[i]
        parts <- c(parts, switch(e, n = "\n", t = "\t", r = "\r", b = "\b", f = "\f",
                                 "\"" = "\"", "\\" = "\\", "/" = "/",
                                 u = {
                                   cp <- strtoi(paste(ch[(i + 1L):(i + 4L)], collapse = ""), 16L)
                                   if (is.na(cp)) bad("invalid \\u escape")
                                   i <<- i + 4L
                                   if (cp >= 0xD800 && cp <= 0xDBFF && i + 6L <= n && ch[i + 1L] == "\\" && ch[i + 2L] == "u") {
                                     lo <- strtoi(paste(ch[(i + 3L):(i + 6L)], collapse = ""), 16L)
                                     if (!is.na(lo) && lo >= 0xDC00 && lo <= 0xDFFF) {
                                       i <<- i + 6L
                                       cp <- 0x10000 + (cp - 0xD800) * 1024 + (lo - 0xDC00)
                                     }
                                   }
                                   if (cp == 0) bad("NUL in string")
                                   intToUtf8(cp)
                                 },
                                 bad("invalid escape")))
        i <<- i + 1L
        st <- i
        next
      }
      if (utf8ToInt(c0) < 32L) bad("control character in string")
      i <<- i + 1L
    }
    if (i > st) parts <- c(parts, paste(ch[st:(i - 1L)], collapse = ""))
    i <<- i + 1L
    paste(parts, collapse = "")
  }
  num_ <- function() {
    st <- i
    if (ch[i] == "-") i <<- i + 1L
    if (i > n || !grepl("[0-9]", ch[i])) bad("invalid number")
    if (ch[i] == "0") i <<- i + 1L else while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    isint <- TRUE
    if (i <= n && ch[i] == ".") {
      isint <- FALSE
      i <<- i + 1L
      if (i > n || !grepl("[0-9]", ch[i])) bad("invalid number")
      while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    }
    if (i <= n && (ch[i] == "e" || ch[i] == "E")) {
      isint <- FALSE
      i <<- i + 1L
      if (i <= n && (ch[i] == "+" || ch[i] == "-")) i <<- i + 1L
      if (i > n || !grepl("[0-9]", ch[i])) bad("invalid number")
      while (i <= n && grepl("[0-9]", ch[i])) i <<- i + 1L
    }
    t0 <- paste(ch[st:(i - 1L)], collapse = "")
    v <- as.numeric(t0)
    if (isint) {
      digs <- sub("^-", "", t0)
      big <- nchar(digs) > 16L || (nchar(digs) == 16L && digs > "9007199254740992")
      if (bigint_as_char && big) return(t0)
      if (v > 2147483647 || v < -2147483647) return(v)
      return(as.integer(v))
    }
    v
  }
  value <- function() {
    ws()
    if (i > n) bad("unexpected end of input")
    c0 <- ch[i]
    if (c0 == "{") {
      i <<- i + 1L
      ws()
      keys <- character(0)
      vals <- list()
      if (i <= n && ch[i] == "}") {
        i <<- i + 1L
        return(structure(list(), names = character(0)))
      }
      repeat {
        ws()
        if (i > n || ch[i] != "\"") bad("expected a string key")
        k <- str_()
        ws()
        if (i > n || ch[i] != ":") bad("expected ':'")
        i <<- i + 1L
        vals[[length(vals) + 1L]] <- list(value())
        keys <- c(keys, k)
        ws()
        if (i <= n && ch[i] == ",") {
          i <<- i + 1L
          next
        }
        if (i <= n && ch[i] == "}") {
          i <<- i + 1L
          break
        }
        bad("expected ',' or '}'")
      }
      out <- lapply(vals, function(w) w[[1L]])
      names(out) <- keys
      return(out)
    }
    if (c0 == "[") {
      i <<- i + 1L
      ws()
      vals <- list()
      if (i <= n && ch[i] == "]") {
        i <<- i + 1L
        return(list())
      }
      repeat {
        vals[[length(vals) + 1L]] <- list(value())
        ws()
        if (i <= n && ch[i] == ",") {
          i <<- i + 1L
          next
        }
        if (i <= n && ch[i] == "]") {
          i <<- i + 1L
          break
        }
        bad("expected ',' or ']'")
      }
      return(lapply(vals, function(w) w[[1L]]))
    }
    if (c0 == "\"") return(str_())
    if (c0 == "t" && identical(substr(s, i, i + 3L), "true")) {
      i <<- i + 4L
      return(TRUE)
    }
    if (c0 == "f" && identical(substr(s, i, i + 4L), "false")) {
      i <<- i + 5L
      return(FALSE)
    }
    if (c0 == "n" && identical(substr(s, i, i + 3L), "null")) {
      i <<- i + 4L
      return(NULL)
    }
    if (c0 == "-" || grepl("[0-9]", c0)) return(num_())
    bad("unexpected character")
  }
  v <- value()
  ws()
  if (i <= n) bad("trailing content")
  v
}

# ================================================================ simplify

#' @noRd
.jsonlt_is_namedlist <- function(x) isTRUE(is.list(x) && !is.null(names(x)))
#' @noRd
.jsonlt_is_recordlist <- function(x) {
  if (!(is.list(x) && is.null(names(x)) && length(x))) return(FALSE)
  one <- FALSE
  for (el in x) {
    if (!(.jsonlt_is_namedlist(el) || is.null(el))) return(FALSE)
    if (!one && .jsonlt_is_namedlist(el)) one <- TRUE
  }
  one
}
#' @noRd
.jsonlt_is_scalarlist <- function(x) {
  if (!is.list(x)) return(FALSE)
  for (el in x) if (!(is.null(el) || (is.atomic(el) && length(el) < 2L))) return(FALSE)
  TRUE
}
#' @noRd
.jsonlt_null_to_na <- function(x) {
  n <- length(x)
  if (!n) return(x)
  looks_chr <- FALSE
  for (i in seq_len(n)) {
    el <- x[[i]]
    if (is.null(el)) x[i] <- list(NA)
    else if (!looks_chr && is.character(el) && !(el[1L] %in% c("NA", "NaN", "Inf", "-Inf"))) looks_chr <- TRUE
  }
  if (looks_chr) return(x)
  for (i in seq_len(n)) {
    el <- x[[i]]
    if (is.character(el)) x[[i]] <- switch(el[1L], "NA" = NA, "NaN" = NaN, "Inf" = Inf, "-Inf" = -Inf, el)
  }
  x
}
#' @noRd
.jsonlt_is_datelist <- function(x) {
  if (!is.list(x) || !length(x)) return(FALSE)
  status <- FALSE
  for (el in x) {
    if (is.null(el)) next
    if (is.character(el) && length(el) && el[1L] == "NA") next
    if (is.numeric(el) && inherits(el, "POSIXct")) status <- TRUE else return(FALSE)
  }
  status
}
#' @noRd
.jsonlt_list_to_vec <- function(x) {
  isdates <- .jsonlt_is_datelist(x)
  out <- unlist(.jsonlt_null_to_na(x), recursive = FALSE, use.names = FALSE)
  if (isdates && is.numeric(out)) structure(out, class = c("POSIXct", "POSIXt")) else out
}
#' @noRd
.jsonlt_all_identical <- function(x) length(x) > 0L && all(x == x[1L])
#' @noRd
.jsonlt_parse_date <- function(x) {
  if (is.numeric(x)) return(structure(x / 1000, class = c("POSIXct", "POSIXt")))
  if (is.character(x)) {
    tz <- if (all(grepl("Z$", x))) "UTC" else ""
    return(as.POSIXct(strptime(x, format = "%Y-%m-%dT%H:%M:%OS", tz = tz)))
  }
  x
}

#' @noRd
.jsonlt_simplify <- function(x, simplifyVector = TRUE, simplifyDataFrame = TRUE,
                             simplifyMatrix = TRUE, simplifyDate = simplifyVector,
                             homoList = TRUE, flatten = FALSE, columnmajor = FALSE,
                             simplifySubMatrix = simplifyMatrix) {
  if (!is.list(x) || !length(x)) return(x)
  if (isTRUE(simplifyDataFrame) && .jsonlt_is_recordlist(x)) {
    mydf <- .jsonlt_simplify_df(x, flatten = flatten, simplifyMatrix = simplifySubMatrix)
    if (isTRUE(simplifyDate) && is.data.frame(mydf) && identical(names(mydf), "$date") &&
        (is.numeric(mydf[["$date"]]) || is.character(mydf[["$date"]])))
      return(.jsonlt_parse_date(mydf[["$date"]]))
    return(mydf)
  }
  if (isTRUE(simplifyVector) && is.null(names(x)) && .jsonlt_is_scalarlist(x))
    return(.jsonlt_list_to_vec(x))
  out <- lapply(x, .jsonlt_simplify, simplifyVector = simplifyVector,
                simplifyDataFrame = simplifyDataFrame, simplifyMatrix = simplifySubMatrix,
                columnmajor = columnmajor, flatten = flatten)
  if (isTRUE(simplifyVector) && .jsonlt_is_scalarlist(out) && length(out) &&
      all(vapply(out, inherits, logical(1), "POSIXt")))
    return(structure(.jsonlt_list_to_vec(out), class = c("POSIXct", "POSIXt")))
  if (isTRUE(simplifyMatrix) && isTRUE(simplifyVector) && .jsonlt_is_matrixlist(out) &&
      all(vapply(x, .jsonlt_is_scalarlist, logical(1)))) {
    return(if (isTRUE(columnmajor)) do.call(cbind, out) else do.call(rbind, out))
  }
  if (isTRUE(simplifyMatrix) && .jsonlt_is_arraylist(out)) {
    if (isTRUE(columnmajor))
      return(array(do.call(cbind, out), dim = c(dim(out[[1L]]), length(out))))
    return(array(do.call(rbind, lapply(out, as.vector)), dim = c(length(out), dim(out[[1L]]))))
  }
  if (isTRUE(homoList) && is.null(names(out))) {
    isempty <- vapply(out, identical, logical(1), list())
    if (any(isempty) && !all(isempty)) {
      if (all(vapply(out[!isempty], is.data.frame, logical(1)))) {
        for (i in which(isempty)) out[[i]] <- data.frame()
        return(out)
      }
      if (all(vapply(out[!isempty], function(z) isTRUE(is.vector(z) && is.atomic(z)), logical(1)))) {
        for (i in which(isempty)) out[[i]] <- vector(mode = typeof(out[[which(!isempty)[1L]]]))
        return(out)
      }
    }
  }
  if (isTRUE(simplifyDate) && is.list(out) && identical(names(out), "$date") &&
      (is.numeric(out[["$date"]]) || is.character(out[["$date"]])))
    return(.jsonlt_parse_date(out[["$date"]]))
  out
}
#' @noRd
.jsonlt_is_matrixlist <- function(x) {
  isTRUE(is.list(x) && length(x) && is.null(names(x)) &&
         all(vapply(x, is.atomic, logical(1))) &&
         .jsonlt_all_identical(vapply(x, length, integer(1))))
}
#' @noRd
.jsonlt_is_arraylist <- function(x) {
  isTRUE(is.list(x) && length(x) && is.null(names(x)) &&
         all(vapply(x, is.array, logical(1))) &&
         .jsonlt_all_identical(vapply(x, function(y) paste(dim(y), collapse = "-"), character(1))))
}
#' @noRd
.jsonlt_simplify_df <- function(recordlist, columns, flatten, simplifyMatrix) {
  if (!length(recordlist)) {
    if (!missing(columns))
      return(as.data.frame(matrix(ncol = length(columns), nrow = 0, dimnames = list(NULL, columns))))
    return(data.frame())
  }
  if (!any(vapply(recordlist, length, integer(1), USE.NAMES = FALSE)) && missing(columns))
    return(data.frame(matrix(nrow = length(recordlist), ncol = 0)))
  if (missing(columns))
    columns <- unique(unlist(lapply(recordlist, names), recursive = FALSE, use.names = FALSE))
  columnlist <- lapply(columns, function(nm) {
    lapply(recordlist, function(rec) {
      k <- match(nm, names(rec))
      if (is.na(k)) NULL else rec[[k]]
    })
  })
  columnlist <- lapply(columnlist, .jsonlt_simplify, simplifyVector = TRUE, simplifyDataFrame = TRUE,
                       simplifyMatrix = FALSE, simplifySubMatrix = simplifyMatrix, flatten = flatten)
  lens <- vapply(columnlist, function(z) if (length(dim(z)) > 1L) nrow(z) else length(z), integer(1))
  n <- unique(lens)
  if (length(n) > 1L) stop("Elements not of equal length: ", paste(lens, collapse = " "), call. = FALSE)
  names(columnlist) <- columns
  if (isTRUE(flatten)) {
    dfcols <- vapply(columnlist, is.data.frame, logical(1))
    if (any(dfcols)) columnlist <- c(columnlist[!dfcols], do.call(c, columnlist[dfcols]))
  }
  class(columnlist) <- "data.frame"
  if ("_row" %in% names(columnlist)) {
    rn <- columnlist[["_row"]]
    columnlist["_row"] <- NULL
    if (is.double(rn)) rn <- as.integer(rn)
    rn_na <- is.na(rn)
    if (sum(rn_na) > 0L) rn[rn_na] <- paste0("NA_", seq_len(sum(rn_na)))
    if (any(duplicated(rn))) {
      warning('Duplicate names in "_row" field. Data frames must have unique row names.', call. = FALSE)
      if (is.character(rn)) {
        row.names(columnlist) <- make.unique(rn)
      } else {
        row.names(columnlist) <- seq_len(n)
      }
    } else {
      row.names(columnlist) <- rn
    }
  } else {
    row.names(columnlist) <- seq_len(n)
  }
  columnlist
}

#' Parse JSON into R objects (jsonlite's fromJSON, natively)
#'
#' @param txt JSON text, a file path, or an http(s) URL.
#' @param simplifyVector,simplifyDataFrame,simplifyMatrix,flatten as in jsonlite.
#' @param bigint_as_char integers beyond 2^53 come back as strings.
#' @param simplify legacy: `FALSE` turns every simplification off.
#' @param ... ignored, for call compatibility.
#' @return an R object.
#' @examples
#' morie_jsonlt_from_json('[{"a":1,"b":"x"},{"a":2,"b":"y"}]')
#' morie_jsonlt_from_json('[[1,2],[3,4]]')
#' @export
morie_jsonlt_from_json <- function(txt, simplifyVector = TRUE,
                                   simplifyDataFrame = simplifyVector,
                                   simplifyMatrix = simplifyVector,
                                   flatten = FALSE, bigint_as_char = FALSE,
                                   simplify = NULL, ...) {
  if (identical(simplify, FALSE)) simplifyVector <- simplifyDataFrame <- simplifyMatrix <- FALSE
  if (!is.character(txt) && !inherits(txt, "connection"))
    stop("Argument 'txt' must be a JSON string, URL or file.", call. = FALSE)
  if (is.character(txt) && length(txt) == 1L && nchar(txt, type = "bytes") < 2084 &&
      !morie_jsonlt_validate(txt)) {
    if (grepl("^https?://", txt, useBytes = TRUE)) {
      txt <- base::url(txt, headers = c(Accept = "application/json, text/*, */*"))
    } else if (file.exists(txt)) {
      txt <- file(txt)
    }
  }
  if (inherits(txt, "connection")) {
    con <- txt
    if (!isOpen(con)) {
      open(con, "rb")
      on.exit(close(con), add = TRUE)
    }
    txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
  }
  obj <- .jsonlt_parse(txt, bigint_as_char)
  if (any(isTRUE(simplifyVector), isTRUE(simplifyDataFrame), isTRUE(simplifyMatrix)))
    return(.jsonlt_simplify(obj, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,
                            simplifyMatrix = simplifyMatrix, flatten = flatten))
  obj
}

#' @rdname morie_jsonlt_from_json
#' @param json JSON text.
#' @export
morie_jsonlt_parse_json <- function(json, simplifyVector = FALSE, ...) {
  morie_jsonlt_from_json(json, simplifyVector = simplifyVector, ...)
}

#' Read and write JSON files
#'
#' @param path file path.
#' @param simplifyVector as in jsonlite (`FALSE` by default for files).
#' @param ... options of [morie_jsonlt_from_json()] / [morie_jsonlt_to_json()].
#' @return `read_json`: the parsed object; `write_json`: `path`, invisibly.
#' @export
morie_jsonlt_read_json <- function(path, simplifyVector = FALSE, ...) {
  morie_jsonlt_from_json(file(path), simplifyVector = simplifyVector, ...)
}
#' @rdname morie_jsonlt_read_json
#' @param x object to write.
#' @export
morie_jsonlt_write_json <- function(x, path, ...) {
  json <- morie_jsonlt_to_json(x, ...)
  writeLines(json, path, useBytes = TRUE)
  invisible(path)
}

#' Validate JSON text
#'
#' @param txt character; lines are joined with newlines.
#' @return `TRUE`, or `FALSE` with attributes `err` and `offset`.
#' @export
morie_jsonlt_validate <- function(txt) {
  stopifnot(is.character(txt))
  txt <- paste(txt, collapse = "\n")
  if (startsWith(txt, "\ufeff"))
    return(structure(FALSE, err = "JSON string contains UTF8 byte-order-mark."))
  res <- tryCatch({
    .jsonlt_parse(txt)
    TRUE
  }, error = function(e) e)
  if (isTRUE(res)) return(TRUE)
  off <- suppressWarnings(as.integer(sub(".* at character ([0-9]+)$", "\\1", conditionMessage(res))))
  structure(FALSE, offset = if (is.na(off)) NA_integer_ else off, err = conditionMessage(res))
}

# ================================================================ text tools

# Tokenizer + yajl-style generator: strings are decoded and re-encoded (so
# "e" comes out as the character, "\/" as "/"), numbers are copied
# verbatim, and the beautifier follows yajl_gen exactly (newline after
# every opener, ",\n" between items, ": " after keys, final newline).
#' @noRd
.jsonlt_reformat <- function(txt, pretty, indent_string = "    ") {
  s <- paste(txt, collapse = "\n")
  if (startsWith(s, "\ufeff")) s <- substring(s, 2L)
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(ch)
  i <- 1L
  out <- character(0)
  depth <- 0L
  state <- "start"        # start | array_start | in_array | map_start | map_key | map_val | complete
  stack <- character(0)
  bad <- function(msg) stop(sprintf("%s at character %d", msg, i), call. = FALSE)
  emit <- function(t) out <<- c(out, t)
  need_sep <- FALSE
  sep <- function() {
    if (state == "map_key" || state == "in_array") {
      if (need_sep) bad("expected ',' between values")
      emit(",")
      if (pretty) emit("\n")
    } else if (state == "map_val") {
      emit(":")
      if (pretty) emit(" ")
    }
  }
  wsp <- function() if (pretty && state != "map_val" && depth > 0L) emit(strrep(indent_string, depth))
  atom_done <- function() {
    state <<- switch(state, start = "complete", map_start = "map_val", map_key = "map_val",
                     array_start = "in_array", map_val = "map_key", state)
    need_sep <<- state %in% c("in_array", "map_key")
  }
  skip_ws <- function() while (i <= n && (ch[i] == " " || ch[i] == "\t" || ch[i] == "\n" || ch[i] == "\r")) i <<- i + 1L
  read_string <- function() {
    j <- i + 1L
    parts <- character(0)
    st <- j
    repeat {
      if (j > n) bad("unterminated string")
      c0 <- ch[j]
      if (c0 == "\"") break
      if (c0 == "\\") {
        if (j > st) parts <- c(parts, paste(ch[st:(j - 1L)], collapse = ""))
        j <- j + 1L
        e <- ch[j]
        if (e == "u") {
          cp <- strtoi(paste(ch[(j + 1L):(j + 4L)], collapse = ""), 16L)
          if (is.na(cp)) bad("invalid \\u escape")
          j <- j + 4L
          if (cp >= 0xD800 && cp <= 0xDBFF && j + 6L <= n && ch[j + 1L] == "\\" && ch[j + 2L] == "u") {
            lo <- strtoi(paste(ch[(j + 3L):(j + 6L)], collapse = ""), 16L)
            if (!is.na(lo) && lo >= 0xDC00 && lo <= 0xDFFF) {
              j <- j + 6L
              cp <- 0x10000 + (cp - 0xD800) * 1024 + (lo - 0xDC00)
            }
          }
          parts <- c(parts, intToUtf8(cp))
        } else {
          parts <- c(parts, switch(e, n = "\n", t = "\t", r = "\r", b = "\b", f = "\f", "\"" = "\"", "\\" = "\\", "/" = "/", bad("invalid escape")))
        }
        j <- j + 1L
        st <- j
        next
      }
      j <- j + 1L
    }
    if (j > st) parts <- c(parts, paste(ch[st:(j - 1L)], collapse = ""))
    i <<- j + 1L
    paste(parts, collapse = "")
  }
  encode_string <- function(v) {
    v <- gsub("\\", "\\\\", v, fixed = TRUE)
    v <- gsub("\"", "\\\"", v, fixed = TRUE)
    v <- gsub("\n", "\\n", v, fixed = TRUE)
    v <- gsub("\r", "\\r", v, fixed = TRUE)
    v <- gsub("\t", "\\t", v, fixed = TRUE)
    v <- gsub("\b", "\\b", v, fixed = TRUE)
    v <- gsub("\f", "\\f", v, fixed = TRUE)
    v <- gsub("</", "<\\/", v, fixed = TRUE)
    for (cc in c(1:7, 11L, 14:31)) {
      c1 <- rawToChar(as.raw(cc))
      if (grepl(c1, v, fixed = TRUE, useBytes = TRUE)) v <- gsub(c1, sprintf("\\u%04X", cc), v, fixed = TRUE, useBytes = TRUE)
    }
    paste0("\"", v, "\"")
  }
  read_number <- function() {
    st <- i
    if (ch[i] == "-") i <<- i + 1L
    if (i > n || !grepl("[0-9]", ch[i])) bad("invalid number")
    while (i <= n && grepl("[0-9.eE+-]", ch[i])) i <<- i + 1L
    paste(ch[st:(i - 1L)], collapse = "")
  }
  while (TRUE) {
    skip_ws()
    if (i > n) break
    if (state == "complete") bad("trailing content")
    c0 <- ch[i]
    if (c0 == "{" || c0 == "[") {
      if (state %in% c("map_key", "map_start")) bad("keys must be strings")
      sep()
      wsp()
      depth <- depth + 1L
      stack <- c(stack, state)
      state <- if (c0 == "{") "map_start" else "array_start"
      emit(c0)
      if (pretty) emit("\n")
      i <- i + 1L
      next
    }
    if (c0 == "}" || c0 == "]") {
      if ((c0 == "}" && !(state %in% c("map_start", "map_key"))) || (c0 == "]" && !(state %in% c("array_start", "in_array")))) bad("unexpected closing bracket")
      depth <- depth - 1L
      if (pretty) emit("\n")
      state <- stack[length(stack)]
      stack <- stack[-length(stack)]
      atom_done()
      wsp()
      emit(c0)
      if (pretty && state == "complete") emit("\n")
      i <- i + 1L
      next
    }
    if (c0 == ",") {
      if (!(state %in% c("in_array", "map_key"))) bad("unexpected ','")
      need_sep <- FALSE
      i <- i + 1L
      skip_ws()
      if (i > n || ch[i] %in% c("]", "}")) bad("trailing comma")
      next
    }
    if (c0 == ":") {
      if (state != "map_val") bad("unexpected ':'")
      i <- i + 1L
      next
    }
    if (c0 == "\"") {
      v <- read_string()
      if (state %in% c("map_start", "map_key")) {
        # key
        if (state == "map_key") {
          if (need_sep) bad("expected ',' between members")
          emit(",")
          if (pretty) emit("\n")
        }
        wsp()
        emit(encode_string(v))
        state <- "map_val"
        skip_ws()
        if (i > n || ch[i] != ":") bad("expected ':'")
        next
      }
      sep()
      wsp()
      emit(encode_string(v))
      atom_done()
      if (pretty && state == "complete") emit("\n")
      next
    }
    if (state %in% c("map_key", "map_start")) bad("keys must be strings")
    if (c0 == "t" && identical(substr(s, i, i + 3L), "true")) {
      i <- i + 4L
      tok <- "true"
    } else if (c0 == "f" && identical(substr(s, i, i + 4L), "false")) {
      i <- i + 5L
      tok <- "false"
    } else if (c0 == "n" && identical(substr(s, i, i + 3L), "null")) {
      i <- i + 4L
      tok <- "null"
    } else if (c0 == "-" || grepl("[0-9]", c0)) {
      tok <- read_number()
    } else {
      bad("unexpected character")
    }
    sep()
    wsp()
    emit(tok)
    atom_done()
    if (pretty && state == "complete") emit("\n")
  }
  if (state != "complete") bad("unexpected end of input")
  structure(paste(out, collapse = ""), class = "json")
}

#' Indent JSON text (jsonlite's prettify)
#'
#' @param txt JSON text.
#' @param indent spaces per level (negative = tabs).
#' @return the same JSON, indented, with a trailing newline.
#' @export
morie_jsonlt_prettify <- function(txt, indent = 4) {
  stopifnot(is.numeric(indent))
  indent_string <- strrep(if (indent > 0) " " else "\t", as.integer(abs(indent)))
  .jsonlt_reformat(txt, TRUE, indent_string)
}

#' Strip whitespace from JSON text (jsonlite's minify)
#'
#' @param txt JSON text.
#' @return the same JSON with no whitespace outside strings.
#' @export
morie_jsonlt_minify <- function(txt) .jsonlt_reformat(txt, FALSE)

#' Expand nested data.frame columns (jsonlite's flatten)
#'
#' @param x a data.frame.
#' @param recursive expand nested frames all the way down.
#' @return a data.frame whose nested columns became outer.inner columns.
#' @export
morie_jsonlt_flatten <- function(x, recursive = TRUE) {
  stopifnot(is.data.frame(x))
  nr <- nrow(x)
  dfcols <- vapply(x, is.data.frame, logical(1))
  if (!any(dfcols)) return(x)
  x <- if (recursive) c(x[!dfcols], do.call(c, lapply(x[dfcols], morie_jsonlt_flatten)))
       else c(x[!dfcols], do.call(c, x[dfcols]))
  class(x) <- "data.frame"
  row.names(x) <- if (!nr) character(0) else 1:nr
  x
}

# ================================================================ streaming

#' Stream newline-delimited JSON (jsonlite's stream_in / stream_out)
#'
#' @param con a connection (opened in binary mode if not already open).
#' @param handler optional function called on each simplified page.
#' @param pagesize records per page.
#' @param verbose print progress.
#' @param ... passed to the simplifier / encoder.
#' @return `stream_in`: a data.frame of all records (or nothing with a
#'   handler); `stream_out`: invisible.
#' @export
morie_jsonlt_stream_in <- function(con, handler = NULL, pagesize = 500, verbose = TRUE, ...) {
  if (!inherits(con, "connection")) stop("Argument 'con' must be a connection.", call. = FALSE)
  count <- 0
  pages <- list()
  cb <- if (is.null(handler)) {
    function(x) {
      if (length(x)) {
        count <<- count + length(x)
        pages[[length(pages) + 1L]] <<- x
      }
    }
  } else {
    if (verbose) message("using a custom handler function.")
    function(x) {
      handler(.jsonlt_post_process(x, ...))
      count <<- count + length(x)
    }
  }
  if (!isOpen(con, "r")) {
    if (verbose) message("opening ", class(con)[1L], " input connection.")
    open(con, "rb")
    on.exit({
      if (verbose) message("closing ", class(con)[1L], " input connection.")
      close(con)
    }, add = TRUE)
  }
  repeat {
    page <- readLines(con, n = pagesize, encoding = "UTF-8")
    if (length(page)) {
      cleanpage <- Filter(nchar, page)
      cb(lapply(cleanpage, .jsonlt_parse))
      if (verbose) cat("\r Found", count, "records...")
    }
    if (length(page) < pagesize) break
  }
  if (is.null(handler)) {
    if (verbose) cat("\r Imported", count, "records. Simplifying...\n")
    .jsonlt_post_process(unlist(pages, FALSE, FALSE), ...)
  } else {
    invisible()
  }
}
#' @noRd
.jsonlt_post_process <- function(x, simplifyVector = TRUE, simplifyDataFrame = simplifyVector,
                                 simplifyMatrix = simplifyVector, flatten = FALSE) {
  out <- .jsonlt_simplify(x, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame,
                          simplifyMatrix = simplifyMatrix, flatten = flatten)
  if (isTRUE(simplifyDataFrame)) as.data.frame(out) else out
}
#' @rdname morie_jsonlt_stream_in
#' @param x a data.frame to write, one record per line.
#' @param prefix text prepended to every line (e.g. `""` for RFC 7464).
#' @export
morie_jsonlt_stream_out <- function(x, con = stdout(), pagesize = 500, verbose = TRUE, prefix = "", ...) {
  if (!inherits(con, "connection")) stop("Argument 'con' must be a connection.", call. = FALSE)
  stopifnot(is.data.frame(x))
  if (!isOpen(con, "w")) {
    if (verbose) message("opening ", class(con)[1L], " output connection.")
    open(con, "wb")
    on.exit({
      if (verbose) message("closing ", class(con)[1L], " output connection.")
      close(con)
    }, add = TRUE)
  }
  nr <- nrow(x)
  npages <- nr %/% pagesize
  lastpage <- nr %% pagesize
  write_page <- function(page) {
    str <- .jsonlt_page_lines(page, ...)
    if (is.character(prefix) && length(prefix) && nchar(prefix)) str <- paste0(prefix[1L], str)
    writeLines(enc2utf8(str), con = con, useBytes = TRUE)
  }
  for (i in seq_len(npages)) {
    write_page(x[(pagesize * (i - 1) + 1):(pagesize * i), , drop = FALSE])
    if (verbose) cat("\rProcessed", i * pagesize, "rows...")
  }
  if (lastpage) write_page(x[(nr - lastpage + 1):nr, , drop = FALSE])
  if (verbose) cat("\rComplete! Processed total of", nr, "rows.\n")
  invisible()
}
#' @noRd
.jsonlt_page_lines <- function(page, ...) {
  dots <- list(...)
  dots$pretty <- NULL
  # each row encodes as "[{...}]"; the record is what sits inside the brackets
  rows <- vapply(seq_len(nrow(page)), function(i) {
    as.character(do.call(morie_jsonlt_to_json, c(list(page[i, , drop = FALSE]), dots)))
  }, character(1))
  substr(rows, 2L, nchar(rows) - 1L)
}

#' Row-bind a list of data.frames with differing columns (jsonlite's rbind_pages)
#'
#' @param pages a list of data.frames (NULL entries are dropped).
#' @return one data.frame; missing columns are filled with NA.
#' @export
morie_jsonlt_rbind_pages <- function(pages) {
  stopifnot(is.list(pages))
  pages <- Filter(Negate(is.null), pages)
  stopifnot(all(vapply(pages, is.data.frame, logical(1))))
  if (!length(pages)) return(data.frame())
  cols <- unique(unlist(lapply(pages, names)))
  filled <- lapply(pages, function(p) {
    for (nm in setdiff(cols, names(p))) p[[nm]] <- rep(NA, nrow(p))
    p[cols]
  })
  out <- do.call(rbind, c(filled, list(make.row.names = FALSE, stringsAsFactors = FALSE)))
  row.names(out) <- NULL
  out
}

# ================================================================ serialize

#' @noRd
.jsonlt_pack <- function(obj) {
  mode <- typeof(obj)
  if (mode == "NULL") return(list(type = morie_jsonlt_unbox("NULL")))
  if (mode == "closure") obj <- as.list(obj)
  if (mode == "environment" && isNamespace(obj)) mode <- "namespace"
  attrib <- attributes(obj)
  if (isS4(obj)) {
    mode <- "S4"
    sn <- methods::slotNames(obj)
    if (".Data" %in% sn) attrib[[".Data"]] <- obj@.Data
    attrib <- attrib[sn]
    names(attrib) <- sn
  }
  value <- switch(mode,
    environment = NULL, externalptr = NULL,
    namespace = lapply(as.list(getNamespaceInfo(obj, "spec")), morie_jsonlt_unbox),
    S4 = list(class = morie_jsonlt_unbox(class(obj)), package = morie_jsonlt_unbox(attr(class(obj), "package"))),
    raw = morie_jsonlt_unbox(morie_jsonlt_base64_enc(unclass(obj))),
    logical = as.vector(unclass(obj), mode = "logical"),
    integer = as.vector(unclass(obj), mode = "integer"),
    numeric = , double = as.vector(unclass(obj), mode = "double"),
    character = as.vector(unclass(obj), mode = "character"),
    complex = as.vector(unclass(obj), mode = "complex"),
    list = unname(lapply(unclass(obj), .jsonlt_pack)),
    pairlist = unname(lapply(as.vector(obj, mode = "list"), .jsonlt_pack)),
    closure = unname(lapply(obj, .jsonlt_pack)),
    builtin = , special = morie_jsonlt_unbox(morie_jsonlt_base64_enc(serialize(unclass(obj), NULL))),
    language = , name = , symbol = deparse(unclass(obj)),
    expression = deparse(obj[[1L]]),
    {
      warning("No encoding has been defined for objects with storage mode ", mode, " and will be skipped.")
      NULL
    })
  list(type = morie_jsonlt_unbox(mode),
       attributes = structure(lapply(attrib, .jsonlt_pack), names = as.character(names(attrib))),
       value = value)
}
#' @noRd
.jsonlt_unpack <- function(obj) {
  mode <- obj$type
  if (identical(mode, "NULL")) return(NULL)
  if (identical(mode, "S4")) {
    data <- lapply(obj$attributes, .jsonlt_unpack)
    return(do.call(methods::new, c(Class = obj$value$class, data)))
  }
  vals <- obj$value
  newdata <- list(.Data = switch(mode,
    environment = new.env(parent = emptyenv()),
    namespace = getNamespace(obj$value$name),
    externalptr = NULL,
    raw = morie_jsonlt_base64_dec(vals),
    logical = as.logical(.jsonlt_list_to_vec(vals)),
    integer = as.integer(.jsonlt_list_to_vec(vals)),
    numeric = , double = as.double(.jsonlt_list_to_vec(vals)),
    character = as.character(.jsonlt_list_to_vec(vals)),
    complex = as.complex(.jsonlt_list_to_vec(vals)),
    list = , pairlist = , closure = lapply(vals, .jsonlt_unpack),
    symbol = , name = if (identical(unlist(vals), "")) quote(expr = ) else as.name(unlist(vals)),
    expression = parse(text = unlist(vals)),
    language = as.call(parse(text = unlist(vals)))[[1L]],
    special = , builtin = unserialize(morie_jsonlt_base64_dec(vals)),
    stop("Switch falling through for encode.mode: ", mode, call. = FALSE)))
  # a missing formal comes back as the empty symbol; it must not be evaluated
  if (identical(newdata[[1L]], substitute())) return(substitute())
  attrs <- lapply(obj$attributes, .jsonlt_unpack)
  output <- do.call("structure", c(newdata, attrs), quote = TRUE)
  if (mode == "closure") {
    f <- as.function(output)
    environment(f) <- globalenv()
    return(f)
  }
  if (mode == "pairlist") return(as.pairlist(output))
  output
}

#' Serialise an R object losslessly (jsonlite's serializeJSON)
#'
#' Type and attributes travel with the value, so the round trip returns the
#' same object rather than something that merely prints the same.
#'
#' @param x object to serialise.
#' @param digits decimal digits for doubles (8, as jsonlite).
#' @param pretty indent the output.
#' @return a length-one character vector of class `json`.
#' @export
morie_jsonlt_serialize <- function(x, digits = 8, pretty = FALSE) {
  morie_jsonlt_to_json(.jsonlt_pack(x), digits = digits, pretty = pretty)
}

#' Restore an object written by morie_jsonlt_serialize
#'
#' @param txt JSON produced by [morie_jsonlt_serialize()].
#' @return the original R object.
#' @export
morie_jsonlt_unserialize <- function(txt) .jsonlt_unpack(.jsonlt_parse(txt))

# ================================================================ route

#' jsonlite's JSON mapping, natively
#'
#' @param x the value or JSON text the route acts on.
#' @param route one of to_json, from_json, prettify, minify, validate,
#'   flatten, serialize, unserialize, base64_enc, base64_dec.
#' @param ... options for the chosen route.
#' @return a list with route, result and method.
#' @export
morie_jsonlt <- function(x = NULL, route = "to_json", ...) {
  routes <- c("to_json", "from_json", "prettify", "minify", "validate", "flatten",
              "serialize", "unserialize", "base64_enc", "base64_dec")
  if (!(length(route) == 1L && route %in% routes))
    stop(sprintf("jsonlt: route = %s; expected one of %s", route, paste(routes, collapse = ", ")), call. = FALSE)
  dots <- list(...)
  res <- switch(route,
    to_json = do.call(morie_jsonlt_to_json, c(list(x), dots)),
    from_json = do.call(morie_jsonlt_from_json, c(list(x), dots)),
    prettify = morie_jsonlt_prettify(x, if (is.null(dots$indent)) 4 else dots$indent),
    minify = morie_jsonlt_minify(x),
    validate = morie_jsonlt_validate(x),
    flatten = morie_jsonlt_flatten(x, if (is.null(dots$recursive)) TRUE else dots$recursive),
    serialize = do.call(morie_jsonlt_serialize, c(list(x), dots)),
    unserialize = morie_jsonlt_unserialize(x),
    base64_enc = morie_jsonlt_base64_enc(x),
    base64_dec = morie_jsonlt_base64_dec(x))
  list(route = route, result = res, method = "jsonlite 2.0.0 mapping (Ooms 2014), RFC 8259")
}
