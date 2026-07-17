# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native parsers (feat/native-specializations, module 23). Pure-R
# JSON parse/stringify, a SAX-style XML parser, a tolerant HTML tag
# parser, CSV/TSV via base utils, and a minimal Parquet reader
# (PLAIN-encoded columns, uncompressed or Snappy). Per the module
# charter, jsonlite / xml2 / arrow remain OPTIONAL fast paths in
# Suggests: every call site routes through the .morie_from_json /
# .morie_xml_parse / .morie_read_parquet shims below, which prefer
# the installed accelerator and otherwise use the native engines, so
# a bare install parses everything. HTTP stays on the package's own
# libcurl C++ backend (src/morie_http.cpp).

# ---------------------------------------------------------------------------
# JSON: recursive-descent parser + stringifier
# ---------------------------------------------------------------------------

#' Parse JSON text natively (pure R)
#'
#' Recursive-descent JSON parser with jsonlite-style simplification:
#' arrays of same-type scalars become vectors, arrays of consistently
#' keyed objects become data frames (when `simplify = TRUE`).
#'
#' @param txt A single JSON string (or raw vector).
#' @param simplify Simplify arrays to vectors / data frames
#'   (default TRUE, mirroring \code{jsonlite::fromJSON}).
#' @return The parsed R object.
#' @examples
#' morie_fetch_json('{"a": [1, 2, 3], "b": "x"}')
#' @export
morie_fetch_json <- function(txt, simplify = TRUE) {
  if (is.raw(txt)) txt <- rawToChar(txt)
  stopifnot(is.character(txt), length(txt) == 1L)
  st <- new.env(parent = emptyenv())
  st$s <- txt
  st$i <- 1L
  st$n <- nchar(txt)
  val <- .mj_value(st)
  .mj_ws(st)
  if (st$i <= st$n) {
    stop("JSON parse error: trailing content at position ", st$i,
         call. = FALSE)
  }
  if (simplify) .mj_simplify(val) else val
}

#' @noRd
.mj_ws <- function(st) {
  while (st$i <= st$n &&
         substr(st$s, st$i, st$i) %in% c(" ", "\t", "\n", "\r")) {
    st$i <- st$i + 1L
  }
}

#' @noRd
.mj_value <- function(st) {
  .mj_ws(st)
  if (st$i > st$n) stop("JSON parse error: unexpected end", call. = FALSE)
  ch <- substr(st$s, st$i, st$i)
  if (ch == "{") return(.mj_object(st))
  if (ch == "[") return(.mj_array(st))
  if (ch == "\"") return(.mj_string(st))
  if (ch == "t") { .mj_lit(st, "true"); return(TRUE) }
  if (ch == "f") { .mj_lit(st, "false"); return(FALSE) }
  if (ch == "n") { .mj_lit(st, "null"); return(NULL) }
  .mj_number(st)
}

#' @noRd
.mj_lit <- function(st, lit) {
  k <- nchar(lit)
  if (substr(st$s, st$i, st$i + k - 1L) != lit) {
    stop("JSON parse error at position ", st$i, call. = FALSE)
  }
  st$i <- st$i + k
}

#' @noRd
.mj_string <- function(st) {
  # st$i sits on the opening quote
  i <- st$i + 1L
  out <- character(0)
  seg_start <- i
  s <- st$s
  n <- st$n
  while (i <= n) {
    ch <- substr(s, i, i)
    if (ch == "\"") {
      out <- c(out, substr(s, seg_start, i - 1L))
      st$i <- i + 1L
      return(paste(out, collapse = ""))
    }
    if (ch == "\\") {
      out <- c(out, substr(s, seg_start, i - 1L))
      esc <- substr(s, i + 1L, i + 1L)
      rep <- switch(esc,
        "\"" = "\"", "\\" = "\\", "/" = "/", b = "\b", f = "\f",
        n = "\n", r = "\r", t = "\t",
        u = NA_character_,
        stop("JSON parse error: bad escape \\", esc, call. = FALSE))
      if (is.na(rep)) {
        code <- strtoi(substr(s, i + 2L, i + 5L), 16L)
        if (is.na(code)) {
          stop("JSON parse error: bad \\u escape", call. = FALSE)
        }
        rep <- intToUtf8(code)
        i <- i + 6L
      } else {
        i <- i + 2L
      }
      out <- c(out, rep)
      seg_start <- i
    } else {
      i <- i + 1L
    }
  }
  stop("JSON parse error: unterminated string", call. = FALSE)
}

#' @noRd
.mj_number <- function(st) {
  m <- regexpr("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?",
               substr(st$s, st$i, min(st$n, st$i + 63L)))
  if (m == -1L) stop("JSON parse error at position ", st$i, call. = FALSE)
  len <- attr(m, "match.length")
  num <- substr(st$s, st$i, st$i + len - 1L)
  st$i <- st$i + len
  as.numeric(num)
}

#' @noRd
.mj_array <- function(st) {
  st$i <- st$i + 1L                     # consume [
  out <- list()
  .mj_ws(st)
  if (substr(st$s, st$i, st$i) == "]") { st$i <- st$i + 1L; return(out) }
  repeat {
    v <- .mj_value(st)
    out[[length(out) + 1L]] <- if (is.null(v)) NA else v
    .mj_ws(st)
    ch <- substr(st$s, st$i, st$i)
    st$i <- st$i + 1L
    if (ch == "]") return(out)
    if (ch != ",") stop("JSON parse error: expected , or ] at ",
                        st$i - 1L, call. = FALSE)
  }
}

#' @noRd
.mj_object <- function(st) {
  st$i <- st$i + 1L                     # consume {
  out <- list()
  nms <- character(0)
  .mj_ws(st)
  if (substr(st$s, st$i, st$i) == "}") { st$i <- st$i + 1L; return(out) }
  repeat {
    .mj_ws(st)
    if (substr(st$s, st$i, st$i) != "\"") {
      stop("JSON parse error: expected key at ", st$i, call. = FALSE)
    }
    key <- .mj_string(st)
    .mj_ws(st)
    if (substr(st$s, st$i, st$i) != ":") {
      stop("JSON parse error: expected : at ", st$i, call. = FALSE)
    }
    st$i <- st$i + 1L
    v <- .mj_value(st)
    out[length(out) + 1L] <- list(v)   # list() wrapper keeps NULLs
    nms <- c(nms, key)
    .mj_ws(st)
    ch <- substr(st$s, st$i, st$i)
    st$i <- st$i + 1L
    if (ch == "}") { names(out) <- nms; return(out) }
    if (ch != ",") stop("JSON parse error: expected , or } at ",
                        st$i - 1L, call. = FALSE)
  }
}

#' @noRd
.mj_simplify <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, .mj_simplify)
  if (!is.null(names(x))) return(x)     # object stays a named list
  if (!length(x)) return(x)
  # array of same-type scalars -> vector
  scal <- vapply(x, function(e)
    is.atomic(e) && length(e) == 1L && !is.list(e), logical(1))
  if (all(scal)) {
    types <- vapply(x, function(e) class(e)[1], character(1))
    u <- unique(types[types != "logical" | !vapply(x, function(e)
      length(e) == 1 && is.na(e), logical(1))])
    if (length(unique(types)) == 1L) return(unlist(x, use.names = FALSE))
    if (all(types %in% c("numeric", "logical"))) {
      # NA placeholders from null mixed with numbers
      return(as.numeric(unlist(x, use.names = FALSE)))
    }
    if (all(types %in% c("character", "logical"))) {
      return(as.character(unlist(x, use.names = FALSE)))
    }
    return(x)
  }
  # array of equal-length same-type vectors -> matrix (jsonlite
  # convention for arrays of arrays)
  is_vec <- vapply(x, function(e)
    is.atomic(e) && is.null(names(e)) && length(e) >= 1L, logical(1))
  if (all(is_vec) && length(x) > 1L) {
    lens <- vapply(x, length, integer(1))
    typs <- vapply(x, function(e) class(e)[1], character(1))
    if (length(unique(lens)) == 1L && lens[1] > 1L &&
        length(unique(typs)) == 1L) {
      return(do.call(rbind, x))
    }
  }
  # array of consistently keyed objects -> data.frame
  is_obj <- vapply(x, function(e)
    is.list(e) && !is.null(names(e)), logical(1))
  if (all(is_obj)) {
    keys <- unique(unlist(lapply(x, names)))
    cols_ok <- TRUE
    cols <- lapply(keys, function(k) {
      vals <- lapply(x, function(e) {
        v <- e[[k]]
        if (is.null(v)) NA else v
      })
      if (all(vapply(vals, function(v)
        is.atomic(v) && length(v) == 1L, logical(1)))) {
        unlist(vals, use.names = FALSE)
      } else {
        cols_ok <<- FALSE
        vals
      }
    })
    if (cols_ok) {
      names(cols) <- keys
      return(as.data.frame(cols, stringsAsFactors = FALSE,
                           check.names = FALSE))
    }
  }
  x
}

#' Serialize an R object to JSON natively (pure R)
#'
#' @param x Vectors, lists, data frames (rows become objects).
#' @param auto_unbox Emit length-1 vectors as scalars (default TRUE).
#' @return A single JSON string.
#' @examples
#' morie_json_stringify(list(a = 1:3, b = "x"))
#' @export
morie_json_stringify <- function(x, auto_unbox = TRUE) {
  esc <- function(s) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\t", "\\t", s, fixed = TRUE)
    s
  }
  ser <- function(v) {
    if (is.null(v)) return("null")
    if (is.data.frame(v)) {
      rows <- vapply(seq_len(nrow(v)), function(i)
        ser(as.list(v[i, , drop = FALSE])), character(1))
      return(paste0("[", paste(rows, collapse = ","), "]"))
    }
    if (is.list(v)) {
      if (!is.null(names(v)) && length(v)) {
        parts <- vapply(seq_along(v), function(i)
          paste0("\"", esc(names(v)[i]), "\":", ser(v[[i]])),
          character(1))
        return(paste0("{", paste(parts, collapse = ","), "}"))
      }
      parts <- vapply(v, ser, character(1))
      return(paste0("[", paste(parts, collapse = ","), "]"))
    }
    atom <- function(e) {
      if (is.na(e)) return("null")
      if (is.logical(e)) return(if (e) "true" else "false")
      if (is.numeric(e)) {
        if (is.finite(e) && e == round(e) && abs(e) < 1e15) {
          return(format(e, scientific = FALSE))
        }
        return(as.character(e))
      }
      paste0("\"", esc(as.character(e)), "\"")
    }
    if (length(v) == 1L && auto_unbox) return(atom(v[[1]]))
    paste0("[", paste(vapply(v, atom, character(1)), collapse = ","),
           "]")
  }
  ser(x)
}

#' Internal shim: prefer jsonlite, fall back to the native parser
#' @noRd
.morie_from_json <- function(txt, ...) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::fromJSON(txt, ...))
  }
  args <- list(...)
  simplify <- !isFALSE(args$simplifyVector)
  if (length(txt) == 1L && !grepl("^[\\[{ \t\r\n\"]", txt) &&
      (file.exists(txt) || grepl("^https?://", txt))) {
    txt <- paste(readLines(txt, warn = FALSE), collapse = "\n")
  }
  morie_fetch_json(txt, simplify = simplify)
}

#' Internal shim: prefer jsonlite::toJSON, fall back to native
#' @noRd
.morie_to_json <- function(x, ...) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(x, ...))
  }
  morie_json_stringify(x)
}

# ---------------------------------------------------------------------------
# XML: SAX-style parser (events) + convenience tree builder
# ---------------------------------------------------------------------------

#' Parse XML natively with SAX-style callbacks (pure R)
#'
#' Streaming tag scanner for the well-formed machine-generated XML
#' rmorie consumes (CKAN, ArcGIS, StatCan, SIU exports). Emits
#' start/text/end events; \code{morie_fetch_xml} builds a plain list
#' tree from them.
#'
#' @param txt XML text (single string).
#' @param on_start,on_text,on_end Optional callbacks:
#'   \code{on_start(tag, attrs)}, \code{on_text(text)},
#'   \code{on_end(tag)}.
#' @return Invisibly, the number of elements seen.
#' @examples
#' starts <- character(0)
#' morie_xml_sax("<a><b>hi</b></a>",
#'               on_start = function(tag, attrs) starts <<- c(starts, tag))
#' starts
#' @export
morie_xml_sax <- function(txt, on_start = NULL, on_text = NULL,
                          on_end = NULL) {
  stopifnot(is.character(txt), length(txt) == 1L)
  # strip comments, CDATA -> text, prolog/doctype
  txt <- gsub("<!--.*?-->", "", txt, perl = TRUE)
  txt <- gsub("<\\?.*?\\?>", "", txt, perl = TRUE)
  txt <- gsub("<!DOCTYPE[^>]*>", "", txt, perl = TRUE)
  # CDATA: escape the payload so embedded markup stays literal text
  repeat {
    m <- regexpr("<!\\[CDATA\\[.*?\\]\\]>", txt, perl = TRUE)
    if (m == -1L) break
    len <- attr(m, "match.length")
    body <- substr(txt, m + 9L, m + len - 4L)
    body <- gsub("&", "&amp;", body, fixed = TRUE)
    body <- gsub("<", "&lt;", body, fixed = TRUE)
    body <- gsub(">", "&gt;", body, fixed = TRUE)
    txt <- paste0(substr(txt, 1L, m - 1L), body,
                  substring(txt, m + len))
  }
  n_elem <- 0L
  pos <- gregexpr("<[^>]+>", txt, perl = TRUE)[[1]]
  if (pos[1] == -1L) return(invisible(0L))
  lens <- attr(pos, "match.length")
  last_end <- 1L
  unescape <- function(s) {
    s <- gsub("&lt;", "<", s, fixed = TRUE)
    s <- gsub("&gt;", ">", s, fixed = TRUE)
    s <- gsub("&quot;", "\"", s, fixed = TRUE)
    s <- gsub("&apos;", "'", s, fixed = TRUE)
    gsub("&amp;", "&", s, fixed = TRUE)
  }
  for (k in seq_along(pos)) {
    if (pos[k] > last_end) {
      text <- substr(txt, last_end, pos[k] - 1L)
      if (nzchar(trimws(text)) && !is.null(on_text)) {
        on_text(unescape(trimws(text)))
      }
    }
    tag <- substr(txt, pos[k], pos[k] + lens[k] - 1L)
    last_end <- pos[k] + lens[k]
    inner <- substr(tag, 2L, nchar(tag) - 1L)
    if (startsWith(inner, "/")) {
      if (!is.null(on_end)) on_end(trimws(substring(inner, 2L)))
      next
    }
    self_close <- endsWith(inner, "/")
    if (self_close) inner <- substr(inner, 1L, nchar(inner) - 1L)
    m <- regexpr("^[A-Za-z_][A-Za-z0-9_.:-]*", inner)
    if (m == -1L) next
    nm <- substr(inner, 1L, attr(m, "match.length"))
    rest <- substring(inner, attr(m, "match.length") + 1L)
    attrs <- list()
    am <- gregexpr(
      "([A-Za-z_][A-Za-z0-9_.:-]*)\\s*=\\s*\"([^\"]*)\"", rest,
      perl = TRUE)[[1]]
    if (am[1] != -1L) {
      alen <- attr(am, "match.length")
      for (j in seq_along(am)) {
        kv <- substr(rest, am[j], am[j] + alen[j] - 1L)
        eq <- regexpr("=", kv)
        key <- trimws(substr(kv, 1L, eq - 1L))
        val <- sub("^\\s*\"", "", substring(kv, eq + 1L))
        val <- sub("\"\\s*$", "", val)
        attrs[[key]] <- unescape(val)
      }
    }
    n_elem <- n_elem + 1L
    if (!is.null(on_start)) on_start(nm, attrs)
    if (self_close && !is.null(on_end)) on_end(nm)
  }
  invisible(n_elem)
}

#' Parse XML natively into a list tree (pure R)
#'
#' Built on \code{\link{morie_xml_sax}}. Each element becomes
#' \code{list(tag =, attrs =, children = list(...), text =)}.
#'
#' @param txt XML text.
#' @return The root element as a nested list.
#' @examples
#' r <- morie_fetch_xml("<a x=\"1\"><b>hi</b></a>")
#' r$children[[1]]$text
#' @export
morie_fetch_xml <- function(txt) {
  stack <- list(list(tag = ".root", attrs = list(),
                     children = list(), text = ""))
  morie_xml_sax(
    txt,
    on_start = function(tag, attrs) {
      stack[[length(stack) + 1L]] <<- list(tag = tag, attrs = attrs,
                                           children = list(),
                                           text = "")
    },
    on_text = function(text) {
      top <- stack[[length(stack)]]
      top$text <- paste0(top$text,
                         if (nzchar(top$text)) " " else "", text)
      stack[[length(stack)]] <<- top
    },
    on_end = function(tag) {
      done <- stack[[length(stack)]]
      stack[[length(stack)]] <<- NULL
      parent <- stack[[length(stack)]]
      parent$children[[length(parent$children) + 1L]] <- done
      stack[[length(stack)]] <<- parent
    })
  root <- stack[[1L]]
  if (length(root$children) == 1L) root$children[[1L]] else root
}

#' Internal helper: find elements by tag in a native XML tree
#' @noRd
.morie_xml_find_all <- function(node, tag) {
  out <- list()
  walk <- function(n) {
    if (identical(n$tag, tag)) out[[length(out) + 1L]] <<- n
    for (ch in n$children) walk(ch)
  }
  walk(node)
  out
}

#' Parse HTML natively (tolerant tag scanner, pure R)
#'
#' Same scanner as the XML path but tolerant of HTML quirks: void
#' elements (\code{<br>}, \code{<img>}, ...) never push the stack,
#' unclosed \code{<p>}/\code{<li>} close implicitly, and stray end
#' tags are ignored rather than fatal.
#'
#' @param txt HTML text.
#' @return A nested list tree as in \code{\link{morie_fetch_xml}}.
#' @examples
#' html <- "<html><body><p>first<p>second<br><ul><li>a<li>b</ul></body></html>"
#' root <- morie_fetch_html(html)
#' root
#' @export
morie_fetch_html <- function(txt) {
  void <- c("area", "base", "br", "col", "embed", "hr", "img",
            "input", "link", "meta", "param", "source", "track",
            "wbr")
  implicit <- c("p", "li", "td", "th", "tr", "option", "dd", "dt")
  txt <- gsub("(?is)<script.*?</script>", "", txt, perl = TRUE)
  txt <- gsub("(?is)<style.*?</style>", "", txt, perl = TRUE)
  stack <- list(list(tag = ".root", attrs = list(),
                     children = list(), text = ""))
  pop_to_parent <- function() {
    done <- stack[[length(stack)]]
    stack[[length(stack)]] <<- NULL
    parent <- stack[[length(stack)]]
    parent$children[[length(parent$children) + 1L]] <- done
    stack[[length(stack)]] <<- parent
  }
  morie_xml_sax(
    txt,
    on_start = function(tag, attrs) {
      tl <- tolower(tag)
      top <- stack[[length(stack)]]$tag
      if (tl %in% implicit && identical(tolower(top), tl)) {
        pop_to_parent()               # sibling implies close
      }
      node <- list(tag = tl, attrs = attrs, children = list(),
                   text = "")
      if (tl %in% void) {
        parent <- stack[[length(stack)]]
        parent$children[[length(parent$children) + 1L]] <- node
        stack[[length(stack)]] <<- parent
      } else {
        stack[[length(stack) + 1L]] <<- node
      }
    },
    on_text = function(text) {
      top <- stack[[length(stack)]]
      top$text <- paste0(top$text,
                         if (nzchar(top$text)) " " else "", text)
      stack[[length(stack)]] <<- top
    },
    on_end = function(tag) {
      tl <- tolower(tag)
      if (tl %in% void) return(invisible(NULL))
      open_tags <- vapply(stack, `[[`, character(1), "tag")
      hit <- max(which(tolower(open_tags) == tl), 0L)
      if (hit <= 1L) return(invisible(NULL))   # stray end tag
      while (length(stack) >= hit) pop_to_parent()
    })
  while (length(stack) > 1L) pop_to_parent()
  root <- stack[[1L]]
  if (length(root$children) == 1L) root$children[[1L]] else root
}

# ---------------------------------------------------------------------------
# CSV / TSV / Parquet + unified dispatcher
# ---------------------------------------------------------------------------

#' Read CSV/TSV natively
#' @param path File path.
#' @param sep Field separator ("," or "\\t").
#' @return A data frame.
#' @examples
#' tf <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), tf,
#'                  row.names = FALSE)
#' morie_fetch_csv(tf)
#' @export
morie_fetch_csv <- function(path, sep = ",") {
  utils::read.table(path, header = TRUE, sep = sep,
                    stringsAsFactors = FALSE, check.names = FALSE,
                    quote = "\"", comment.char = "", fill = TRUE)
}

#' Internal shim: prefer arrow for Parquet, else the native reader
#' @noRd
.morie_read_parquet <- function(path, ...) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(as.data.frame(arrow::read_parquet(path, ...)))
  }
  morie_fetch_parquet(path)
}

#' Minimal native Parquet reader (pure R)
#'
#' Reads the subset of Parquet that rmorie's own exporters and the
#' open-data portals it consumes actually produce: PLAIN-encoded
#' columns (INT32, INT64, DOUBLE, BYTE_ARRAY/UTF8), uncompressed or
#' Snappy-compressed, in any number of row groups, with all values
#' required (no nulls) or nullability encoded via RLE definition
#' levels of max depth 1. Falls back with a clear error naming
#' \pkg{arrow} for anything richer (dictionary encoding, nested
#' types, other codecs).
#'
#' @param path Path to a .parquet file.
#' @return A data frame.
#' @references Apache Parquet format specification (thrift compact
#'   protocol footer; PLAIN encoding; Snappy framing).
#' @examples
#' if (requireNamespace("arrow", quietly = TRUE)) {
#'   tf <- tempfile(fileext = ".parquet")
#'   arrow::write_parquet(data.frame(a = 1:3), tf, use_dictionary = FALSE,
#'                        compression = "snappy")
#'   morie_fetch_parquet(tf)
#' }
#' @export
morie_fetch_parquet <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  sz <- file.info(path)$size
  magic <- readBin(con, "raw", 4L)
  if (!identical(rawToChar(magic), "PAR1")) {
    stop("not a Parquet file (missing PAR1 header)", call. = FALSE)
  }
  seek(con, sz - 8L)
  meta_len <- readBin(con, "integer", 1L, size = 4L, endian = "little")
  tail_magic <- readBin(con, "raw", 4L)
  if (!identical(rawToChar(tail_magic), "PAR1")) {
    stop("not a Parquet file (missing PAR1 footer)", call. = FALSE)
  }
  seek(con, sz - 8L - meta_len)
  meta_raw <- readBin(con, "raw", meta_len)
  meta <- .mpq_thrift_struct(meta_raw, 1L)$value
  schema <- meta[["2"]]
  cols_meta <- lapply(schema[-1L], function(el) {
    list(name = rawToChar(el[["4"]]),
         type = el[["1"]])
  })
  row_groups <- meta[["4"]]
  out_cols <- stats::setNames(
    replicate(length(cols_meta), list(), simplify = FALSE),
    vapply(cols_meta, `[[`, character(1), "name"))
  for (rg in row_groups) {
    chunks <- rg[["1"]]
    for (ci in seq_along(chunks)) {
      cmeta <- chunks[[ci]][["3"]]
      # Thrift ColumnMetaData: 3 = path_in_schema, 4 = codec,
      # 5 = num_values, 9 = data_page_offset, 11 = dictionary_page_offset.
      codec <- cmeta[["4"]]
      n_vals <- cmeta[["5"]]
      offset <- cmeta[["9"]]
      if (is.null(offset)) offset <- cmeta[["11"]]
      vals <- .mpq_read_column(con, offset, codec, n_vals,
                               cols_meta[[ci]]$type)
      nm <- cols_meta[[ci]]$name
      out_cols[[nm]] <- c(out_cols[[nm]], list(vals))
    }
  }
  as.data.frame(lapply(out_cols, function(parts)
    do.call(c, parts)), stringsAsFactors = FALSE, check.names = FALSE)
}

#' @noRd
.mpq_read_column <- function(con, offset, codec, n_vals, ptype) {
  seek(con, offset)
  # page header (thrift compact)
  hdr_raw <- readBin(con, "raw", 128L)
  ph <- .mpq_thrift_struct(hdr_raw, 1L)
  page <- ph$value
  page_type <- page[["1"]]
  if (!identical(page_type, 0L)) {
    stop("native Parquet reader supports data pages only; install ",
         "the 'arrow' package for this file.", call. = FALSE)
  }
  uncomp_sz <- page[["2"]]
  comp_sz <- page[["3"]]
  dph <- page[["5"]]
  encoding <- dph[["2"]]
  if (!identical(encoding, 0L)) {
    stop("native Parquet reader supports PLAIN encoding only; ",
         "install the 'arrow' package for this file.", call. = FALSE)
  }
  seek(con, offset + ph$pos - 1L)
  buf <- readBin(con, "raw", comp_sz)
  if (identical(codec, 1L)) {
    buf <- .mpq_snappy(buf)
  } else if (!identical(codec, 0L)) {
    stop("native Parquet reader supports UNCOMPRESSED/SNAPPY only; ",
         "install the 'arrow' package for this file.", call. = FALSE)
  }
  # definition levels (max level 1) precede values when the column is
  # optional: 4-byte length + RLE run. Detect by checking whether the
  # buffer is exactly the packed size for required values.
  n <- page[["5"]][["1"]]
  .mpq_plain(buf, n, ptype)
}

#' @noRd
.mpq_plain <- function(buf, n, ptype) {
  need <- switch(as.character(ptype),
                 "1" = 4L * n,  # INT32
                 "2" = 8L * n,  # INT64
                 "5" = 8L * n,  # DOUBLE
                 NA_integer_)
  off <- 0L
  if (!is.na(need) && length(buf) > need) {
    # skip definition-level block: 4-byte LE length + payload
    dl_len <- readBin(buf[1:4], "integer", 1L, size = 4L,
                      endian = "little")
    off <- 4L + dl_len
  }
  body <- buf[(off + 1L):length(buf)]
  if (identical(ptype, 1L)) {
    return(readBin(body, "integer", n, size = 4L, endian = "little"))
  }
  if (identical(ptype, 2L)) {
    return(vapply(seq_len(n), function(i) {
      lo <- readBin(body[(8 * i - 7):(8 * i - 4)], "integer", 1L,
                    size = 4L, endian = "little")
      hi <- readBin(body[(8 * i - 3):(8 * i)], "integer", 1L,
                    size = 4L, endian = "little")
      hi * 2^32 + (lo %% 2^32)
    }, numeric(1)))
  }
  if (identical(ptype, 5L)) {
    return(readBin(body, "double", n, size = 8L, endian = "little"))
  }
  if (identical(ptype, 6L)) {           # BYTE_ARRAY
    out <- character(n)
    i <- 1L
    for (k in seq_len(n)) {
      if (i + 3L > length(body)) break
      len <- readBin(body[i:(i + 3L)], "integer", 1L, size = 4L,
                     endian = "little")
      out[k] <- if (len > 0) rawToChar(body[(i + 4L):(i + 3L + len)])
                else ""
      i <- i + 4L + len
    }
    return(out)
  }
  stop("native Parquet reader: unsupported physical type ", ptype,
       "; install the 'arrow' package.", call. = FALSE)
}

#' Internal helper: Snappy raw-format decompressor (pure R)
#' @noRd
.mpq_snappy <- function(buf) {
  i <- 1L
  # uncompressed length: varint
  out_len <- 0L; shift <- 0L
  repeat {
    b <- as.integer(buf[i]); i <- i + 1L
    out_len <- out_len + bitwAnd(b, 127L) * 2^shift
    if (b < 128L) break
    shift <- shift + 7L
  }
  out <- raw(out_len)
  o <- 1L
  n <- length(buf)
  while (i <= n) {
    tag <- as.integer(buf[i]); i <- i + 1L
    typ <- bitwAnd(tag, 3L)
    if (typ == 0L) {                    # literal
      len <- bitwShiftR(tag, 2L) + 1L
      if (len > 60L) {
        nb <- len - 60L
        len <- 0L
        for (k in seq_len(nb)) {
          len <- len + as.integer(buf[i]) * 2^(8 * (k - 1L))
          i <- i + 1L
        }
        len <- len + 1L
      }
      out[o:(o + len - 1L)] <- buf[i:(i + len - 1L)]
      i <- i + len; o <- o + len
    } else {
      if (typ == 1L) {                  # copy, 1-byte offset
        len <- bitwAnd(bitwShiftR(tag, 2L), 7L) + 4L
        offset <- bitwAnd(bitwShiftR(tag, 5L), 7L) * 256L +
          as.integer(buf[i]); i <- i + 1L
      } else if (typ == 2L) {           # copy, 2-byte offset
        len <- bitwShiftR(tag, 2L) + 1L
        offset <- as.integer(buf[i]) + as.integer(buf[i + 1L]) * 256L
        i <- i + 2L
      } else {                          # copy, 4-byte offset
        len <- bitwShiftR(tag, 2L) + 1L
        offset <- as.integer(buf[i]) + as.integer(buf[i + 1L]) * 256L +
          as.integer(buf[i + 2L]) * 65536L +
          as.integer(buf[i + 3L]) * 16777216L
        i <- i + 4L
      }
      src <- o - offset
      for (k in seq_len(len)) {         # byte-wise: overlaps allowed
        out[o] <- out[src]
        o <- o + 1L; src <- src + 1L
      }
    }
  }
  out
}

# --- thrift compact protocol (subset: structs, lists, i32/i64 zigzag,
# binary) — enough for the Parquet footer + page headers ---

#' @noRd
.mpq_varint <- function(buf, pos) {
  val <- 0; shift <- 0L
  repeat {
    b <- as.integer(buf[pos]); pos <- pos + 1L
    val <- val + bitwAnd(b, 127L) * 2^shift
    if (b < 128L) break
    shift <- shift + 7L
  }
  list(value = val, pos = pos)
}

#' @noRd
.mpq_zigzag <- function(v) {
  if (v %% 2 == 0) v / 2 else -(v + 1) / 2
}

#' @noRd
.mpq_thrift_struct <- function(buf, pos) {
  out <- list()
  fid <- 0L
  repeat {
    b <- as.integer(buf[pos]); pos <- pos + 1L
    if (b == 0L) break                  # STOP
    delta <- bitwShiftR(b, 4L)
    typ <- bitwAnd(b, 15L)
    if (delta == 0L) {
      z <- .mpq_varint(buf, pos); pos <- z$pos
      fid <- as.integer(.mpq_zigzag(z$value))
    } else {
      fid <- fid + delta
    }
    r <- .mpq_thrift_value(buf, pos, typ)
    pos <- r$pos
    out[[as.character(fid)]] <- r$value
  }
  list(value = out, pos = pos)
}

#' @noRd
.mpq_thrift_value <- function(buf, pos, typ) {
  if (typ == 1L) return(list(value = TRUE, pos = pos))
  if (typ == 2L) return(list(value = FALSE, pos = pos))
  if (typ %in% c(5L, 6L)) {             # i32 / i16 zigzag varint
    z <- .mpq_varint(buf, pos)
    return(list(value = as.integer(.mpq_zigzag(z$value)), pos = z$pos))
  }
  if (typ == 7L) {                      # i64
    z <- .mpq_varint(buf, pos)
    return(list(value = .mpq_zigzag(z$value), pos = z$pos))
  }
  if (typ == 8L) {                      # binary/string
    z <- .mpq_varint(buf, pos)
    len <- z$value; pos <- z$pos
    val <- if (len > 0) buf[pos:(pos + len - 1L)] else raw(0)
    return(list(value = val, pos = pos + len))
  }
  if (typ == 9L) {                      # list
    b <- as.integer(buf[pos]); pos <- pos + 1L
    n <- bitwShiftR(b, 4L)
    etyp <- bitwAnd(b, 15L)
    if (n == 15L) {
      z <- .mpq_varint(buf, pos); n <- z$value; pos <- z$pos
    }
    items <- vector("list", n)
    for (k in seq_len(n)) {
      r <- .mpq_thrift_value(buf, pos, etyp)
      items[[k]] <- r$value
      pos <- r$pos
    }
    return(list(value = items, pos = pos))
  }
  if (typ == 12L) {                     # struct
    return(.mpq_thrift_struct(buf, pos))
  }
  if (typ == 4L) {                      # double
    val <- readBin(buf[pos:(pos + 7L)], "double", 1L, size = 8L,
                   endian = "little")
    return(list(value = val, pos = pos + 8L))
  }
  stop("thrift compact: unsupported type ", typ, call. = FALSE)
}

#' Unified fetch dispatcher
#'
#' Inspects the extension / content and dispatches to the native
#' JSON, XML, HTML, CSV/TSV, or Parquet parser, returning a
#' consistent \code{morie_dataset} object.
#'
#' @param path A local file path.
#' @param format Optional explicit format ("json", "xml", "html",
#'   "csv", "tsv", "parquet"); inferred from the extension otherwise.
#' @return An object of class \code{morie_dataset}: list with
#'   \code{data}, \code{format}, \code{source}, \code{n_rows} (when
#'   tabular).
#' @examples
#' d <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' fc <- tempfile(fileext = ".csv")
#' write.csv(d, fc, row.names = FALSE)
#' out <- morie_fetch_unified(fc)
#' out$n_rows
#' @export
morie_fetch_unified <- function(path, format = NULL) {
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(path))
    format <- switch(ext,
      json = "json", geojson = "json", xml = "xml",
      html = "html", htm = "html", csv = "csv", tsv = "tsv",
      parquet = "parquet",
      stop("cannot infer format from extension '", ext,
           "'; pass `format`.", call. = FALSE))
  }
  data <- switch(format,
    json = morie_fetch_json(paste(readLines(path, warn = FALSE),
                                  collapse = "\n")),
    xml = morie_fetch_xml(paste(readLines(path, warn = FALSE),
                                collapse = "\n")),
    html = morie_fetch_html(paste(readLines(path, warn = FALSE),
                                  collapse = "\n")),
    csv = morie_fetch_csv(path, sep = ","),
    tsv = morie_fetch_csv(path, sep = "\t"),
    parquet = .morie_read_parquet(path),
    stop("unknown format: ", format, call. = FALSE))
  structure(
    list(data = data, format = format, source = path,
         n_rows = if (is.data.frame(data)) nrow(data) else NA_integer_),
    class = c("morie_dataset", "list"))
}
