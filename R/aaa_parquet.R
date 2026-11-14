# Native Apache Parquet reader and writer for R.
#
# VENDORED COPY. The same code as R/aaa_parquet.R in rmoriedata, which
# is copied into morie/r-package/morie/R by the R-arm sync, with the same
# C codecs (src/snappy.c, src/gzip.c, src/zstd.c). What differs, on
# purpose: here every function carries roxygen and the two entry points
# are exported (rmoriedata keeps them internal: rmorie Imports rmoriedata,
# and exporting the same names from both would mask on the search path),
# and this reader also reads three-level LIST columns of a primitive type
# (the A2AJ citation fields), which rmoriedata's refuses. Fix bugs in all
# three copies.
#
# The R side of the family read and wrote Parquet through nanoparquet::
# (11 read calls and 5 write calls in rmoriedata alone). This file
# removes that dependency and is the R arm of the Python
# morie/fn/_parquet_core.py; the two decode the same bytes to the same
# values, which is what the parity harness checks.
#
# Scope was measured from the footers of the actual store, not assumed:
# format v1.0, encodings PLAIN / RLE_DICTIONARY / RLE, SNAPPY
# compression, physical types BYTE_ARRAY / INT32 / INT64 / BOOLEAN /
# DOUBLE. FLOAT, INT96, FIXED_LEN_BYTE_ARRAY and uncompressed pages are
# also handled. GZIP pages are read through zlib, and ZSTD pages
# (read only) through src/zstd.c, a decoder written from RFC 8878.
# Other nested and repeated columns are refused rather than
# guessed at, because decoding a repeated column as flat would return a
# wrong row count instead of an error.
#
# References: parquet-format/parquet.thrift (field ids), the Thrift
# compact protocol spec, google/snappy/format_description.txt and RFC 8878.
#
# ponytail: pure R, no compiled code. The snappy tag loop and the RLE
# bit-unpacker are the only hot spots; move those to src/ if a file
# large enough to matter ever shows up. Reading the whole 25,000-row
# complaints sample is the current worst case and it is seconds, not
# minutes.

# ---------------------------------------------------------------- thrift

.pqTStop <- 0x00L
.pqTTrue <- 0x01L
.pqTFalse <- 0x02L
.pqTByte <- 0x03L
.pqTI16 <- 0x04L
.pqTI32 <- 0x05L
.pqTI64 <- 0x06L
.pqTDouble <- 0x07L
.pqTBinary <- 0x08L
.pqTList <- 0x09L
.pqTSet <- 0x0AL
.pqTMap <- 0x0BL
.pqTStruct <- 0x0CL

# A decoder is a plain environment so the position advances by reference
# the way the Python reader's self.pos does; passing it around by value
# would need every helper to return (value, pos) pairs.
#' A decoder is a plain environment so the position advances by
#' reference
#'
#' the way the Python reader's self.pos does; passing it around by
#' value would need every helper to return (value, pos) pairs.
#'
#' @param buf See Usage.
#' @param pos Defaults to \code{1L}.
#' @return The value of \code{e}, as built in the body.
#' @export
#' @keywords internal
.pq_reader <- function(buf, pos = 1L) {
  e <- new.env(parent = emptyenv())
  e$buf <- buf
  e$pos <- pos
  e$depth <- 0L
  e
}

# Every read is bounds-checked: subsetting a raw vector past its end yields 00 bytes, not
# an error, and a reader built on that silently turns a truncated or lying file into wrong
# data. These are the only ways the decoder touches bytes.
.pq_need <- function(e, n) {
  if (!is.finite(n) || n < 0 || e$pos + n - 1 > length(e$buf)) {
    stop("parquet: truncated or corrupt metadata (", n, " bytes needed at byte ",
         e$pos, " of ", length(e$buf), ")", call. = FALSE)
  }
  invisible()
}

.pq_slice <- function(buf, pos, n, what = "data") {
  if (!is.finite(n) || n < 0 || !is.finite(pos) || pos < 1 || pos + n - 1 > length(buf)) {
    stop("parquet: ", what, " runs past the end of its page or chunk (", n,
         " bytes at ", pos, " of ", length(buf), ")", call. = FALSE)
  }
  if (n == 0) raw(0) else buf[seq.int(pos, length.out = n)]
}

#' .pq_byte
#'
#' A step of the parquet implementation. Called by \code{.pq_list}, \code{.pq_map},
#' \code{.pq_scalar} and 2 others in the module.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$buf}, \code{$pos} from it.
#' @return The value of \code{b}, as built in the body.
#' @export
#' @keywords internal
.pq_byte <- function(e) {
  .pq_need(e, 1)
  b <- as.integer(e$buf[e$pos])
  e$pos <- e$pos + 1L
  b
}

#' .pq_varint
#'
#' A step of the parquet implementation. Called by \code{.pq_binary}, \code{.pq_list},
#' \code{.pq_map} and 1 others in the module.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_byte}.
#' @return The value of \code{repeat}.
#' @export
#' @keywords internal
.pq_varint <- function(e) {
  result <- 0
  shift <- 0
  repeat {
    b <- .pq_byte(e)
    result <- result + bitwAnd(b, 127L) * 2^shift
    if (bitwAnd(b, 128L) == 0L) {
      return(result)
    }
    shift <- shift + 7
    # a 64-bit value takes at most 10 bytes; anything longer is not a varint
    if (shift > 63) {
      stop("parquet: varint longer than 10 bytes at byte ", e$pos, call. = FALSE)
    }
  }
}

#' .pq_zigzag
#'
#' A step of the parquet implementation. Called by \code{.pq_scalar}, \code{.pq_struct}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_varint}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @keywords internal
.pq_zigzag <- function(e) {
  n <- .pq_varint(e)
  # (n >> 1) xor -(n & 1), written in doubles so values past 2^31 stay
  # exact up to 2^53 -- file offsets and row counts both live there.
  if (n %% 2 == 1) -(n + 1) / 2 else n / 2
}

#' .pq_binary
#'
#' A step of the parquet implementation. Called by \code{.pq_scalar}.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$buf}, \code{$pos} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @keywords internal
.pq_binary <- function(e) {
  n <- .pq_varint(e)
  .pq_need(e, n)
  out <- if (n == 0) raw(0) else e$buf[seq.int(e$pos, length.out = n)]
  e$pos <- e$pos + n
  out
}

#' .pq_double
#'
#' A step of the parquet implementation. Called by \code{.pq_scalar}.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$buf}, \code{$pos} from it.
#' @return The value of \code{v}, as built in the body.
#' @export
#' @keywords internal
.pq_double <- function(e) {
  .pq_need(e, 8)
  v <- readBin(e$buf[seq.int(e$pos, length.out = 8L)], "double",
    n = 1L, size = 8L, endian = "little"
  )
  e$pos <- e$pos + 8L
  v
}

#' .pq_scalar
#'
#' A step of the parquet implementation. Called by \code{.pq_list}, \code{.pq_map},
#' \code{.pq_struct}.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$pos} from it.
#' @param ttype Passed to \code{==}.
#' @return Nothing; this branch always raises.
#' @export
#' @keywords internal
.pq_scalar <- function(e, ttype) {
  if (ttype == .pqTTrue) {
    return(TRUE)
  }
  if (ttype == .pqTFalse) {
    return(FALSE)
  }
  if (ttype == .pqTByte) {
    b <- .pq_byte(e)
    return(if (b > 127L) b - 256L else b)
  }
  if (ttype == .pqTI16 || ttype == .pqTI32 || ttype == .pqTI64) {
    return(.pq_zigzag(e))
  }
  if (ttype == .pqTDouble) {
    return(.pq_double(e))
  }
  if (ttype == .pqTBinary) {
    return(.pq_binary(e))
  }
  if (ttype == .pqTList || ttype == .pqTSet) {
    return(.pq_list(e))
  }
  if (ttype == .pqTMap) {
    return(.pq_map(e))
  }
  if (ttype == .pqTStruct) {
    return(.pq_struct(e))
  }
  stop("unknown thrift compact type ", ttype, " at byte ", e$pos,
    call. = FALSE
  )
}

# A list or map holds values that may be lists, maps or structs again: the same
# depth cap as .pq_struct, or 5000 nested list headers overflow R's stack.
.pq_deeper <- function(e) {
  e$depth <- e$depth + 1L
  if (e$depth > 64L) stop("parquet: metadata nested deeper than 64 levels", call. = FALSE)
}

#' .pq_list
#'
#' A step of the parquet implementation. Called by \code{.pq_scalar}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_byte}.
#' @return The value of \code{lapply}.
#' @export
#' @keywords internal
.pq_list <- function(e) {
  .pq_deeper(e)
  on.exit(e$depth <- e$depth - 1L)
  h <- .pq_byte(e)
  size <- bitwShiftR(h, 4L)
  etype <- bitwAnd(h, 15L)
  if (size == 15L) size <- .pq_varint(e)
  if (size == 0) {
    return(list())
  }
  # every element takes at least one byte, booleans included
  if (size > length(e$buf) - e$pos + 1) {
    stop("parquet: list of ", size, " elements in ", length(e$buf) - e$pos + 1,
         " remaining bytes", call. = FALSE)
  }
  lapply(seq_len(size), function(i) .pq_scalar(e, etype))
}

#' .pq_map
#'
#' A step of the parquet implementation. Called by \code{.pq_scalar}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_varint}.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @keywords internal
.pq_map <- function(e) {
  .pq_deeper(e)
  on.exit(e$depth <- e$depth - 1L)
  size <- .pq_varint(e)
  if (size == 0) {
    return(list())
  }
  # a map entry is at least a key byte and a value byte; a 21-byte file claiming 2^56
  # entries used to loop until killed
  if (size > (length(e$buf) - e$pos) / 2) {
    stop("parquet: map of ", size, " entries in ", length(e$buf) - e$pos + 1,
         " remaining bytes", call. = FALSE)
  }
  kv <- .pq_byte(e)
  ktype <- bitwShiftR(kv, 4L)
  vtype <- bitwAnd(kv, 15L)
  out <- list()
  for (i in seq_len(size)) {
    k <- .pq_scalar(e, ktype)
    out[[rawToChar(k)]] <- .pq_scalar(e, vtype)
  }
  out
}

#' .pq_struct
#'
#' A step of the parquet implementation. Called by \code{.pq_column_values},
#' \code{.pq_read_footer}, \code{.pq_scalar}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_byte}.
#' @return The value of \code{repeat}.
#' @export
#' @keywords internal
.pq_struct <- function(e) {
  .pq_deeper(e)
  on.exit(e$depth <- e$depth - 1L)
  out <- list()
  fid <- 0L
  repeat {
    h <- .pq_byte(e)
    if (h == .pqTStop) {
      return(out)
    }
    delta <- bitwShiftR(h, 4L)
    ttype <- bitwAnd(h, 15L)
    fid <- if (delta != 0L) fid + delta else as.integer(.pq_zigzag(e))
    out[[as.character(fid)]] <- .pq_scalar(e, ttype)
  }
}

#' .pq_f
#'
#' A step of the parquet implementation. Called by \code{.pq_column_values},
#' \code{morie_read_parquet}.
#' See the file header for the source the module follows.
#'
#' @param st A vector; indexed elementwise.
#' @param id Coerced to character by the body, with \code{as.character}.
#' @param default Defaults to \code{NULL}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @keywords internal
.pq_f <- function(st, id, default = NULL) {
  v <- st[[as.character(id)]]
  if (is.null(v)) default else v
}

# Writer. Accumulates into a list of raw vectors and concatenates once,
# because growing a raw vector element by element is quadratic.
#' Writer. Accumulates into a list of raw vectors and concatenates once,
#'
#' because growing a raw vector element by element is quadratic.
#'
#' @return The value of \code{e}, as built in the body.
#' @export
#' @examples
#' res <- .pq_writer()
#' res
#' @keywords internal
.pq_writer <- function() {
  e <- new.env(parent = emptyenv())
  e$parts <- list()
  e$n <- 0L
  e
}

#' .pq_emit
#'
#' A step of the parquet implementation. Called by \code{.pq_wbinary}, \code{.pq_wfield},
#' \code{.pq_wlisthdr} and 4 others in the module.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$n}, \code{$parts} from it.
#' @param r See Usage.
#' @return Invisibly,nothing; the function is called for its effect.
#' @export
#' @keywords internal
.pq_emit <- function(e, r) {
  e$n <- e$n + 1L
  e$parts[[e$n]] <- r
  invisible(NULL)
}

#' .pq_wvarint
#'
#' A step of the parquet implementation. Called by \code{.pq_wbinary},
#' \code{.pq_wlisthdr}, \code{.pq_wzigzag}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_emit}.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{.pq_emit}.
#' @export
#' @keywords internal
.pq_wvarint <- function(e, n) {
  out <- raw(0)
  repeat {
    if (n < 128) {
      out <- c(out, as.raw(n))
      break
    }
    out <- c(out, as.raw(bitwOr(as.integer(n %% 128), 128L)))
    n <- n %/% 128
  }
  .pq_emit(e, out)
}

#' .pq_wzigzag
#'
#' A step of the parquet implementation. Called by \code{.pq_wfield}, \code{.pq_wi32},
#' \code{.pq_wi64} and 1 others in the module.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wvarint}.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{.pq_wvarint}.
#' @export
#' @keywords internal
.pq_wzigzag <- function(e, n) {
  .pq_wvarint(e, if (n < 0) -2 * n - 1 else 2 * n)
}

# The bytes of a string as UTF-8, which is what a Parquet string is by
# definition. A string marked latin1 (or UTF-8) is transcoded by
# enc2utf8(); an unmarked string is written byte for byte when it already
# is valid UTF-8 (what read.csv() and the store give), because enc2utf8()
# would translate it from the native encoding, and in a C locale that
# turns every non-ASCII byte into its "<c3><a9>" display form. Unmarked
# bytes that are not UTF-8 are refused: written through they make a file
# no conforming reader opens, escaped they lose the data.
.pq_utf8 <- function(s, what = "a column") {
  s <- as.character(s)
  marked <- !is.na(s) & Encoding(s) != "unknown"
  if (any(marked)) s[marked] <- enc2utf8(s[marked])
  bad <- !is.na(s) & !validUTF8(s)
  if (any(bad)) {
    stop(what, " holds a string that is not valid UTF-8 (Parquet strings ",
         "are UTF-8): set its encoding with Encoding()<- or convert it ",
         "with iconv() before writing.", call. = FALSE)
  }
  s
}

#' .pq_wbinary
#'
#' A step of the parquet implementation. Called by \code{.pq_wbytes}, \code{.pq_wlistbin}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wvarint}.
#' @param b A vector; its length is taken.
#' @return The value of \code{.pq_emit}.
#' @export
#' @keywords internal
.pq_wbinary <- function(e, b) {
  if (is.character(b)) b <- charToRaw(.pq_utf8(b))
  .pq_wvarint(e, length(b))
  .pq_emit(e, b)
}

#' .pq_wfield
#'
#' A step of the parquet implementation. Called by \code{.pq_wbytes}, \code{.pq_wi32},
#' \code{.pq_wi64} and 4 others in the module.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_emit}.
#' @param fid Numeric; combined arithmetically in the body.
#' @param ttype Passed to \code{bitwOr}.
#' @param last Numeric; combined arithmetically in the body.
#' @return The value of \code{fid}, as built in the body.
#' @export
#' @keywords internal
.pq_wfield <- function(e, fid, ttype, last) {
  delta <- fid - last
  if (delta > 0 && delta <= 15) {
    .pq_emit(e, as.raw(bitwOr(bitwShiftL(delta, 4L), ttype)))
  } else {
    .pq_emit(e, as.raw(ttype))
    .pq_wzigzag(e, fid)
  }
  fid
}

#' .pq_wi32
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param v Passed to \code{.pq_wzigzag}.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wi32 <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTI32, last)
  .pq_wzigzag(e, v)
  last
}

#' .pq_wi64
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param v Passed to \code{.pq_wzigzag}.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wi64 <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTI64, last)
  .pq_wzigzag(e, v)
  last
}

#' .pq_wbytes
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param v Passed to \code{.pq_wbinary}.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wbytes <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTBinary, last)
  .pq_wbinary(e, v)
  last
}

#' .pq_wlisthdr
#'
#' A step of the parquet implementation. Called by \code{.pq_wlistbin},
#' \code{.pq_wlisti32}, \code{.pq_wliststruct}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_emit}.
#' @param size Passed to \code{.pq_wvarint}.
#' @param etype Passed to \code{bitwOr}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @keywords internal
.pq_wlisthdr <- function(e, size, etype) {
  if (size < 15) {
    .pq_emit(e, as.raw(bitwOr(bitwShiftL(size, 4L), etype)))
  } else {
    .pq_emit(e, as.raw(bitwOr(240L, etype)))
    .pq_wvarint(e, size)
  }
}

#' .pq_wlisti32
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param vals A vector; its length is taken.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wlisti32 <- function(e, fid, vals, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(vals), .pqTI32)
  for (v in vals) .pq_wzigzag(e, v)
  last
}

#' .pq_wlistbin
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param vals A vector; its length is taken.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wlistbin <- function(e, fid, vals, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(vals), .pqTBinary)
  for (v in vals) .pq_wbinary(e, v)
  last
}

#' .pq_wliststruct
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param bodies A vector; its length is taken.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wliststruct <- function(e, fid, bodies, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(bodies), .pqTStruct)
  for (b in bodies) .pq_emit(e, b)
  last
}

#' .pq_wstruct
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e Passed to \code{.pq_wfield}.
#' @param fid Passed to \code{.pq_wfield}.
#' @param body Passed to \code{.pq_emit}.
#' @param last Passed to \code{.pq_wfield}.
#' @return The value of \code{last}, as built in the body.
#' @export
#' @keywords internal
.pq_wstruct <- function(e, fid, body, last) {
  last <- .pq_wfield(e, fid, .pqTStruct, last)
  .pq_emit(e, body)
  last
}

#' .pq_wstop
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param e A list; the body reads \code{$parts} from it.
#' @return The value of \code{do.call}.
#' @export
#' @keywords internal
.pq_wstop <- function(e) {
  .pq_emit(e, as.raw(0L))
  do.call(base::c, e$parts)
}

# ---------------------------------------------------------------- snappy

#' .pq_snappy_decompress
#'
#' Snappy decompression in C (src/snappy.c): every snappy tag, literals and
#' 1-, 2- and 4-byte-offset copies. Called by \code{.pq_column_values}.
#'
#' @param data A raw vector, a snappy stream.
#' @return A raw vector.
#' @export
#' @keywords internal
.pq_snappy_decompress <- function(data) {
  # src/snappy.c: every snappy tag (literals, 1-, 2- and 4-byte-offset copies)
  .Call(C_rmd_snappy_decompress, data)
}

#' Snappy compression in C (src/snappy.c)
#'
#' The reference greedy compressor: a 4-byte hash per 64 KiB block and
#' 2-byte-offset copies.
#'
#' @param data A raw vector.
#' @return A raw vector, the snappy stream.
#' @export
#' @examples
#' x <- charToRaw(strrep("abc", 20))
#' res <- .pq_snappy_compress(data = x)
#' identical(.pq_snappy_decompress(res), x)
#' @keywords internal
.pq_snappy_compress <- function(data) {
  # src/snappy.c: the reference greedy compressor (4-byte hash per 64 KiB block,
  # 2-byte-offset copies)
  .Call(C_rmd_snappy_compress, data)
}

# ------------------------------------------------------------- constants

.pqBoolean <- 0L
.pqInt32 <- 1L
.pqInt64 <- 2L
.pqInt96 <- 3L
.pqFloat <- 4L
.pqDouble <- 5L
.pqByteArray <- 6L
.pqFlba <- 7L
.pqEPlain <- 0L
.pqEPlainDict <- 2L
.pqERle <- 3L
.pqEBitPacked <- 4L
.pqERleDict <- 8L
.pqCUncompressed <- 0L
.pqCSnappy <- 1L
.pqCGzip <- 2L
.pqCZstd <- 6L
.pqRequired <- 0L
.pqOptional <- 1L
.pqRepeated <- 2L
.pqPData <- 0L
.pqPIndex <- 1L
.pqPDict <- 2L
.pqPDataV2 <- 3L
.pqCtUtf8 <- 0L
.pqCtDate <- 6L
.pqCtTsMillis <- 9L
.pqCtTsMicros <- 10L

# --------------------------------------------------------------- decoding

# readBin ignores signed = FALSE for sizes above 2 and warns about it, so
# unsigned 32-bit values (byte-array lengths, the footer length, the low
# half of an INT64) have to be read signed and folded back up.
#' ReadBin ignores signed = FALSE for sizes above 2 and warns about it,
#' so
#'
#' unsigned 32-bit values (byte-array lengths, the footer length, the
#' low half of an INT64) have to be read signed and folded back up.
#'
#' @param r A vector; its length is taken.
#' @return The value of \code{ifelse}.
#' @export
#' @keywords internal
.pq_u32 <- function(r) {
  v <- readBin(r, "integer",
    n = length(r) %/% 4L, size = 4L,
    endian = "little"
  )
  ifelse(v < 0, v + 2^32, v)
}

#' .pq_bit_width
#'
#' A step of the parquet implementation. Called by \code{.pq_column_values}.
#' See the file header for the source the module follows.
#'
#' @param n Passed to \code{>}.
#' @return The value of \code{w}, as built in the body.
#' @export
#' @examples
#' res <- .pq_bit_width(n = 3L)
#' res
#' @keywords internal
.pq_bit_width <- function(n) {
  w <- 0L
  while (n > 0) {
    w <- w + 1L
    n <- bitwShiftR(n, 1L)
  }
  w
}

#' .pq_read_rle
#'
#' A step of the parquet implementation. Called by \code{.pq_column_values}.
#' See the file header for the source the module follows.
#'
#' @param buf A vector; indexed elementwise.
#' @param pos Numeric; combined arithmetically in the body.
#' @param width A count; the body uses it as \code{seq_len(...)}.
#' @param count A count; the body uses it as \code{rep(...)}.
#' @param end Passed to \code{<=}.
#' @return A list with \code{values}, \code{pos}.
#' @export
#' @keywords internal
.pq_read_rle <- function(buf, pos, width, count, end) {
  if (width == 0L) {
    return(list(values = rep(0L, count), pos = pos))
  }
  if (width > 32L) stop("parquet: RLE bit width ", width, " above 32", call. = FALSE)
  end <- min(end, length(buf))
  out <- integer(count)
  o <- 0L
  nbytes <- (width + 7L) %/% 8L
  while (o < count && pos <= end) {
    header <- 0
    shift <- 0
    repeat {
      if (pos > end) stop("parquet: RLE run header runs past its data", call. = FALSE)
      b <- as.integer(buf[pos])
      pos <- pos + 1L
      header <- header + bitwAnd(b, 127L) * 2^shift
      if (bitwAnd(b, 128L) == 0L) break
      shift <- shift + 7
      if (shift > 63) stop("parquet: RLE run header longer than 10 bytes", call. = FALSE)
    }
    if (header %% 2 == 1) { # bit-packed
      groups <- (header - 1) / 2
      need <- groups * width
      if (need > end - pos + 1) stop("parquet: bit-packed run of ", groups,
                                     " groups runs past its data", call. = FALSE)
      nvals <- groups * 8
      chunk <- .pq_slice(buf, pos, need, "bit-packed run")
      pos <- pos + need
      # Unpack LSB-first across byte boundaries.
      bits <- as.integer(rawToBits(chunk))
      take <- min(nvals, count - o)
      if (take > 0L) {
        vals <- vapply(seq_len(take), function(i) {
          b0 <- (i - 1L) * width
          sum(bits[(b0 + 1L):(b0 + width)] * 2^(seq_len(width) - 1L))
        }, numeric(1))
        out[seq.int(o + 1L, length.out = take)] <- as.integer(vals)
      }
      o <- o + take
    } else { # RLE run
      run <- header / 2
      val <- sum(as.integer(.pq_slice(buf, pos, nbytes, "RLE run value")) *
        256^(seq_len(nbytes) - 1L))
      pos <- pos + nbytes
      take <- min(run, count - o)
      if (take > 0L) {
        out[seq.int(o + 1L, length.out = take)] <- as.integer(val)
      }
      o <- o + take
    }
  }
  if (o < count) {
    stop("parquet: RLE data ran out after ", o, " of ", count, " values", call. = FALSE)
  }
  list(values = out, pos = pos)
}

#' ReadBin has no 64-bit integer, so recombine two 32-bit halves. Exact
#'
#' to 2^53, which covers every count, offset and epoch value here.
#'
#' @param raw_bytes Passed to \code{.pq_u32}.
#' @param count Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @keywords internal
.pq_read_i64 <- function(raw_bytes, count) {
  # readBin has no 64-bit integer, so recombine two 32-bit halves. Exact
  # to 2^53, which covers every count, offset and epoch value here.
  lo <- .pq_u32(raw_bytes)[seq(1L, by = 2L, length.out = count)]
  hi <- readBin(raw_bytes, "integer",
    n = count * 2L, size = 4L,
    endian = "little"
  )[seq(2L, by = 2L, length.out = count)]
  hi * 2^32 + lo
}

#' .pq_decode_plain
#'
#' A step of the parquet implementation. Called by \code{.pq_column_values}.
#' See the file header for the source the module follows.
#'
#' @param buf A vector; indexed elementwise.
#' @param pos Numeric; combined arithmetically in the body.
#' @param ptype Passed to \code{==}.
#' @param count A count; the body uses it as \code{seq_len(...)}.
#' @param type_length Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return Nothing; this branch always raises.
#' @export
#' @keywords internal
.pq_decode_plain <- function(buf, pos, ptype, count, type_length = NULL) {
  if (count == 0L) {
    return(list(values = list(), pos = pos))
  }
  take <- function(n) .pq_slice(buf, pos, n, "a PLAIN value block")
  if (ptype == .pqBoolean) {
    nb <- (count + 7L) %/% 8L
    bits <- as.integer(rawToBits(take(nb)))
    return(list(
      values = as.logical(bits[seq_len(count)]),
      pos = pos + nb
    ))
  }
  if (ptype == .pqInt32) {
    v <- readBin(take(count * 4L), "integer", n = count, size = 4L, endian = "little")
    return(list(values = v, pos = pos + count * 4L))
  }
  if (ptype == .pqInt64) {
    v <- .pq_read_i64(take(count * 8L), count)
    return(list(values = v, pos = pos + count * 8L))
  }
  if (ptype == .pqFloat) {
    v <- readBin(take(count * 4L), "double", n = count, size = 4L, endian = "little")
    return(list(values = v, pos = pos + count * 4L))
  }
  if (ptype == .pqDouble) {
    v <- readBin(take(count * 8L), "double", n = count, size = 8L, endian = "little")
    return(list(values = v, pos = pos + count * 8L))
  }
  if (ptype == .pqByteArray) {
    vals <- vector("list", count)
    for (i in seq_len(count)) {
      n <- .pq_u32(.pq_slice(buf, pos, 4L, "a BYTE_ARRAY length"))
      pos <- pos + 4L
      vals[[i]] <- .pq_slice(buf, pos, n, "a BYTE_ARRAY value")
      pos <- pos + n
    }
    return(list(values = vals, pos = pos))
  }
  if (ptype == .pqFlba) {
    if (is.null(type_length) || type_length < 1) {
      stop("FIXED_LEN_BYTE_ARRAY without a positive type_length", call. = FALSE)
    }
    block <- take(count * type_length)
    vals <- lapply(seq_len(count), function(i) {
      block[seq.int((i - 1L) * type_length + 1L, length.out = type_length)]
    })
    return(list(values = vals, pos = pos + count * type_length))
  }
  if (ptype == .pqInt96) {
    block <- take(count * 12L)
    vals <- numeric(count)
    for (i in seq_len(count)) {
      o <- (i - 1L) * 12L
      nanos <- .pq_read_i64(block[o + 1:8], 1L)
      jday <- .pq_u32(block[o + 9:12])
      vals[i] <- (jday - 2440588) * 86400 * 1e9 + nanos
    }
    return(list(values = vals, pos = pos + count * 12L))
  }
  stop("unsupported physical type ", ptype, call. = FALSE)
}

#' .pq_convert
#'
#' A step of the parquet implementation. Called by \code{morie_read_parquet}.
#' See the file header for the source the module follows.
#'
#' @param vals Iterated over elementwise, with \code{vapply}.
#' @param ptype Passed to \code{==}.
#' @param converted Accepted by the signature and not used anywhere in the body.
#' @return The value of \code{vals}, as built in the body.
#' @export
#' @keywords internal
.pq_convert <- function(vals, ptype, converted) {
  if (ptype == .pqByteArray) {
    return(vapply(vals, function(v) {
      if (is.null(v)) {
        return(NA_character_)
      }
      if (length(v) == 0L) {
        return("")
      }
      s <- rawToChar(v)
      Encoding(s) <- "UTF-8"
      s
    }, character(1)))
  }
  vals
}

# Logical (converted) types. Applied identically in the Python arm, so
# the two agree on the epoch value; without this a DATE column came back
# as a bare day count while nanoparquet handed the caller a Date, which
# is a silent factor-of-86400 trap for anyone swapping the engines.
#' Logical (converted) types. Applied identically in the Python arm, so
#'
#' the two agree on the epoch value; without this a DATE column came
#' back as a bare day count while nanoparquet handed the caller a Date,
#' which is a silent factor-of-86400 trap for anyone swapping the
#' engines.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @param ptype Passed to \code{==}.
#' @param converted Optional; may be \code{NULL}. Passed to \code{is.null}.
#' @return The value of \code{v}, as built in the body.
#' @export
#' @keywords internal
.pq_apply_logical <- function(v, ptype, converted) {
  if (is.null(converted)) {
    return(v)
  }
  if (converted == .pqCtDate && ptype == .pqInt32) {
    return(structure(as.numeric(v), class = "Date"))
  }
  if (converted == .pqCtTsMillis) {
    return(as.POSIXct(v / 1e3, origin = "1970-01-01", tz = "UTC"))
  }
  if (converted == .pqCtTsMicros) {
    return(as.POSIXct(v / 1e6, origin = "1970-01-01", tz = "UTC"))
  }
  v
}

# ------------------------------------------------------------------ read

#' .pq_read_footer
#'
#' A step of the parquet implementation. Called by \code{morie_read_parquet}.
#' See the file header for the source the module follows.
#'
#' @param con Passed to \code{seek}.
#' @param size Numeric; combined arithmetically in the body.
#' @return The value of \code{.pq_struct}.
#' @export
#' @keywords internal
.pq_read_footer <- function(con, size) {
  if (size < 12) stop("not a parquet file: ", size, " bytes", call. = FALSE)
  seek(con, 0L)
  if (!identical(readBin(con, "raw", 4L), charToRaw("PAR1"))) {
    stop("not a parquet file: missing leading PAR1", call. = FALSE)
  }
  seek(con, size - 8L)
  tail <- readBin(con, "raw", 8L)
  if (!identical(tail[5:8], charToRaw("PAR1"))) {
    stop("not a parquet file: missing trailing PAR1", call. = FALSE)
  }
  n <- .pq_u32(tail[1:4])
  if (n < 2 || n > size - 12) {
    stop("parquet: footer length ", n, " does not fit a ", size, "-byte file",
         call. = FALSE)
  }
  seek(con, size - 8L - n)
  .pq_struct(.pq_reader(readBin(con, "raw", n)))
}

# The largest page this reader will inflate, and the most rows it will build. A
# page header or a footer that claims more is refused before anything is allocated.
.pq_max_page <- function() getOption("rmoriedata.parquet_max_page_bytes", 2^30)
.pq_max_rows <- function() getOption("rmoriedata.parquet_max_rows", 5e7)

# The repetition or definition levels at the start of a data page (RLE, with
# their byte length ahead of them), each checked against the column's maximum.
#' .pq_read_levels
#'
#' One RLE level run of a v1 data page: a 4-byte length, then the run.
#' Repetition levels and definition levels are both stored this way.
#'
#' @param page Raw page bytes.
#' @param p Start position (1-based).
#' @param maxlevel Largest level value; sets the bit width.
#' @param n Number of level slots in the page.
#' @param encoding Level encoding from the data page header.
#' @param what \code{"repetition"} or \code{"definition"}.
#' @param cname The column's name, for error messages.
#' @return A list with \code{levels} and \code{pos}.
#' @noRd
.pq_read_levels <- function(page, p, maxlevel, n, encoding, what, cname) {
  if (encoding != .pqERle) {
    stop("BIT_PACKED ", what, " levels not implemented", call. = FALSE)
  }
  ln <- .pq_u32(.pq_slice(page, p, 4L, paste0("a ", what, "-level length")))
  p <- p + 4L
  .pq_slice(page, p, ln, paste(what, "levels"))
  rr <- .pq_read_rle(page, p, .pq_bit_width(maxlevel), n, p + ln - 1L)
  if (n > 0L && max(rr$values) > maxlevel) {
    stop("parquet: column ", cname, " has a ", what, " level above its maximum of ",
         maxlevel, call. = FALSE)
  }
  list(levels = rr$values, pos = p + ln)
}

# The rows of a LIST column from its levels: a slot with repetition level 0 starts
# a row, one with level 1 adds to it. A row is NULL for a null list, list() for an
# empty one, and holds NULL for a null element.
#' .pq_assemble_lists
#'
#' Fold level slots into one list per row. A slot whose definition level
#' is below \code{repdef - 1} is a null list, exactly \code{repdef - 1}
#' is an empty list, and anything at or above \code{repdef} is an
#' element (\code{NULL} when it stops short of the leaf's max level).
#'
#' @param reps Integer repetition levels, one per slot.
#' @param defs Integer definition levels, one per slot.
#' @param slot_vals List of decoded values, \code{NULL} where absent.
#' @param repdef Definition level of the REPEATED node.
#' @param cname The column's name, for error messages.
#' @return A list with one entry per row.
#' @noRd
.pq_assemble_lists <- function(reps, defs, slot_vals, repdef, cname) {
  if (length(reps) && reps[[1L]] != 0L) {
    stop("parquet: column ", cname, " starts in the middle of a list", call. = FALSE)
  }
  rows <- list()
  for (i in seq_along(reps)) {
    d <- defs[i]
    if (reps[i] == 0L) {
      k <- length(rows) + 1L
      if (d < repdef - 1L) {
        rows[k] <- list(NULL)
      } else if (d == repdef - 1L) {
        rows[[k]] <- list()
      } else {
        rows[[k]] <- list(slot_vals[[i]])
      }
    } else {
      k <- length(rows)
      if (d < repdef || is.null(rows[[k]])) {
        stop("parquet: column ", cname, " adds an element to a null or empty list",
             call. = FALSE)
      }
      rows[[k]] <- c(rows[[k]], list(slot_vals[[i]]))
    }
  }
  rows
}

#' .pq_column_values
#'
#' A step of the parquet implementation. Called by \code{morie_read_parquet}.
#' See the file header for the source the module follows.
#'
#' A flat column gives one value (or \code{NULL}) per row. A LIST column
#' (\code{maxrep = 1}) gives one list per row: \code{NULL} for a null
#' list, \code{list()} for an empty one, and \code{NULL} inside the list
#' for a null element.
#'
#' @param con Passed to \code{seek}.
#' @param cm Passed to \code{.pq_f}.
#' @param num_rows A count; the body uses it as \code{seq_len(...)}.
#' @param maxdef Largest definition level of the leaf.
#' @param typelen Passed to \code{.pq_decode_plain}.
#' @param stype The physical type the schema declares for the column.
#' @param fsize The size of the file in bytes.
#' @param cname The column's name, for error messages.
#' @param maxrep Largest repetition level of the leaf (0 for flat).
#' @param repdef Definition level of the REPEATED node (LIST only).
#' @return The value of \code{[}.
#' @export
#' @keywords internal
.pq_column_values <- function(con, cm, num_rows, maxdef, typelen, stype, fsize, cname,
                              maxrep = 0L, repdef = 0L) {
  ptype <- as.integer(.pq_f(cm, 1))
  if (!identical(ptype, as.integer(stype))) {
    stop("parquet: column ", cname, " is declared type ", stype,
         " in the schema but its chunk says ", ptype, call. = FALSE)
  }
  codec <- as.integer(.pq_f(cm, 4))
  total_values <- .pq_f(cm, 5)
  clen <- .pq_f(cm, 7)
  data_off <- .pq_f(cm, 9)
  dict_off <- .pq_f(cm, 11)
  # a LIST column holds one value per element, at least one per row
  if (is.null(total_values) || is.null(clen) || is.null(data_off) ||
      (maxrep == 0L && total_values != num_rows) || total_values < num_rows) {
    stop("parquet: column ", cname, " chunk holds ", total_values %||% "?",
         " values; its row group has ", num_rows, call. = FALSE)
  }

  start <- if (!is.null(dict_off) && dict_off > 0 &&
    dict_off < data_off) {
    dict_off
  } else {
    data_off
  }
  if (start < 4 || clen < 0 || start + clen > fsize - 8) {
    stop("parquet: column ", cname, " chunk (", clen, " bytes at ", start,
         ") lies outside the ", fsize, "-byte file", call. = FALSE)
  }
  seek(con, start)
  blob <- readBin(con, "raw", clen)
  if (length(blob) != clen) {
    stop("parquet: column ", cname, " chunk is truncated", call. = FALSE)
  }

  pos <- 1L
  dictionary <- NULL
  values <- list()
  all_reps <- integer()
  all_defs <- integer()
  got <- 0

  while (got < total_values && pos <= length(blob)) {
    r <- .pq_reader(blob, pos)
    head <- .pq_struct(r)
    pos <- r$pos
    usize <- .pq_f(head, 2)
    csize <- .pq_f(head, 3)
    if (is.null(usize) || is.null(csize) || usize < 0 || usize > .pq_max_page()) {
      stop("parquet: page of ", usize %||% "?", " bytes in column ", cname,
           " (cap ", .pq_max_page(), ")", call. = FALSE)
    }
    raw_page <- .pq_slice(blob, pos, csize, paste("a page of column", cname))
    pos <- pos + csize
    crc <- .pq_f(head, 4)
    if (!is.null(crc)) {
      have <- .Call(C_rmd_crc32, raw_page)
      if (have != crc %% 2^32) {
        stop("parquet: page CRC mismatch in column ", cname, call. = FALSE)
      }
    }
    ptype_page <- as.integer(.pq_f(head, 1))
    if (ptype_page == .pqPDataV2) {
      # v2 keeps its levels uncompressed ahead of the values, so
      # decompressing the whole page would fail with a snappy error
      # before this message could fire
      stop("data page v2 not implemented (store is v1)", call. = FALSE)
    }
    if (codec == .pqCSnappy) {
      page <- .pq_snappy_decompress(raw_page)
    } else if (codec == .pqCGzip) {
      page <- .Call(C_rmd_gzip_decompress, raw_page, usize)
    } else if (codec == .pqCZstd) {
      # src/zstd.c: never writes past usize and fails unless the frames give
      # exactly usize bytes
      page <- .Call(C_rmd_zstd_decompress, raw_page, usize)
    } else if (codec == .pqCUncompressed) {
      page <- raw_page
    } else {
      stop("compression codec ", codec, " not implemented; the reader ",
        "takes ZSTD, GZIP, SNAPPY or none",
        call. = FALSE
      )
    }
    if (length(page) != usize) {
      stop("parquet: page of column ", cname, " inflates to ", length(page),
           " bytes; its header says ", usize, call. = FALSE)
    }

    if (ptype_page == .pqPDict) {
      dh <- .pq_f(head, 7)
      nd <- as.integer(.pq_f(dh, 1))
      dec <- .pq_decode_plain(page, 1L, ptype, nd, typelen)
      if (dec$pos != length(page) + 1L) {
        stop("parquet: dictionary page of column ", cname, " holds ",
             length(page) - dec$pos + 1L, " bytes beyond its ", nd, " values",
             call. = FALSE)
      }
      dictionary <- dec$values
      next
    }
    if (ptype_page != .pqPData) next

    dph <- .pq_f(head, 5)
    n <- as.integer(.pq_f(dph, 1))
    if (is.na(n) || n < 0 || got + n > total_values) {
      stop("parquet: page of column ", cname, " claims ", n, " values with ",
           total_values - got, " left in the chunk", call. = FALSE)
    }
    encoding <- as.integer(.pq_f(dph, 2))
    p <- 1L

    # Repetition levels come first and exist only under a REPEATED node; then
    # definition levels. A flat OPTIONAL column has maximum definition level 1;
    # REQUIRED has 0 and writes none.
    reps <- integer(n)
    if (maxrep > 0L) {
      rl <- .pq_read_levels(page, p, maxrep, n, as.integer(.pq_f(dph, 4)), "repetition",
                            cname)
      reps <- rl$levels
      p <- rl$pos
    }
    if (maxdef > 0L) {
      dl <- .pq_read_levels(page, p, maxdef, n, as.integer(.pq_f(dph, 3)), "definition",
                            cname)
      defs <- dl$levels
      p <- dl$pos
    } else {
      defs <- integer(n)
    }

    present <- sum(defs == maxdef)
    if (encoding == .pqEPlainDict || encoding == .pqERleDict) {
      if (is.null(dictionary)) {
        stop("dictionary-encoded page with no dictionary page",
          call. = FALSE
        )
      }
      width <- as.integer(.pq_slice(page, p, 1L, "a dictionary bit width"))
      p <- p + 1L
      idx <- .pq_read_rle(page, p, width, present, length(page))$values
      if (present && (min(idx) < 0L || max(idx) >= length(dictionary))) {
        stop("parquet: dictionary index out of range in column ", cname, call. = FALSE)
      }
      vals <- dictionary[idx + 1L]
    } else if (encoding == .pqEPlain) {
      dec <- .pq_decode_plain(page, p, ptype, present, typelen)
      # PLAIN values fill the page exactly: bytes left over (or a shortfall, caught above)
      # mean the declared type is not the type the page was written with
      if (dec$pos != length(page) + 1L) {
        stop("parquet: page of column ", cname, " holds ", length(page) - dec$pos + 1L,
             " bytes beyond its ", present, " values of the declared type", call. = FALSE)
      }
      vals <- dec$values
    } else {
      stop("encoding ", encoding, " not implemented; the store uses ",
        "PLAIN and RLE_DICTIONARY",
        call. = FALSE
      )
    }

    # Splice the present values back into their definition-level slots.
    col <- vector("list", n)
    j <- 0L
    for (i in seq_len(n)) {
      if (defs[i] == maxdef) {
        j <- j + 1L
        col[[i]] <- if (is.list(vals)) vals[[j]] else vals[j]
      } else {
        col[i] <- list(NULL)
      }
    }
    values <- c(values, col)
    if (maxrep > 0L) {
      all_reps <- c(all_reps, reps)
      all_defs <- c(all_defs, defs)
    }
    got <- got + n
  }

  if (maxrep > 0L) {
    if (got != total_values) {
      stop("parquet: column ", cname, " decoded ", got, " of its ", total_values,
           " values", call. = FALSE)
    }
    values <- .pq_assemble_lists(all_reps, all_defs, values, repdef, cname)
  }
  # exact, never padded or truncated: a count the data does not bear out is an error
  if (length(values) != num_rows) {
    stop("parquet: column ", cname, " decoded ", length(values),
         " values; its row group has ", num_rows, call. = FALSE)
  }
  values
}

# The leaf columns of the schema, with their maximum definition and repetition
# levels. A group is read only as a three-level LIST of one primitive (an optional
# or required group annotated LIST, holding one repeated group, holding the
# element), which is returned under the LIST's name; any other group, and any
# other repeated field, is refused.
#' .pq_schema_leaves
#'
#' Walk the flattened schema tree into one entry per leaf column. The
#' supported shapes are a flat leaf and the three-level LIST group
#' (LIST group, REPEATED "list" group, one element leaf). Anything else
#' stops rather than decoding to a wrong row count.
#'
#' @param schema The footer's schema list; element 1 is the root.
#' @return A list of leaf descriptors.
#' @noRd
.pq_schema_leaves <- function(schema) {
  root <- schema[[1L]]
  leaves <- list()
  # stack rows: children left, maxdef, maxrep, repdef, list name
  stack <- list(list(
    left = .pq_f(root, 5, 0L), maxdef = 0L, maxrep = 0L, repdef = 0L,
    list_name = NULL
  ))
  for (i in seq.int(2L, length.out = length(schema) - 1L)) {
    el <- schema[[i]]
    while (length(stack) && stack[[length(stack)]]$left == 0L) {
      stack[[length(stack)]] <- NULL
    }
    if (!length(stack)) {
      stop("parquet: the schema has more elements than its root declares",
        call. = FALSE
      )
    }
    parent <- stack[[length(stack)]]
    stack[[length(stack)]]$left <- parent$left - 1L
    rep_type <- .pq_f(el, 3, .pqRequired)
    name <- rawToChar(.pq_f(el, 4))
    maxdef <- parent$maxdef + as.integer(rep_type != .pqRequired)
    maxrep <- parent$maxrep + as.integer(rep_type == .pqRepeated)
    repdef <- if (rep_type == .pqRepeated) maxdef else parent$repdef
    nchild <- .pq_f(el, 5, 0L)
    if (nchild > 0L) {
      logical <- .pq_f(el, 10)
      is_list <- identical(as.integer(.pq_f(el, 6, -1L)), 3L) ||
        (!is.null(logical) && !is.null(logical[["3"]]))
      if (rep_type == .pqRepeated && !is.null(parent$list_name) &&
        nchild == 1L) {
        stack[[length(stack) + 1L]] <- list(
          left = 1L, maxdef = maxdef, maxrep = maxrep, repdef = repdef,
          list_name = parent$list_name
        )
        next
      }
      if (is_list && rep_type != .pqRepeated &&
        is.null(parent$list_name) && nchild == 1L) {
        stack[[length(stack) + 1L]] <- list(
          left = 1L, maxdef = maxdef, maxrep = maxrep, repdef = repdef,
          list_name = name
        )
        next
      }
      stop("nested schema not implemented: group field ", name,
        " has ", nchild, " children",
        call. = FALSE
      )
    }
    if (maxrep > 1L || (maxrep == 1L && is.null(parent$list_name))) {
      stop("repeated column ", name, " not implemented; decoding it ",
        "as flat would silently change the row count",
        call. = FALSE
      )
    }
    leaves[[length(leaves) + 1L]] <- list(
      name = if (is.null(parent$list_name)) name else parent$list_name,
      type = as.integer(.pq_f(el, 1)),
      typelen = .pq_f(el, 2),
      converted = .pq_f(el, 6),
      maxdef = maxdef,
      maxrep = maxrep,
      repdef = repdef
    )
  }
  leaves
}

# One leaf's decoded values as an R vector of its type.
#' .pq_leaf_vector
#'
#' Turn one leaf's decoded slots into an R vector of the leaf's type,
#' applying the converted (logical) type.
#'
#' @param col List of decoded values, \code{NULL} where absent.
#' @param leaf A leaf descriptor from \code{.pq_schema_leaves}.
#' @return An atomic vector.
#' @noRd
.pq_leaf_vector <- function(col, leaf) {
  if (leaf$type == .pqByteArray) {
    return(.pq_convert(col, leaf$type, leaf$converted))
  }
  v <- vapply(col, function(x) {
    if (is.null(x)) NA_real_ else as.numeric(x)
  }, numeric(1))
  if (leaf$type == .pqBoolean) {
    v <- as.logical(v)
  } else if (leaf$type == .pqInt32) {
    v <- as.integer(v)
  }
  .pq_apply_logical(v, leaf$type, leaf$converted)
}

#' Read a Parquet file
#'
#' Native Parquet reader: no nanoparquet, no arrow. Handles the v1
#' format with PLAIN, RLE and dictionary encodings, GZIP, Snappy, Zstandard
#' (ZSTD) or no compression. ZSTD pages are read by a decoder in the package
#' (RFC 8878, without dictionaries), so no other package is needed for them;
#' the writer does not write ZSTD. Flat columns and three-level \code{LIST}
#' columns of a primitive type are decoded; a \code{LIST} column comes back
#' as a list column (class \code{AsIs}) holding one vector per row,
#' \code{NULL} for a null list. Other nesting is refused.
#'
#' @param path Path to a `.parquet` file.
#' @param columns Optional character vector of column names to decode;
#'   the rest are skipped entirely.
#' @return A `data.frame`. Every error the reader raises, a missing or
#'   damaged file included, starts with `parquet:`; no base-R warning escapes
#'   it (a value a damaged file makes R warn about is an error instead).
#' @export
#' @examples
#' df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
#' p <- tempfile(fileext = ".parquet")
#' morie_write_parquet(df, p)
#' morie_read_parquet(p)
morie_read_parquet <- function(path, columns = NULL) {
  size <- file.info(path)$size
  if (is.na(size)) stop("parquet: no such file: ", path, call. = FALSE)
  # A check the reader makes says what is wrong, after the `parquet: ` every reader
  # error starts with; anything else a damaged footer or page trips over (a NULL
  # field, an index past the end) is said to be damage too. A base-R warning on the
  # way (a count past the integer range, say) is damage as well, not a stray warning.
  tryCatch(
    withCallingHandlers(.pq_read(path, columns, size), warning = function(w) {
      m <- conditionMessage(w)
      if (!startsWith(m, "duplicate column names in ")) {
        stop("parquet: the file is damaged (", m, ")", call. = FALSE)
      }
    }),
    error = function(e) {
      m <- conditionMessage(e)
      if (startsWith(m, "parquet: ")) stop(e)
      if (grepl(.pq_own_re, m) || grepl("^cannot allocate", m)) {
        stop("parquet: ", m, call. = FALSE)
      }
      stop("parquet: the file is damaged (", m, ")", call. = FALSE)
    }
  )
}

# The reader's own error messages, passed on with the `parquet: ` prefix.
.pq_own_re <- paste0("^(not a parquet file|snappy: |gzip: |zstd: |crc32: ",
                     "|unknown thrift ",
                     "|FIXED_LEN_BYTE_ARRAY |unsupported physical |data page v2 ",
                     "|compression codec |BIT_PACKED |dictionary-encoded |encoding ",
                     "|nested schema |repeated column |no such column|footer says )")

.pq_read <- function(path, columns, size) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  meta <- .pq_read_footer(con, size)
  schema <- .pq_f(meta, 2)
  num_rows <- .pq_f(meta, 3)
  row_groups <- .pq_f(meta, 4)
  if (is.null(row_groups)) row_groups <- list()
  if (is.null(num_rows) || num_rows < 0 || num_rows > .pq_max_rows()) {
    stop("parquet: footer claims ", num_rows %||% "?", " rows (cap ",
         .pq_max_rows(), ", option rmoriedata.parquet_max_rows)", call. = FALSE)
  }
  rg_rows <- vapply(row_groups, function(rg) as.numeric(.pq_f(rg, 3) %||% NA), numeric(1))
  if (anyNA(rg_rows) || any(rg_rows < 0) || sum(rg_rows) != num_rows) {
    stop("parquet: row groups hold ", sum(rg_rows), " rows; the footer says ",
         num_rows, call. = FALSE)
  }
  if (is.null(schema) || length(schema) < 2L) stop("parquet: empty schema", call. = FALSE)

  leaves <- .pq_schema_leaves(schema)

  wanted <- seq_along(leaves)
  if (!is.null(columns)) {
    nm <- vapply(leaves, function(l) l$name, character(1))
    missing <- setdiff(columns, nm)
    if (length(missing)) {
      stop("no such column(s) in ", path, ": ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    wanted <- match(columns, nm)
  }

  out <- list()
  out_names <- character()
  for (i in wanted) {
    leaf <- leaves[[i]]
    col <- list()
    for (rg in row_groups) {
      chunks <- .pq_f(rg, 1)
      if (length(chunks) != length(leaves)) {
        stop("parquet: a row group has ", length(chunks), " column chunks for ",
             length(leaves), " schema columns", call. = FALSE)
      }
      cm <- .pq_f(chunks[[i]], 3)
      col <- c(col, .pq_column_values(
        con, cm, .pq_f(rg, 3),
        leaf$maxdef, leaf$typelen, leaf$type, size, leaf$name,
        leaf$maxrep, leaf$repdef
      ))
    }
    v <- if (leaf$maxrep > 0L) {
      I(lapply(col, function(row) {
        if (is.null(row)) NULL else .pq_leaf_vector(row, leaf)
      }))
    } else {
      .pq_leaf_vector(col, leaf)
    }
    out[[length(out) + 1L]] <- v
    out_names <- c(out_names, leaf$name)
  }
  if (anyDuplicated(out_names)) {
    warning("duplicate column names in ", basename(path), ": ",
            paste(sQuote(unique(out_names[duplicated(out_names)])),
                  collapse = ", "), "; made unique with make.unique()",
            call. = FALSE)
    out_names <- make.unique(out_names)
  }
  names(out) <- out_names

  df <- as.data.frame(out,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (nrow(df) != num_rows) {
    stop("footer says ", num_rows, " rows, decoded ", nrow(df),
      call. = FALSE
    )
  }
  df
}

# ----------------------------------------------------------------- write

#' Date and POSIXct must keep their logical type on the way out
#'
#' Without this a column read as TIMESTAMP_MICROS was written back as a
#' bare DOUBLE of seconds: readable, but no longer a timestamp to any
#' other engine, and off by a factor of 1e6 from where it started.
#'
#' @param v Numeric; the body checks with \code{is.numeric}.
#' @param name The column's name, for error messages.
#' @return A list with \code{type}, \code{converted}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .pq_infer(v = x)
#' res
#' @keywords internal
.pq_infer <- function(v, name = "a column") {
  # a matrix column holds several values per row: written flat it had more values
  # than rows, which other readers cut short without a word
  if (!is.null(dim(v))) {
    stop("column ", sQuote(name), " is a ",
         if (is.data.frame(v)) "data frame" else "matrix or array",
         " column, with more than one value per row, which Parquet as written here ",
         "cannot hold; split it into one column per value first.", call. = FALSE)
  }
  # Date and POSIXct must keep their logical type on the way out.
  # Without this a column read as TIMESTAMP_MICROS was written back as a
  # bare DOUBLE of seconds: readable, but no longer a timestamp to any
  # other engine, and off by a factor of 1e6 from where it started.
  if (inherits(v, "Date")) {
    if (any(abs(floor(unclass(v))) > .Machine$integer.max, na.rm = TRUE)) {
      stop("column ", sQuote(name), " has a Date outside the range a Parquet DATE ",
           "holds (2^31 days either side of 1970-01-01).", call. = FALSE)
    }
    return(list(type = .pqInt32, converted = .pqCtDate))
  }
  if (inherits(v, "POSIXct")) {
    if (any(abs(floor(as.numeric(v) * 1000)) >= 2^63, na.rm = TRUE)) {
      stop("column ", sQuote(name), " has a time that is infinite or outside the ",
           "range a Parquet TIMESTAMP holds (2^63 milliseconds either side of 1970).",
           call. = FALSE)
    }
    return(list(type = .pqInt64, converted = .pqCtTsMillis))
  }
  # written as text these would read back as a different type: refused, not converted
  # an integer64's bits are not a double's value: written as a DOUBLE it read back wrong
  if (is.list(v) || is.complex(v) || is.raw(v) ||
        inherits(v, c("difftime", "integer64"))) {
    stop("column ", sQuote(name), " is of type ",
         if (is.object(v)) class(v)[[1L]] else typeof(v),
         ", which Parquet as written here cannot hold; convert it first (for ",
         "example with as.character() or as.numeric()).", call. = FALSE)
  }
  if (is.logical(v)) {
    return(list(type = .pqBoolean, converted = NULL))
  }
  if (is.integer(v)) {
    return(list(type = .pqInt32, converted = NULL))
  }
  if (is.numeric(v)) {
    return(list(type = .pqDouble, converted = NULL))
  }
  list(type = .pqByteArray, converted = .pqCtUtf8)
}

# Down to whole days and whole milliseconds, as R prints them: as.integer() cut
# Date(-0.5), which prints as 1969-12-31, to day 0 (1970-01-01), and rounding moved
# 23:59:59.9996 to the next day.
# (.pq_infer() has refused values outside the range of either)
#' .pq_prep_write
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param v Coerced to numeric by the body, with \code{as.numeric}.
#' @param inf Accepted by the signature and not used anywhere in the body.
#' @return The value of \code{v}, as built in the body.
#' @export
#' @keywords internal
.pq_prep_write <- function(v, inf) {
  if (inherits(v, "Date")) {
    return(as.integer(floor(unclass(v))))
  }
  if (inherits(v, "POSIXct")) {
    return(floor(as.numeric(v) * 1000))
  }
  v
}

#' WriteBin has no 64-bit integer; emit two 32-bit halves, folding the
#'
#' low half back into signed range because that is all writeBin takes.
#'
#' @param values Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{writeBin}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .pq_encode_i64(values = x)
#' res
#' @keywords internal
.pq_encode_i64 <- function(values) {
  # writeBin has no 64-bit integer; emit two 32-bit halves, folding the
  # low half back into signed range because that is all writeBin takes.
  v <- as.numeric(values)
  hi <- floor(v / 2^32)
  lo <- v - hi * 2^32
  lo <- ifelse(lo >= 2^31, lo - 2^32, lo)
  out <- integer(2L * length(v))
  out[seq(1L, by = 2L, length.out = length(v))] <- as.integer(lo)
  out[seq(2L, by = 2L, length.out = length(v))] <- as.integer(hi)
  writeBin(out, raw(), size = 4L, endian = "little")
}

#' .pq_encode_plain
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param values A vector; its length is taken and its elements indexed.
#' @param ptype Passed to \code{==}.
#' @param name The column's name, for error messages.
#' @return Nothing; this branch always raises.
#' @export
#' @keywords internal
.pq_encode_plain <- function(values, ptype, name = "a column") {
  if (length(values) == 0L) {
    return(raw(0))
  }
  if (ptype == .pqBoolean) {
    n <- length(values)
    pad <- (8L - n %% 8L) %% 8L
    bits <- as.raw(c(as.integer(values), rep(0L, pad)))
    return(packBits(as.logical(bits), "raw"))
  }
  if (ptype == .pqInt32) {
    return(writeBin(as.integer(values), raw(),
      size = 4L,
      endian = "little"
    ))
  }
  if (ptype == .pqInt64) {
    return(.pq_encode_i64(values))
  }
  if (ptype == .pqDouble) {
    return(writeBin(as.numeric(values), raw(),
      size = 8L,
      endian = "little"
    ))
  }
  if (ptype == .pqByteArray) {
    parts <- vector("list", length(values) * 2L)
    for (i in seq_along(values)) {
      b <- charToRaw(.pq_utf8(values[[i]], paste0("column ", sQuote(name))))
      parts[[2L * i - 1L]] <- writeBin(length(b), raw(),
        size = 4L,
        endian = "little"
      )
      parts[[2L * i]] <- b
    }
    return(do.call(base::c, parts))
  }
  stop("cannot PLAIN-encode physical type ", ptype, call. = FALSE)
}

#' .pq_encode_levels
#'
#' A step of the parquet implementation. Called by \code{morie_write_parquet}.
#' See the file header for the source the module follows.
#'
#' @param levels A vector; its length is taken.
#' @param width Passed to \code{==}.
#' @return A vector, from \code{c}.
#' @export
#' @keywords internal
.pq_encode_levels <- function(levels, width) {
  if (width == 0L) {
    return(raw(0))
  }
  groups <- (length(levels) + 7L) %/% 8L
  padded <- c(as.integer(levels != 0L), rep(0L, groups * 8L -
    length(levels)))
  body <- packBits(as.logical(padded), "raw")
  h <- groups * 2 + 1
  header <- raw(0)
  repeat {
    if (h < 128) {
      header <- c(header, as.raw(h))
      break
    }
    header <- c(header, as.raw(bitwOr(as.integer(h %% 128), 128L)))
    h <- h %/% 128
  }
  payload <- c(header, body)
  c(
    writeBin(length(payload), raw(), size = 4L, endian = "little"),
    payload
  )
}

#' Write a data frame to Parquet
#'
#' Native Parquet writer: single row group, PLAIN encoding, all columns
#' OPTIONAL. Output is read back unchanged by pyarrow and nanoparquet.
#'
#' @param df A `data.frame` with at least one column. Factor columns are
#'   written as character; list, complex, raw, `difftime`, `integer64`,
#'   matrix and data frame columns are refused with an error naming the
#'   column. `NaN` in a numeric column is
#'   kept as `NaN`. Every string must be valid UTF-8 (marked, or unmarked in
#'   UTF-8 bytes), and column names must be unique.
#' @param path Destination path.
#' @param compression `"gzip"` (default; zlib level 9), `"snappy"`, or `NULL`
#'   for uncompressed pages.
#' @details A `Date` column is written as a Parquet DATE (whole days) and a
#'   `POSIXct` column as a Parquet TIMESTAMP (whole milliseconds since
#'   1970-01-01 UTC), each taken down to the whole day or millisecond as R
#'   prints them, before 1970 too: a fractional `Date` of -0.5 (printed
#'   1969-12-31) is written as 1969-12-31. The format stores the instant
#'   but not a time zone, so it reads back as the same instant in UTC: a column in
#'   `America/Chicago` comes back with `tz = "UTC"`. Set the zone again with
#'   `attr(x, "tzone") <- "America/Chicago"`, or keep an ISO 8601 text column
#'   beside it, as `rmoriedata::chiload()`'s `date_iso` does.
#' @return `path`, invisibly.
#' @export
#' @examples
#' df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
#' p <- tempfile(fileext = ".parquet")
#' morie_write_parquet(df, p)
morie_write_parquet <- function(df, path, compression = "gzip") {
  if (!is.null(compression) &&
        !(identical(compression, "gzip") || identical(compression, "snappy"))) {
    stop("compression must be \"gzip\", \"snappy\" or NULL; got ",
      sQuote(compression),
      call. = FALSE
    )
  }
  codec <- if (is.null(compression)) {
    .pqCUncompressed
  } else if (compression == "gzip") {
    .pqCGzip
  } else {
    .pqCSnappy
  }

  nrows <- nrow(df)
  names_ <- names(df)
  # a file with no columns has a schema with no leaves, which no reader opens
  if (!length(names_)) {
    stop("a data frame with no columns cannot be written: a Parquet file needs ",
         "at least one column.", call. = FALSE)
  }
  if (anyDuplicated(names_)) {
    stop("duplicate column names: ",
         paste(sQuote(unique(names_[duplicated(names_)])), collapse = ", "),
         "; a Parquet reader cannot address them, and this one would ",
         "keep only the last.", call. = FALSE)
  }
  # every column's type is settled (or refused) before the file is opened
  infs <- lapply(seq_along(names_), function(k) .pq_infer(df[[k]], names_[k]))
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("PAR1"), con)
  offset <- 4

  chunks <- list()
  for (k in seq_along(names_)) {
    v <- df[[k]]
    inf <- infs[[k]]
    # NaN is a value of a DOUBLE column, not a missing one: written as null it read
    # back as NA
    miss <- is.na(v)
    if (inf$type == .pqDouble) miss <- miss & !is.nan(v)
    defs <- as.integer(!miss)
    present <- .pq_prep_write(v[!miss], inf)

    body <- c(
      .pq_encode_levels(defs, 1L),
      .pq_encode_plain(present, inf$type, names_[k])
    )
    # page sizes are 32-bit in the page header (and the codecs take no more)
    if (length(body) > .Machine$integer.max) {
      stop("column ", sQuote(names_[k]), " needs a page of more than the 2^31 - 1 ",
           "bytes a Parquet page header can state; write the table in smaller ",
           "pieces.", call. = FALSE)
    }
    payload <- switch(as.character(codec),
                      "1" = .pq_snappy_compress(body),
                      "2" = .Call(C_rmd_gzip_compress, body),
                      body)

    dph <- .pq_writer()
    last <- .pq_wi32(dph, 1L, nrows, 0L)
    last <- .pq_wi32(dph, 2L, .pqEPlain, last)
    last <- .pq_wi32(dph, 3L, .pqERle, last)
    last <- .pq_wi32(dph, 4L, .pqERle, last)
    dph_body <- .pq_wstop(dph)

    ph <- .pq_writer()
    last <- .pq_wi32(ph, 1L, .pqPData, 0L)
    last <- .pq_wi32(ph, 2L, length(body), last)
    last <- .pq_wi32(ph, 3L, length(payload), last)
    # PageHeader.crc: CRC-32 of the page as stored (after compression), so a reader can
    # tell a damaged page from data; written as the signed 32-bit value the spec uses
    crc <- .Call(C_rmd_crc32, payload)
    last <- .pq_wi32(ph, 4L, if (crc >= 2^31) crc - 2^32 else crc, last)
    last <- .pq_wstruct(ph, 5L, dph_body, last)
    ph_body <- .pq_wstop(ph)

    page_off <- offset
    writeBin(ph_body, con)
    writeBin(payload, con)
    total <- length(ph_body) + length(payload)
    offset <- offset + total

    cmd <- .pq_writer()
    last <- .pq_wi32(cmd, 1L, inf$type, 0L)
    last <- .pq_wlisti32(cmd, 2L, c(.pqERle, .pqEPlain), last)
    last <- .pq_wlistbin(cmd, 3L, names_[k], last)
    last <- .pq_wi32(cmd, 4L, codec, last)
    last <- .pq_wi64(cmd, 5L, nrows, last)
    last <- .pq_wi64(cmd, 6L, length(ph_body) + length(body), last)
    last <- .pq_wi64(cmd, 7L, total, last)
    last <- .pq_wi64(cmd, 9L, page_off, last)
    cmd_body <- .pq_wstop(cmd)

    cc <- .pq_writer()
    last <- .pq_wi64(cc, 2L, page_off, 0L)
    last <- .pq_wstruct(cc, 3L, cmd_body, last)
    chunks[[k]] <- list(body = .pq_wstop(cc), inf = inf, total = total)
  }

  root <- .pq_writer()
  last <- .pq_wbytes(root, 4L, "schema", 0L)
  last <- .pq_wi32(root, 5L, length(names_), last)
  schema_structs <- list(.pq_wstop(root))
  for (k in seq_along(names_)) {
    se <- .pq_writer()
    last <- .pq_wi32(se, 1L, chunks[[k]]$inf$type, 0L)
    last <- .pq_wi32(se, 3L, .pqOptional, last)
    last <- .pq_wbytes(se, 4L, names_[k], last)
    if (!is.null(chunks[[k]]$inf$converted)) {
      last <- .pq_wi32(se, 6L, chunks[[k]]$inf$converted, last)
    }
    schema_structs[[k + 1L]] <- .pq_wstop(se)
  }

  rg <- .pq_writer()
  last <- .pq_wliststruct(rg, 1L, lapply(chunks, `[[`, "body"), 0L)
  last <- .pq_wi64(rg, 2L, sum(vapply(
    chunks, `[[`, numeric(1),
    "total"
  )), last)
  last <- .pq_wi64(rg, 3L, nrows, last)
  rg_body <- .pq_wstop(rg)

  fm <- .pq_writer()
  last <- .pq_wi32(fm, 1L, 1L, 0L)
  last <- .pq_wliststruct(fm, 2L, schema_structs, last)
  last <- .pq_wi64(fm, 3L, nrows, last)
  last <- .pq_wliststruct(fm, 4L, list(rg_body), last)
  last <- .pq_wbytes(fm, 6L, "morie native parquet writer", last)
  footer <- .pq_wstop(fm)

  writeBin(footer, con)
  writeBin(writeBin(length(footer), raw(),
    size = 4L,
    endian = "little"
  ), con)
  writeBin(charToRaw("PAR1"), con)
  invisible(path)
}
