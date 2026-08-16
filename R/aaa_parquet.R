# Native Apache Parquet reader and writer for R.
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
# also handled. Nested and repeated columns are refused rather than
# guessed at, because decoding a repeated column as flat would return a
# wrong row count instead of an error.
#
# References: parquet-format/parquet.thrift (field ids), the Thrift
# compact protocol spec, and google/snappy/format_description.txt.
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
#' the way the Python reader\'s self.pos does; passing it around by
#' value would need every helper to return (value, pos) pairs.
#'
#' @param buf See Usage.
#' @param pos Defaults to \code{1L}.
#' @return The value of \code{e}, as built in the body.
#' @export
.pq_reader <- function(buf, pos = 1L) {
  e <- new.env(parent = emptyenv())
  e$buf <- buf
  e$pos <- pos
  e
}

#' .pq_byte
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{b}, as built in the body.
#' @export
.pq_byte <- function(e) {
  b <- as.integer(e$buf[e$pos])
  e$pos <- e$pos + 1L
  b
}

#' .pq_varint
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{repeat}.
#' @export
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
  }
}

#' .pq_zigzag
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.pq_zigzag <- function(e) {
  n <- .pq_varint(e)
  # (n >> 1) xor -(n & 1), written in doubles so values past 2^31 stay
  # exact up to 2^53 -- file offsets and row counts both live there.
  if (n %% 2 == 1) -(n + 1) / 2 else n / 2
}

#' .pq_binary
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.pq_binary <- function(e) {
  n <- .pq_varint(e)
  out <- e$buf[seq.int(e$pos, length.out = n)]
  e$pos <- e$pos + n
  out
}

#' .pq_double
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
.pq_double <- function(e) {
  v <- readBin(e$buf[seq.int(e$pos, length.out = 8L)], "double",
    n = 1L, size = 8L, endian = "little"
  )
  e$pos <- e$pos + 8L
  v
}

#' .pq_scalar
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param ttype See Usage.
#' @return Nothing; this branch always raises.
#' @export
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

#' .pq_list
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{lapply}.
#' @export
.pq_list <- function(e) {
  h <- .pq_byte(e)
  size <- bitwShiftR(h, 4L)
  etype <- bitwAnd(h, 15L)
  if (size == 15L) size <- .pq_varint(e)
  if (size == 0) {
    return(list())
  }
  lapply(seq_len(size), function(i) .pq_scalar(e, etype))
}

#' .pq_map
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.pq_map <- function(e) {
  size <- .pq_varint(e)
  if (size == 0) {
    return(list())
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{repeat}.
#' @export
.pq_struct <- function(e) {
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param st See Usage.
#' @param id See Usage.
#' @param default Defaults to \code{NULL}.
#' @return One of two values, depending on the branch taken.
#' @export
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
.pq_writer <- function() {
  e <- new.env(parent = emptyenv())
  e$parts <- list()
  e$n <- 0L
  e
}

#' .pq_emit
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param r See Usage.
#' @return Invisibly,nothing; the function is called for its effect.
#' @export
.pq_emit <- function(e, r) {
  e$n <- e$n + 1L
  e$parts[[e$n]] <- r
  invisible(NULL)
}

#' .pq_wvarint
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param n See Usage.
#' @return The value of \code{.pq_emit}.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param n See Usage.
#' @return The value of \code{.pq_wvarint}.
#' @export
.pq_wzigzag <- function(e, n) {
  .pq_wvarint(e, if (n < 0) -2 * n - 1 else 2 * n)
}

#' .pq_wbinary
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param b See Usage.
#' @return The value of \code{.pq_emit}.
#' @export
.pq_wbinary <- function(e, b) {
  if (is.character(b)) b <- charToRaw(enc2utf8(b))
  .pq_wvarint(e, length(b))
  .pq_emit(e, b)
}

#' .pq_wfield
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param ttype See Usage.
#' @param last See Usage.
#' @return The value of \code{fid}, as built in the body.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param v See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wi32 <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTI32, last)
  .pq_wzigzag(e, v)
  last
}

#' .pq_wi64
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param v See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wi64 <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTI64, last)
  .pq_wzigzag(e, v)
  last
}

#' .pq_wbytes
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param v See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wbytes <- function(e, fid, v, last) {
  last <- .pq_wfield(e, fid, .pqTBinary, last)
  .pq_wbinary(e, v)
  last
}

#' .pq_wlisthdr
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param size See Usage.
#' @param etype See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param vals See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wlisti32 <- function(e, fid, vals, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(vals), .pqTI32)
  for (v in vals) .pq_wzigzag(e, v)
  last
}

#' .pq_wlistbin
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param vals See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wlistbin <- function(e, fid, vals, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(vals), .pqTBinary)
  for (v in vals) .pq_wbinary(e, v)
  last
}

#' .pq_wliststruct
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param bodies See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wliststruct <- function(e, fid, bodies, last) {
  last <- .pq_wfield(e, fid, .pqTList, last)
  .pq_wlisthdr(e, length(bodies), .pqTStruct)
  for (b in bodies) .pq_emit(e, b)
  last
}

#' .pq_wstruct
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @param fid See Usage.
#' @param body See Usage.
#' @param last See Usage.
#' @return The value of \code{last}, as built in the body.
#' @export
.pq_wstruct <- function(e, fid, body, last) {
  last <- .pq_wfield(e, fid, .pqTStruct, last)
  .pq_emit(e, body)
  last
}

#' .pq_wstop
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param e See Usage.
#' @return The value of \code{do.call}.
#' @export
.pq_wstop <- function(e) {
  .pq_emit(e, as.raw(0L))
  do.call(base::c, e$parts)
}

# ---------------------------------------------------------------- snappy

#' .pq_snappy_decompress
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param data See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.pq_snappy_decompress <- function(data) {
  pos <- 1L
  n <- 0
  shift <- 0
  repeat {
    b <- as.integer(data[pos])
    pos <- pos + 1L
    n <- n + bitwAnd(b, 127L) * 2^shift
    if (bitwAnd(b, 128L) == 0L) break
    shift <- shift + 7
  }

  out <- raw(n)
  o <- 0L
  total <- length(data)
  while (pos <= total) {
    tag <- as.integer(data[pos])
    pos <- pos + 1L
    kind <- bitwAnd(tag, 3L)
    if (kind == 0L) { # literal
      ln <- bitwShiftR(tag, 2L)
      if (ln >= 60L) {
        extra <- ln - 59L
        ln <- sum(as.integer(data[seq.int(pos, length.out = extra)]) *
          256^(seq_len(extra) - 1L))
        pos <- pos + extra
      }
      ln <- ln + 1L
      out[seq.int(o + 1L, length.out = ln)] <-
        data[seq.int(pos, length.out = ln)]
      pos <- pos + ln
      o <- o + ln
      next
    }
    if (kind == 1L) { # 1-byte offset
      ln <- 4L + bitwAnd(bitwShiftR(tag, 2L), 7L)
      off <- bitwShiftL(bitwShiftR(tag, 5L), 8L) + as.integer(data[pos])
      pos <- pos + 1L
    } else if (kind == 2L) { # 2-byte offset
      ln <- bitwShiftR(tag, 2L) + 1L
      off <- as.integer(data[pos]) + 256L * as.integer(data[pos + 1L])
      pos <- pos + 2L
    } else { # 4-byte offset
      ln <- bitwShiftR(tag, 2L) + 1L
      off <- sum(as.integer(data[seq.int(pos, length.out = 4L)]) *
        256^(0:3))
      pos <- pos + 4L
    }
    if (off <= 0 || off > o) {
      stop("snappy: bad copy offset ", off, call. = FALSE)
    }
    start <- o - off
    if (off >= ln) {
      # Non-overlapping: one vectorised move.
      out[seq.int(o + 1L, length.out = ln)] <-
        out[seq.int(start + 1L, length.out = ln)]
    } else {
      # Overlapping copies are how snappy encodes runs, so the source
      # grows as it is read and this has to go a byte at a time.
      for (i in seq_len(ln)) out[o + i] <- out[start + i]
    }
    o <- o + ln
  }
  if (o != n) {
    stop("snappy: expected ", n, " bytes, decoded ", o, call. = FALSE)
  }
  out
}

#' Literal-only stream: fully conformant, just does not shrink
#'
#' ponytail: no match-finder; add one if written size ever matters.
#'
#' @param data See Usage.
#' @return The value of \code{do.call}.
#' @export
.pq_snappy_compress <- function(data) {
  # Literal-only stream: fully conformant, just does not shrink.
  # ponytail: no match-finder; add one if written size ever matters.
  out <- list()
  k <- 0L
  n <- length(data)
  hdr <- raw(0)
  m <- n
  repeat {
    if (m < 128) {
      hdr <- c(hdr, as.raw(m))
      break
    }
    hdr <- c(hdr, as.raw(bitwOr(as.integer(m %% 128), 128L)))
    m <- m %/% 128
  }
  k <- k + 1L
  out[[k]] <- hdr

  pos <- 0L
  while (pos < n) {
    chunk <- min(n - pos, 65536L)
    ln <- chunk - 1L
    if (ln < 60L) {
      tag <- as.raw(bitwShiftL(ln, 2L))
    } else if (ln < 256L) {
      tag <- c(as.raw(240L), as.raw(ln))
    } else {
      tag <- c(as.raw(244L), as.raw(ln %% 256L), as.raw(ln %/% 256L))
    }
    k <- k + 1L
    out[[k]] <- tag
    k <- k + 1L
    out[[k]] <- data[seq.int(pos + 1L, length.out = chunk)]
    pos <- pos + chunk
  }
  do.call(base::c, out)
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
#' @param r See Usage.
#' @return The value of \code{ifelse}.
#' @export
.pq_u32 <- function(r) {
  v <- readBin(r, "integer",
    n = length(r) %/% 4L, size = 4L,
    endian = "little"
  )
  ifelse(v < 0, v + 2^32, v)
}

#' .pq_bit_width
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return The value of \code{w}, as built in the body.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param buf See Usage.
#' @param pos See Usage.
#' @param width See Usage.
#' @param count See Usage.
#' @param end See Usage.
#' @return A list with \code{values}, \code{pos}.
#' @export
.pq_read_rle <- function(buf, pos, width, count, end) {
  if (width == 0L) {
    return(list(values = rep(0L, count), pos = pos))
  }
  out <- integer(count)
  o <- 0L
  nbytes <- (width + 7L) %/% 8L
  while (o < count && pos <= end) {
    header <- 0
    shift <- 0
    repeat {
      b <- as.integer(buf[pos])
      pos <- pos + 1L
      header <- header + bitwAnd(b, 127L) * 2^shift
      if (bitwAnd(b, 128L) == 0L) break
      shift <- shift + 7
    }
    if (header %% 2 == 1) { # bit-packed
      groups <- (header - 1) / 2
      nvals <- groups * 8
      need <- groups * width
      chunk <- as.integer(buf[seq.int(pos, length.out = need)])
      pos <- pos + need
      # Unpack LSB-first across byte boundaries.
      bits <- as.integer(rawToBits(as.raw(chunk)))
      idx <- seq_len(nvals)
      vals <- vapply(idx, function(i) {
        b0 <- (i - 1L) * width
        sum(bits[(b0 + 1L):(b0 + width)] * 2L^(seq_len(width) - 1L))
      }, numeric(1))
      take <- min(length(vals), count - o)
      if (take > 0L) {
        out[seq.int(o + 1L, length.out = take)] <-
          as.integer(vals[seq_len(take)])
      }
      o <- o + take
    } else { # RLE run
      run <- header / 2
      val <- sum(as.integer(buf[seq.int(pos, length.out = nbytes)]) *
        256^(seq_len(nbytes) - 1L))
      pos <- pos + nbytes
      take <- min(run, count - o)
      if (take > 0L) {
        out[seq.int(o + 1L, length.out = take)] <-
          as.integer(val)
      }
      o <- o + take
    }
  }
  list(values = out, pos = pos)
}

#' ReadBin has no 64-bit integer, so recombine two 32-bit halves. Exact
#'
#' to 2^53, which covers every count, offset and epoch value here.
#'
#' @param raw_bytes See Usage.
#' @param count See Usage.
#' @return A numeric value.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param buf See Usage.
#' @param pos See Usage.
#' @param ptype See Usage.
#' @param count See Usage.
#' @param type_length Defaults to \code{NULL}.
#' @return Nothing; this branch always raises.
#' @export
.pq_decode_plain <- function(buf, pos, ptype, count, type_length = NULL) {
  if (count == 0L) {
    return(list(values = list(), pos = pos))
  }
  if (ptype == .pqBoolean) {
    nb <- (count + 7L) %/% 8L
    bits <- as.integer(rawToBits(buf[seq.int(pos, length.out = nb)]))
    return(list(
      values = as.logical(bits[seq_len(count)]),
      pos = pos + nb
    ))
  }
  if (ptype == .pqInt32) {
    v <- readBin(buf[seq.int(pos, length.out = count * 4L)], "integer",
      n = count, size = 4L, endian = "little"
    )
    return(list(values = v, pos = pos + count * 4L))
  }
  if (ptype == .pqInt64) {
    v <- .pq_read_i64(buf[seq.int(pos, length.out = count * 8L)], count)
    return(list(values = v, pos = pos + count * 8L))
  }
  if (ptype == .pqFloat) {
    v <- readBin(buf[seq.int(pos, length.out = count * 4L)], "double",
      n = count, size = 4L, endian = "little"
    )
    return(list(values = v, pos = pos + count * 4L))
  }
  if (ptype == .pqDouble) {
    v <- readBin(buf[seq.int(pos, length.out = count * 8L)], "double",
      n = count, size = 8L, endian = "little"
    )
    return(list(values = v, pos = pos + count * 8L))
  }
  if (ptype == .pqByteArray) {
    vals <- vector("list", count)
    for (i in seq_len(count)) {
      n <- .pq_u32(buf[seq.int(pos, length.out = 4L)])
      pos <- pos + 4L
      vals[[i]] <- if (n == 0L) {
        raw(0)
      } else {
        buf[seq.int(pos, length.out = n)]
      }
      pos <- pos + n
    }
    return(list(values = vals, pos = pos))
  }
  if (ptype == .pqFlba) {
    if (is.null(type_length)) {
      stop("FIXED_LEN_BYTE_ARRAY without type_length", call. = FALSE)
    }
    vals <- lapply(seq_len(count), function(i) {
      buf[seq.int(pos + (i - 1L) * type_length, length.out = type_length)]
    })
    return(list(values = vals, pos = pos + count * type_length))
  }
  if (ptype == .pqInt96) {
    vals <- numeric(count)
    for (i in seq_len(count)) {
      nanos <- .pq_read_i64(buf[seq.int(pos, length.out = 8L)], 1L)
      jday <- .pq_u32(buf[seq.int(pos + 8L, length.out = 4L)])
      pos <- pos + 12L
      vals[i] <- (jday - 2440588) * 86400 * 1e9 + nanos
    }
    return(list(values = vals, pos = pos))
  }
  stop("unsupported physical type ", ptype, call. = FALSE)
}

#' .pq_convert
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param vals See Usage.
#' @param ptype See Usage.
#' @param converted See Usage.
#' @return The value of \code{vals}, as built in the body.
#' @export
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
#' @param v See Usage.
#' @param ptype See Usage.
#' @param converted See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param con See Usage.
#' @param size See Usage.
#' @return The value of \code{.pq_struct}.
#' @export
.pq_read_footer <- function(con, size) {
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
  seek(con, size - 8L - n)
  .pq_struct(.pq_reader(readBin(con, "raw", n)))
}

#' .pq_column_values
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param con See Usage.
#' @param cm See Usage.
#' @param num_rows See Usage.
#' @param maxdef See Usage.
#' @param typelen See Usage.
#' @return The value of \code{[}.
#' @export
.pq_column_values <- function(con, cm, num_rows, maxdef, typelen) {
  ptype <- as.integer(.pq_f(cm, 1))
  codec <- as.integer(.pq_f(cm, 4))
  total_values <- .pq_f(cm, 5)
  data_off <- .pq_f(cm, 9)
  dict_off <- .pq_f(cm, 11)

  start <- if (!is.null(dict_off) && dict_off > 0 &&
    dict_off < data_off) {
    dict_off
  } else {
    data_off
  }
  seek(con, start)
  blob <- readBin(con, "raw", .pq_f(cm, 7) + 64)

  pos <- 1L
  dictionary <- NULL
  values <- list()
  got <- 0

  while (got < total_values && pos <= length(blob)) {
    r <- .pq_reader(blob, pos)
    head <- .pq_struct(r)
    pos <- r$pos
    csize <- .pq_f(head, 3)
    raw_page <- blob[seq.int(pos, length.out = csize)]
    pos <- pos + csize
    if (codec == .pqCSnappy) {
      page <- .pq_snappy_decompress(raw_page)
    } else if (codec == .pqCUncompressed) {
      page <- raw_page
    } else {
      stop("compression codec ", codec, " not implemented; the store ",
        "uses SNAPPY only",
        call. = FALSE
      )
    }

    ptype_page <- as.integer(.pq_f(head, 1))
    if (ptype_page == .pqPDict) {
      dh <- .pq_f(head, 7)
      dictionary <- .pq_decode_plain(
        page, 1L, ptype,
        as.integer(.pq_f(dh, 1))
      )$values
      next
    }
    if (ptype_page == .pqPDataV2) {
      stop("data page v2 not implemented (store is v1)", call. = FALSE)
    }
    if (ptype_page != .pqPData) next

    dph <- .pq_f(head, 5)
    n <- as.integer(.pq_f(dph, 1))
    encoding <- as.integer(.pq_f(dph, 2))
    p <- 1L

    if (maxdef > 0L) {
      width <- .pq_bit_width(maxdef)
      if (as.integer(.pq_f(dph, 3)) == .pqERle) {
        ln <- .pq_u32(page[seq.int(p, length.out = 4L)])
        p <- p + 4L
        rr <- .pq_read_rle(page, p, width, n, p + ln - 1L)
        defs <- rr$values
        p <- p + ln
      } else {
        stop("BIT_PACKED definition levels not implemented",
          call. = FALSE
        )
      }
    } else {
      defs <- rep(1L, n)
    }

    present <- sum(defs != 0L)
    if (encoding == .pqEPlainDict || encoding == .pqERleDict) {
      if (is.null(dictionary)) {
        stop("dictionary-encoded page with no dictionary page",
          call. = FALSE
        )
      }
      width <- as.integer(page[p])
      p <- p + 1L
      idx <- .pq_read_rle(page, p, width, present, length(page))$values
      vals <- dictionary[idx + 1L]
    } else if (encoding == .pqEPlain) {
      vals <- .pq_decode_plain(page, p, ptype, present, typelen)$values
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
      if (defs[i] != 0L) {
        j <- j + 1L
        col[[i]] <- if (is.list(vals)) vals[[j]] else vals[j]
      } else {
        col[i] <- list(NULL)
      }
    }
    values <- c(values, col)
    got <- got + n
  }

  if (length(values) < num_rows) {
    values <- c(values, vector("list", num_rows - length(values)))
  }
  values[seq_len(num_rows)]
}

#' Read a Parquet file
#'
#' Native Parquet reader: no nanoparquet, no arrow. Handles the v1
#' format with PLAIN, RLE and dictionary encodings, Snappy or no
#' compression. Nested and repeated columns are refused.
#'
#' @param path Path to a `.parquet` file.
#' @param columns Optional character vector of column names to decode;
#'   the rest are skipped entirely.
#' @return A `data.frame`.
#' @export
morie_read_parquet <- function(path, columns = NULL) {
  size <- file.info(path)$size
  if (is.na(size)) stop("no such file: ", path, call. = FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  meta <- .pq_read_footer(con, size)
  schema <- .pq_f(meta, 2)
  num_rows <- .pq_f(meta, 3)
  row_groups <- .pq_f(meta, 4)
  if (is.null(row_groups)) row_groups <- list()

  leaves <- list()
  for (i in seq.int(2L, length(schema))) {
    el <- schema[[i]]
    if (!is.null(.pq_f(el, 5))) {
      stop("nested schema not implemented: group field ",
        rawToChar(.pq_f(el, 4)), " has ", .pq_f(el, 5), " children",
        call. = FALSE
      )
    }
    rep_type <- .pq_f(el, 3, .pqRequired)
    if (rep_type == .pqRepeated) {
      stop("repeated column ", rawToChar(.pq_f(el, 4)), " not ",
        "implemented; decoding it as flat would silently change ",
        "the row count",
        call. = FALSE
      )
    }
    leaves[[length(leaves) + 1L]] <- list(
      name = rawToChar(.pq_f(el, 4)),
      type = as.integer(.pq_f(el, 1)),
      typelen = .pq_f(el, 2),
      converted = .pq_f(el, 6),
      maxdef = if (rep_type == .pqOptional) 1L else 0L
    )
  }

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
  for (i in wanted) {
    leaf <- leaves[[i]]
    col <- list()
    for (rg in row_groups) {
      chunk <- .pq_f(rg, 1)[[i]]
      cm <- .pq_f(chunk, 3)
      col <- c(col, .pq_column_values(
        con, cm, .pq_f(rg, 3),
        leaf$maxdef, leaf$typelen
      ))
    }
    if (leaf$type == .pqByteArray) {
      v <- .pq_convert(col, leaf$type, leaf$converted)
    } else {
      v <- vapply(col, function(x) {
        if (is.null(x)) NA_real_ else as.numeric(x)
      }, numeric(1))
      if (leaf$type == .pqBoolean) {
        v <- as.logical(v)
      } else if (leaf$type == .pqInt32) {
        v <- as.integer(v)
      }
      v <- .pq_apply_logical(v, leaf$type, leaf$converted)
    }
    out[[leaf$name]] <- v
  }

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
#' @param v See Usage.
#' @return A list with \code{type}, \code{converted}.
#' @export
.pq_infer <- function(v) {
  # Date and POSIXct must keep their logical type on the way out.
  # Without this a column read as TIMESTAMP_MICROS was written back as a
  # bare DOUBLE of seconds: readable, but no longer a timestamp to any
  # other engine, and off by a factor of 1e6 from where it started.
  if (inherits(v, "Date")) {
    return(list(type = .pqInt32, converted = .pqCtDate))
  }
  if (inherits(v, "POSIXct")) {
    return(list(type = .pqInt64, converted = .pqCtTsMillis))
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

#' .pq_prep_write
#'
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @param inf See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
.pq_prep_write <- function(v, inf) {
  if (inherits(v, "Date")) {
    return(as.integer(unclass(v)))
  }
  if (inherits(v, "POSIXct")) {
    return(round(as.numeric(v) * 1000))
  }
  v
}

#' WriteBin has no 64-bit integer; emit two 32-bit halves, folding the
#'
#' low half back into signed range because that is all writeBin takes.
#'
#' @param values See Usage.
#' @return The value of \code{writeBin}.
#' @export
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param values See Usage.
#' @param ptype See Usage.
#' @return Nothing; this branch always raises.
#' @export
.pq_encode_plain <- function(values, ptype) {
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
      b <- charToRaw(enc2utf8(as.character(values[[i]])))
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
#' Part of the parquet implementation; see the file header for the
#' source it follows.
#'
#' @param levels See Usage.
#' @param width See Usage.
#' @return A vector, from \code{c}.
#' @export
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
#' @param df A `data.frame`.
#' @param path Destination path.
#' @param compression `"snappy"` (default) or `NULL` for uncompressed.
#' @return `path`, invisibly.
#' @export
morie_write_parquet <- function(df, path, compression = "snappy") {
  if (!is.null(compression) && !identical(compression, "snappy")) {
    stop("compression must be \"snappy\" or NULL; got ",
      sQuote(compression),
      call. = FALSE
    )
  }
  codec <- if (is.null(compression)) .pqCUncompressed else .pqCSnappy

  nrows <- nrow(df)
  names_ <- names(df)
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("PAR1"), con)
  offset <- 4

  chunks <- list()
  for (k in seq_along(names_)) {
    v <- df[[k]]
    inf <- .pq_infer(v)
    defs <- as.integer(!is.na(v))
    present <- .pq_prep_write(v[!is.na(v)], inf)

    body <- c(
      .pq_encode_levels(defs, 1L),
      .pq_encode_plain(present, inf$type)
    )
    payload <- if (codec == .pqCSnappy) .pq_snappy_compress(body) else body

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
