# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal helper: the data sheet of a workbook (cover sheets skipped, the most cells wins)
#'
#' CIHI and other publishers put an "Instructions" or "Notes to readers" sheet first; the
#' data table is a later sheet. A workbook over 50 MB is streamed instead (readxl builds the
#' whole sheet in memory: the 93 MB CIHI indicator library needs more than 5 GB).
#' @noRd
.morie_xlsx_data_sheet <- function(path, ...) {
  if (file.size(path) > 50e6) return(.morie_xlsx_stream(path))
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read xlsx data: install.packages(\"readxl\")", call. = FALSE)
  }
  sheets <- readxl::excel_sheets(path)
  cover <- "^(instructions?|notes?( to readers?)?|(table of )?contents|about|read ?me|cover|footnotes?|glossary|definitions|methodology)$"
  data_sheets <- sheets[!grepl(cover, trimws(sheets), ignore.case = TRUE)]
  if (!length(data_sheets)) data_sheets <- sheets
  best <- NULL
  for (nm in data_sheets) {
    # readxl names blank header cells ...17, ...18 and says so for each: that is not news here
    df <- tryCatch(suppressMessages(as.data.frame(readxl::read_excel(path, sheet = nm, ...))), error = function(e) NULL)
    if (!is.null(df) && (is.null(best) || nrow(df) * ncol(df) > nrow(best) * ncol(best))) {
      best <- df
      attr(best, "morie_sheet") <- nm
    }
  }
  if (is.null(best)) stop("no readable sheet in ", basename(path), call. = FALSE)
  best
}

#' Internal helper: stream the first sheet of a large workbook to CSV, then read the CSV
#'
#' Reads the sheet XML out of the zip in 4 MB pieces and parses whole rows with vectorised
#' regular expressions, so memory stays near the size of the result, not of the XML tree.
#' The intermediate CSV goes to the user cache (a download sits in tempdir(), which on some
#' systems is RAM) and is removed once read; the caller caches the table itself.
#' @noRd
.morie_xlsx_stream <- function(path) {
  dir.create(morie_cache_dir("xlsx"), recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(morie_cache_dir("xlsx"), sub("\\.xlsx$", ".csv", basename(path), ignore.case = TRUE))
  on.exit(unlink(csv), add = TRUE)
  members <- utils::unzip(path, list = TRUE)$Name
  rd <- function(m) {
    con <- unz(path, m, open = "rb")
    on.exit(close(con))
    out <- character()
    repeat {
      piece <- readChar(con, 16e6, useBytes = TRUE)
      if (!length(piece) || !nzchar(piece)) break
      out <- c(out, piece)
    }
    paste(out, collapse = "")
  }
  unxml <- function(x) {
    x <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;", "\"", gsub("&apos;", "'", x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
    x <- gsub("&#10;", "\n", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    if (any(grepl("&#", x, fixed = TRUE))) {
      m <- gregexpr("&#(x[0-9A-Fa-f]+|[0-9]+);", x, perl = TRUE)
      regmatches(x, m) <- lapply(regmatches(x, m), function(e) vapply(e, function(s) {
        v <- substr(s, 3L, nchar(s) - 1L)
        intToUtf8(if (startsWith(v, "x")) strtoi(substring(v, 2L), 16L) else as.integer(v))
      }, ""))
    }
    x
  }
  # the first sheet in workbook order, through the relationship map
  wb <- rd("xl/workbook.xml")
  rid <- sub('(?s)^.*?<sheet [^>]*?r:id="([^"]+)".*$', "\\1", wb, perl = TRUE)
  rels <- rd("xl/_rels/workbook.xml.rels")
  target <- regmatches(rels, regexpr(sprintf('<Relationship [^>]*Id="%s"[^>]*>', rid), rels))
  target <- sub('^.*Target="([^"]+)".*$', "\\1", target)
  sheet <- if (length(target)) sub("^/", "", if (startsWith(target, "/")) target else paste0("xl/", target)) else "xl/worksheets/sheet1.xml"
  shared <- character()
  if ("xl/sharedStrings.xml" %in% members) {
    ss <- rd("xl/sharedStrings.xml")
    si <- regmatches(ss, gregexpr("(?s)<si>.*?</si>", ss, perl = TRUE, useBytes = TRUE))[[1L]]
    si <- gsub("(?s)<rPh.*?</rPh>", "", si, perl = TRUE, useBytes = TRUE)  # phonetic runs are not text
    Encoding(si) <- "UTF-8"
    shared <- unxml(gsub("<[^>]+>", "", si))
    rm(ss, si)
  }
  letters_to_col <- function(l) {
    u <- unique(l)
    v <- vapply(strsplit(u, ""), function(ch) Reduce(function(a, b) a * 26L + b, match(ch, LETTERS)), 1L)
    v[match(l, u)]
  }
  tmp <- paste0(csv, ".part")
  out <- file(tmp, "wb")  # raw UTF-8 bytes: a text connection re-encodes to the locale (C gives "<U+00E9>")
  con <- unz(path, sheet, open = "rb")
  open_cons <- TRUE
  on.exit(if (open_cons) {
    close(out)
    close(con)
  }, add = TRUE)
  on.exit(unlink(tmp), add = TRUE)  # gone after the rename; a half-written file when interrupted
  ncol_max <- NA_integer_
  buf <- ""
  done <- 0L
  repeat {
    piece <- readChar(con, 4e6, useBytes = TRUE)
    eof <- !length(piece) || !nzchar(piece)
    if (!eof) buf <- paste0(buf, piece)
    Encoding(buf) <- "bytes"  # a piece can end inside a UTF-8 character: cut on byte offsets
    cut <- if (eof) nchar(buf, type = "bytes") else {
      ends <- gregexpr("</row>", buf, fixed = TRUE, useBytes = TRUE)[[1L]]
      if (ends[[1L]] < 0L) next
      ends[[length(ends)]] + 5L
    }
    txt <- substr(buf, 1L, cut)
    buf <- substr(buf, cut + 1L, nchar(buf, type = "bytes"))
    Encoding(txt) <- "UTF-8"  # whole rows: complete characters again
    cells <- regmatches(txt, gregexpr('(?s)<c r="[A-Z]+[0-9]+"[^>]*?(/>|>.*?</c>)', txt, perl = TRUE, useBytes = TRUE))[[1L]]
    Encoding(cells) <- "UTF-8"
    if (length(cells)) {
      ref <- sub('^<c r="([A-Z]+[0-9]+)".*$', "\\1", substr(cells, 1L, 40L))
      row <- as.integer(sub("^[A-Z]+", "", ref))
      col <- letters_to_col(sub("[0-9]+$", "", ref))
      head <- sub("(?s)>.*$", "", cells, perl = TRUE)
      val <- ifelse(grepl("<v>", cells, fixed = TRUE), sub("(?s)^.*?<v>(.*?)</v>.*$", "\\1", cells, perl = TRUE), "")
      is_s <- grepl('\\bt="s"', head)
      val[is_s] <- shared[as.integer(val[is_s]) + 1L]
      is_i <- grepl('t="inlineStr"', head, fixed = TRUE)
      val[is_i] <- gsub("<[^>]+>", "", sub("(?s)^.*?<is>(.*?)</is>.*$", "\\1", cells[is_i], perl = TRUE))
      val[!is_s] <- unxml(val[!is_s])
      if (is.na(ncol_max)) ncol_max <- max(col)  # the header row fixes the width
      keep <- col <= ncol_max
      rows <- sort(unique(row))
      m <- matrix("", length(rows), ncol_max)
      m[cbind(match(row[keep], rows), col[keep])] <- val[keep]
      q <- matrix(paste0('"', gsub('"', '""', enc2utf8(m), fixed = TRUE), '"'), nrow(m))
      writeLines(do.call(paste, c(asplit(q, 2L), sep = ",")), out, useBytes = TRUE)
      done <- done + length(rows)
      if (isTRUE(getOption("morie.progress"))) message(sprintf("\r%s: %s rows to CSV", basename(path), format(done, big.mark = ",")), appendLF = FALSE)
    }
    if (eof) break
  }
  if (isTRUE(getOption("morie.progress"))) message("")
  close(out)
  close(con)
  open_cons <- FALSE
  file.rename(tmp, csv)
  # rows with no cells at all (<row r="9"/>, formatting only) are not data
  utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}
