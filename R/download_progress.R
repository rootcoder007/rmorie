# Download with live progress on stderr, one look across morie, rmorie,
# rmoriebricklayer and rmoriedata: a bar with percent, size and rate while a
# person is watching (an interactive session, or the command line, which sets
# options(morie.progress = TRUE)); milestone lines when stderr is not a
# terminal; nothing when quiet. options(morie.quiet = TRUE) silences it.

.morie_dl_quiet <- function() {
  isTRUE(getOption("morie.quiet")) ||
    !(interactive() || isTRUE(getOption("morie.progress")))
}

.morie_fmt_bytes <- function(n) {
  if (!is.finite(n)) return("?")
  units <- c("B", "KB", "MB", "GB")
  i <- 1L
  while (n >= 1024 && i < 4L) {
    n <- n / 1024
    i <- i + 1L
  }
  if (i == 1L) sprintf("%d B", as.integer(n)) else sprintf("%.1f %s", n, units[i])
}

.morie_dl_line <- function(label, got, size, t0, spin, unit = "B") {
  elapsed <- max(proc.time()[["elapsed"]] - t0, 1e-6)
  fmt <- .morie_fmt_bytes
  if (!identical(unit, "B")) {
    fmt <- function(n) paste(format(round(n), big.mark = ",", scientific = FALSE), unit)
  }
  rate <- paste0(fmt(got / elapsed), "/s")
  if (is.finite(size) && size > 0) {
    pct <- min(100L, as.integer((100 * got) %/% size))
    n <- pct %/% 4L
    sprintf("%s  [%s%s] %3d%%  %s / %s  %s", label, strrep("#", n),
            strrep(".", 25L - n), pct, fmt(got), fmt(size), rate)
  } else {
    sprintf("%s  %s %s  %s", label, c("|", "/", "-", "\\")[spin %% 4L + 1L],
            fmt(got), rate)
  }
}

.morie_dl <- function(url, dest, headers = NULL, label = basename(dest),
                      size = NULL, timeout = 3600, quiet = NULL, tty = NULL,
                      attempts = 3L) {
  # a transfer that drops part-way is started again (base R's url() does not
  # expose the status, so a Range resume could silently append a duplicate body)
  if (is.null(quiet)) quiet <- .morie_dl_quiet()
  for (attempt in seq_len(attempts)) {
    res <- tryCatch(
      .morie_dl_once(url, dest, headers, label, size, timeout, quiet, tty),
      error = function(e) e
    )
    if (!inherits(res, "error")) return(res)
    if (attempt == attempts) stop(res)
    if (!quiet) {
      cat(sprintf("%s: the transfer dropped (%s); retrying, attempt %d of %d\n",
                  label, conditionMessage(res), attempt + 1L, attempts),
          file = stderr())
    }
    unlink(dest)
    Sys.sleep(attempt)
  }
}

.morie_dl_once <- function(url, dest, headers, label, size, timeout, quiet, tty) {
  size <- suppressWarnings(as.numeric(if (is.null(size)) NA else size[[1L]]))
  if (!is.finite(size) || size <= 0) size <- NA_real_
  old <- options(timeout = max(getOption("timeout", 60), timeout))
  on.exit(options(old), add = TRUE)
  if (is.null(tty)) tty <- isatty(stderr())
  if (is.null(headers)) {
    con <- url(url, open = "rb")
  } else {
    con <- url(url, open = "rb", headers = headers)
  }
  on.exit(close(con), add = TRUE)
  out <- file(dest, open = "wb")
  on.exit(close(out), add = TRUE)
  got <- 0
  t0 <- proc.time()[["elapsed"]]
  last <- t0
  mile <- 0L
  spin <- 0L
  width <- 0L
  if (!quiet && !tty) {
    cat(sprintf("%s: downloading%s\n", label,
                if (is.finite(size)) paste0(" ", .morie_fmt_bytes(size)) else ""),
        file = stderr())
  }
  repeat {
    chunk <- readBin(con, what = "raw", n = 1048576L)
    if (!length(chunk)) break
    writeBin(chunk, out)
    got <- got + length(chunk)
    if (quiet) next
    if (tty) {
      now <- proc.time()[["elapsed"]]
      # the first chunk draws at once, then ten frames a second
      if (spin > 0L && now - last < 0.1) next
      last <- now
      spin <- spin + 1L
      line <- .morie_dl_line(label, got, size, t0, spin)
      pad <- strrep(" ", max(0L, width - nchar(line)))
      cat("\r", line, pad, sep = "", file = stderr())
      width <- max(width, nchar(line))
    } else {
      if (is.finite(size)) {
        step <- as.integer((10 * got) %/% size)
      } else {
        step <- as.integer(got %/% 52428800)
      }
      if (step > mile) {
        mile <- step
        line <- .morie_dl_line(label, got, size, t0, 0L)
        cat("  ", line, "\n", sep = "", file = stderr())
      }
    }
  }
  if (!quiet) {
    if (tty) cat("\r", strrep(" ", width), "\r", sep = "", file = stderr())
    cat(sprintf("%s: %s in %.0f s\n", label, .morie_fmt_bytes(got),
                proc.time()[["elapsed"]] - t0), file = stderr())
  }
  invisible(dest)
}
