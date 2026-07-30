#!/usr/bin/env Rscript
# Fail if any morie_* / .morie_* symbol is CALLED in R/ but defined nowhere.
#
# This is the exact failure that broke as-cran CI on 2026-07-30: an exported
# alias `morie_bt_632()` forwarded to `morie_esl_oob_632()`, which had never
# been written. Nothing catches that until R CMD check runs the examples,
# which is 20 minutes into CI and only on a runner. It is a two-second
# static check.
#
# Usage: Rscript scripts/audit/check_r_undefined.R [pkg-root]
# Exit 1 on any undefined symbol.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else "."
rdir <- file.path(root, "R")
if (!dir.exists(rdir)) stop("no R/ directory under ", root)

files <- list.files(rdir, pattern = "[.][Rr]$", full.names = TRUE)
exprs <- unlist(lapply(files, function(f) as.list(parse(f, keep.source = FALSE))))

# Every name assigned at top level, however it was assigned.
defined <- character(0)
for (e in exprs) {
  if (is.call(e) && length(e) >= 2L && is.name(e[[1L]]) &&
      as.character(e[[1L]]) %in% c("<-", "=", "<<-", "assign")) {
    tgt <- e[[2L]]
    if (is.name(tgt) || is.character(tgt)) defined <- c(defined, as.character(tgt))
  }
}

# Names registered from compiled code or re-exported also count as defined.
ns <- file.path(root, "NAMESPACE")
if (file.exists(ns)) {
  txt <- readLines(ns, warn = FALSE)
  hits <- regmatches(txt, gregexpr("[.]?morie[A-Za-z0-9._]*", txt))
  defined <- c(defined, unlist(hits))
}
# useDynLib(..., .registration) entry points: anything the C layer exports.
src <- file.path(root, "src")
if (dir.exists(src)) {
  cfiles <- list.files(src, pattern = "[.](c|cc|cpp|h|hpp)$", full.names = TRUE)
  for (f in cfiles) {
    txt <- readLines(f, warn = FALSE)
    hits <- regmatches(txt, gregexpr('"[.]?morie[A-Za-z0-9._]*"', txt))
    defined <- c(defined, gsub('"', "", unlist(hits)))
  }
}
# A call guarded by exists("f") is a deliberate optional hook, not a typo.
for (f in files) {
  txt <- readLines(f, warn = FALSE)
  hits <- regmatches(txt, gregexpr('exists[(] *"[.]?morie[A-Za-z0-9._]*"', txt))
  defined <- c(defined, gsub('.*"', "", gsub('"$', "", unlist(hits))))
}
defined <- unique(defined)

# Every morie symbol that appears in a call position.
called <- new.env(parent = emptyenv())
walk <- function(e) {
  if (!is.call(e)) return(invisible(NULL))
  fn <- e[[1L]]
  if (is.name(fn)) {
    nm <- as.character(fn)
    if (grepl("^[.]?morie", nm)) assign(nm, TRUE, envir = called)
  }
  # as.list() keeps empty arguments (x[i, ]) from erroring on [[.
  parts <- as.list(e)
  for (i in seq_along(parts)) {
    # An empty argument (as in x[i, ]) is the missing symbol: storing it is
    # fine, touching it errors, so the test itself has to be guarded.
    ok <- tryCatch(is.call(parts[[i]]), error = function(...) FALSE)
    if (isTRUE(ok)) walk(parts[[i]])
  }
  invisible(NULL)
}
invisible(lapply(exprs, walk))

missing <- setdiff(ls(called), defined)
if (length(missing)) {
  cat("UNDEFINED morie symbols called from R/ (", length(missing), "):\n", sep = "")
  for (m in sort(missing)) {
    where <- system2("grep", c("-rln", shQuote(paste0("\\b", m, "\\b")), shQuote(rdir)),
                     stdout = TRUE, stderr = FALSE)
    cat("  ", m, "  <- ", paste(basename(where), collapse = ", "), "\n", sep = "")
  }
  quit(status = 1L)
}
cat("R undefined-symbol check: OK (", length(ls(called)), " morie calls, all defined)\n", sep = "")
