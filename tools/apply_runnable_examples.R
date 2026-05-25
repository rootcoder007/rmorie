# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/apply_runnable_examples.R
#
# Companion to tools/generate_runnable_examples.R.
# Reads tools/runnable_examples.json and rewrites the @examples block of each
# function whose status == "ok", replacing the 5-line boilerplate dontrun
# block with the generated runnable call.
#
# Usage:  Rscript tools/apply_runnable_examples.R

suppressMessages(library(jsonlite))

js <- jsonlite::read_json("tools/runnable_examples.json")
ok <- Filter(
  function(v) identical(v$status, "ok") && nzchar(v$call %||% ""),
  js
)
cat(sprintf("loading %d runnable examples\n", length(ok)))

# Map fn -> source file (function definitions in R/)
def_rx <- "^([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*function"
fn_to_file <- list()
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  for (line in readLines(f, warn = FALSE)) {
    m <- regmatches(line, regexec(def_rx, line))[[1]]
    if (length(m) >= 2 && is.null(fn_to_file[[m[2]]])) {
      fn_to_file[[m[2]]] <- f
    }
  }
}

# Line-wise pattern for the 5-line boilerplate block:
#   #' @examples
#   #' \dontrun{
#   #' # See the package vignettes for usage examples:
#   #' #   vignette(package = "morie")
#   #' }
is_boiler <- function(src, i) {
  if (i + 4L > length(src)) return(FALSE)
  grepl("^#'\\s*@examples\\s*$",                         src[i])      &&
    grepl("^#'\\s*\\\\dontrun\\{\\s*$",                  src[i + 1L]) &&
    grepl("See the package vignettes for usage",         src[i + 2L]) &&
    grepl("vignette\\(package\\s*=\\s*\"morie\"\\)",     src[i + 3L]) &&
    grepl("^#'\\s*\\}\\s*$",                             src[i + 4L])
}

applied <- 0L
files_touched <- character()

for (fname in names(ok)) {
  fpath <- fn_to_file[[fname]]
  if (is.null(fpath)) next
  src <- readLines(fpath, warn = FALSE)
  new <- character(0)
  i <- 1L
  did_one <- FALSE
  while (i <= length(src)) {
    if (!did_one && is_boiler(src, i)) {
      # peek forward up to ~25 lines for the next function definition; only
      # rewrite if the next definition is our fname (so we hit the right block
      # in files that define multiple exports).
      tail <- src[i + 5L:min(i + 29L, length(src))]
      m <- regmatches(tail, regexec(def_rx, tail))
      next_def <- vapply(
        m, function(mm) if (length(mm) >= 2) mm[2] else NA_character_, character(1)
      )
      next_def <- na.omit(next_def)
      if (length(next_def) && identical(next_def[1], fname)) {
        new <- c(new, "#' @examples", paste0("#' ", ok[[fname]]$call))
        i <- i + 5L
        applied <- applied + 1L
        did_one <- TRUE
        files_touched <- union(files_touched, fpath)
        next
      }
    }
    new <- c(new, src[i]); i <- i + 1L
  }
  writeLines(new, fpath)
}

cat(sprintf("applied %d replacements across %d files\n", applied, length(files_touched)))

# %||% helper (rlang-free, base R)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
