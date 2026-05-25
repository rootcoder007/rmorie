# SPDX-License-Identifier: AGPL-3.0-or-later
# tools/unwrap_runnable_dontrun.R
#
# Many remaining `\dontrun{...}` blocks contain REAL example code that
# happens to be wrapped (legacy: "I'm not sure this will run, so dontrun
# to be safe"). This script:
#   1. Finds every remaining `#' \dontrun{ ... #' }` block in R/*.R
#   2. Extracts the inner R code
#   3. Runs it in a fresh callr::r() subprocess with a 5-s timeout
#   4. If it runs cleanly: rewrites the source file to drop the
#      `\dontrun{...}` wrapping (the inner lines stay as plain
#      `#' <code>` lines)
#   5. If it errors / times out / hits network: leaves untouched
#
# Network/file-only legit-dontrun cases are converted to `\donttest{}`
# at the end based on an allowlist (see NETWORK_FNS).
#
# Usage:  Rscript tools/unwrap_runnable_dontrun.R [--apply]

suppressMessages({
  library(morie)
  library(callr)
})

ARGS <- commandArgs(trailingOnly = TRUE)
APPLY <- "--apply" %in% ARGS

# Network/file-only functions: convert dontrun -> donttest (their inner
# code IS runnable but hits network; donttest is the CRAN-policy escape).
NETWORK_FNS <- c(
  "morie_fetch", "morie_ckan_search", "morie_fetch_arcgis",
  "morie_load_cpads", "morie_fetch_ckan", "morie_load_dataset",
  "morie_download_bootstrap", "morie_fetch_tps", "morie_fetch_siu",
  "morie_check_plugin_license"
)

# ---- discover all \dontrun blocks ----------------------------------------
discover <- function(file) {
  src <- readLines(file, warn = FALSE)
  blocks <- list()
  i <- 1L
  while (i <= length(src)) {
    if (grepl("^#'\\s*\\\\dontrun\\{\\s*$", src[i])) {
      start <- i; j <- i + 1L
      while (j <= length(src) && !grepl("^#'\\s*\\}\\s*$", src[j])) j <- j + 1L
      if (j <= length(src)) {
        end <- j
        # find next function definition after end (next 30 lines)
        fn_name <- NA_character_
        for (k in (end + 1L):min(end + 30L, length(src))) {
          m <- regmatches(src[k], regexec(
            "^([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*function", src[k]
          ))[[1]]
          if (length(m) >= 2) { fn_name <- m[2]; break }
        }
        inner <- src[(start + 1L):(end - 1L)]
        # strip leading "#' " on each inner line
        code <- sub("^#'\\s?", "", inner)
        blocks[[length(blocks) + 1L]] <- list(
          file = file, start = start, end = end,
          fn = fn_name, code = code
        )
        i <- end + 1L
        next
      }
    }
    i <- i + 1L
  }
  blocks
}

all_blocks <- list()
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  all_blocks <- c(all_blocks, discover(f))
}
cat(sprintf("found %d \\dontrun blocks across R/*.R\n", length(all_blocks)))

# ---- probe each block ----------------------------------------------------
results <- list()
for (i in seq_along(all_blocks)) {
  b <- all_blocks[[i]]
  if (is.na(b$fn)) { results[[i]] <- list(b = b, status = "no-fn"); next }
  if (b$fn %in% NETWORK_FNS) {
    results[[i]] <- list(b = b, status = "network")
    next
  }
  # join code lines
  expr_src <- paste(b$code, collapse = "\n")
  if (!nzchar(gsub("\\s|#.*", "", expr_src))) {
    results[[i]] <- list(b = b, status = "empty")
    next
  }
  status <- tryCatch({
    callr::r(
      function(src) {
        suppressMessages(library(morie))
        set.seed(1)
        eval(parse(text = src), envir = new.env())
        TRUE
      },
      args = list(src = expr_src),
      timeout = 5
    )
    "ok"
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("timeout|elapsed", msg, ignore.case = TRUE)) "timeout" else "error"
  })
  results[[i]] <- list(b = b, status = status)
  if (i %% 10 == 0) cat(sprintf("  probed %d/%d\n", i, length(all_blocks)))
}

# ---- summary -------------------------------------------------------------
status_counts <- table(vapply(results, function(r) r$status, character(1)))
cat("\n=== probe results ===\n")
print(status_counts)

# ---- apply ---------------------------------------------------------------
if (APPLY) {
  cat("\n=== applying ===\n")
  by_file <- split(results, vapply(results, function(r) r$b$file, character(1)))
  total_unwrap <- 0L; total_donttest <- 0L
  for (file in names(by_file)) {
    src <- readLines(file, warn = FALSE)
    blocks <- by_file[[file]]
    # process in reverse line order so edits don't shift later line numbers
    blocks <- blocks[order(-vapply(blocks, function(r) r$b$start, integer(1)))]
    for (r in blocks) {
      b <- r$b
      if (r$status %in% c("ok", "empty")) {
        # Unwrap: drop the \dontrun{ wrapper. Inner lines (real code OR
        # just `# See the vignettes` comments) become regular @examples
        # content. R parses comments without error; pkgcheck stops
        # counting these as `\dontrun{}` blocks.
        new_lines <- paste0("#' ", b$code)
        src <- c(src[seq_len(b$start - 1L)], new_lines, src[seq.int(b$end + 1L, length(src))])
        total_unwrap <- total_unwrap + 1L
      } else if (r$status == "network") {
        # Rewrap dontrun -> donttest (file-line-preserving)
        src[b$start] <- sub("\\\\dontrun", "\\\\donttest", src[b$start])
        total_donttest <- total_donttest + 1L
      }
      # error/timeout: leave untouched (genuinely broken examples)
    }
    writeLines(src, file)
  }
  cat(sprintf("unwrapped %d, converted %d to \\donttest\n",
              total_unwrap, total_donttest))
}
