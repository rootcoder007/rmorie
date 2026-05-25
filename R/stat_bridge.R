# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bridge between external runners and the morie R command registry
#'
#' R port of \code{morie.stat_bridge}. Exposes the same three modes
#' available on the Python side --- registry enumeration, a formatted
#' help dump, and command execution --- so an external runner (e.g.
#' the Go TIDE TUI, a shell pipeline) can drive morie's R surface via
#' \code{Rscript -e 'morie::stat_bridge_main(...)'}.
#'
#' Two layers are provided:
#'
#' \enumerate{
#'   \item Programmatic helpers (\code{stat_bridge_registry_json},
#'     \code{stat_bridge_help}, \code{stat_bridge_exec}) callable from
#'     ordinary R code.
#'   \item A dispatcher (\code{stat_bridge_main}) that mimics the
#'     command-line entry point of the Python module so the same
#'     invocation pattern works from either runtime.
#' }
#'
#' @name morie_stat_bridge
NULL


#' JSON enumeration of all registered commands
#'
#' @return A length-1 character vector containing JSON text.
#' @export
stat_bridge_registry_json <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required for registry-json output")
  }
  reg <- .morie_stat_commands$registry
  entries <- lapply(reg, function(cmd) {
    list(
      short = cmd$name,
      full = cmd$name,
      category = cmd$category,
      description = cmd$description,
      usage = cmd$usage,
      aliases = cmd$aliases,
      module = cmd$module
    )
  })
  # Stable ordering by name.
  entries <- entries[order(names(entries))]
  jsonlite::toJSON(unname(entries), auto_unbox = TRUE, null = "null")
}


#' Formatted text dump of the command registry
#'
#' @return A length-1 character string.
#' @export
stat_bridge_help <- function() {
  reg <- .morie_stat_commands$registry
  cats <- list()
  for (cmd in reg) {
    cats[[cmd$category]] <- c(cats[[cmd$category]], cmd$name)
  }
  lines <- c("MORIE Statistical Functions (R surface)",
             paste(rep("=", 40L), collapse = ""), "")
  for (cat_name in sort(names(cats))) {
    fns <- sort(cats[[cat_name]])
    lines <- c(lines, sprintf("%s (%d)", cat_name, length(fns)))
    for (i in seq(1L, length(fns), by = 8L)) {
      chunk <- fns[i:min(i + 7L, length(fns))]
      lines <- c(lines, paste0("  ", paste(chunk, collapse = ", ")))
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, sprintf("Total: %d functions", length(reg)))
  paste(lines, collapse = "\
")
}


# Bridge log class used to capture handler output.
.bridge_log <- function() {
  parts <- character(0)
  list(
    write = function(msg) {
      parts <<- c(parts, as.character(msg))
    },
    call = function(msg) {
      parts <<- c(parts, as.character(msg))
    },
    getvalue = function() paste(parts, collapse = "\
")
  )
}


#' Execute a single command and return the resulting text
#'
#' @param cmd_str A whitespace-delimited command line, e.g.
#'   \code{"bonferroni 0.01 0.04 0.05"}.
#' @return Captured handler output as a single string.
#' @export
stat_bridge_exec <- function(cmd_str) {
  parts <- strsplit(trimws(cmd_str), "\\s+")[[1]]
  if (length(parts) == 0L) {
    return("Error: empty command")
  }
  name <- parts[1L]
  cmd <- resolve_stat_command(name)
  if (is.null(cmd)) {
    return(sprintf("Unknown command: %s", name))
  }
  log <- .bridge_log()
  store <- function(...) invisible(NULL)
  result <- tryCatch(
    cmd$handler_stat(parts, log, store),
    error = function(e) {
      log$write(sprintf("Error: %s", conditionMessage(e)))
      NULL
    }
  )
  # Append a default printed form of the result if the handler did not
  # explicitly write to the log.
  captured <- log$getvalue()
  if (!nzchar(captured) && !is.null(result)) {
    captured <- paste(utils::capture.output(print(result)), collapse = "\
")
  }
  trimws(captured)
}


#' Inspect a single command by name
#'
#' @param name Command name or alias.
#' @return Multi-line description string or an explanatory error string.
#' @export
stat_bridge_fn_info <- function(name) {
  cmd <- resolve_stat_command(name)
  if (is.null(cmd)) {
    return(sprintf("Not found: %s", name))
  }
  paste(
    sprintf("%s (%s)", cmd$name, cmd$category),
    sprintf("Usage: %s", cmd$usage),
    cmd$description,
    if (length(cmd$aliases) > 0L) {
      sprintf("Aliases: %s", paste(cmd$aliases, collapse = ", "))
    } else {
      "Aliases: (none)"
    },
    sep = "\
"
  )
}


#' Search the registry for matching commands
#'
#' @param query Free-text query; matched against names, categories,
#'   descriptions, and aliases.
#' @param max_results Cap on the number of matches returned.
#' @return Multi-line summary string.
#' @export
stat_bridge_fn_search <- function(query, max_results = 20L) {
  q <- tolower(as.character(query))
  reg <- .morie_stat_commands$registry
  matches <- character(0)
  for (cmd in reg) {
    haystack <- tolower(paste(c(
      cmd$name, cmd$category, cmd$description, cmd$aliases
    ), collapse = " "))
    if (grepl(q, haystack, fixed = TRUE)) {
      matches <- c(matches, sprintf(
        "  %-12s %-18s %s",
        cmd$name, cmd$category, cmd$description
      ))
    }
  }
  if (length(matches) == 0L) {
    return("No matches.")
  }
  if (length(matches) > max_results) {
    matches <- matches[seq_len(max_results)]
  }
  paste(c(matches, sprintf("\
%d results", length(matches))),
        collapse = "\
")
}


#' Self-test enumeration helper
#'
#' Calls every registered handler with no arguments inside
#' \code{tryCatch}, reporting which entries can be invoked safely.
#' Intended to be called from CI smoke tests.
#'
#' @return A data.frame with columns \code{name}, \code{ok}, \code{message}.
#' @export
stat_bridge_verify <- function() {
  reg <- .morie_stat_commands$registry
  rows <- vector("list", length(reg))
  i <- 1L
  for (cmd in reg) {
    res <- tryCatch({
      cmd$handler_repl()
      list(ok = TRUE, msg = "")
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    rows[[i]] <- data.frame(name = cmd$name, ok = res$ok,
                            message = res$msg, stringsAsFactors = FALSE)
    i <- i + 1L
  }
  do.call(rbind, rows)
}


#' Command-line dispatcher
#'
#' Mirrors \code{python -m morie.stat_bridge <mode> [...]} so the same
#' invocation pattern is available via \code{Rscript -e}.
#'
#' Recognised modes: \code{"registry-json"}, \code{"help"},
#' \code{"exec"}, \code{"fn-info"}, \code{"fn-search"}, \code{"verify"}.
#'
#' @param args Character vector of CLI arguments (mode + parameters).
#'   When \code{NULL}, defaults to \code{commandArgs(trailingOnly = TRUE)}.
#' @return Invisibly returns the printed text; primarily called for
#'   side effects (printing to stdout).
#' @export
stat_bridge_main <- function(args = NULL) {
  if (is.null(args)) {
    args <- commandArgs(trailingOnly = TRUE)
  }
  if (length(args) == 0L) {
    cat("Usage: stat_bridge_main(c(\"registry-json\" | \"help\" | \"exec\" | \"fn-info\" | \"fn-search\" | \"verify\", ...))\
")
    return(invisible(NULL))
  }
  mode <- args[1L]
  rest <- if (length(args) > 1L) args[-1L] else character(0)
  out <- switch(
    mode,
    "registry-json" = stat_bridge_registry_json(),
    "help" = stat_bridge_help(),
    "exec" = {
      if (length(rest) == 0L) {
        "Error: exec requires a command string"
      } else {
        stat_bridge_exec(paste(rest, collapse = " "))
      }
    },
    "fn-info" = {
      if (length(rest) == 0L) {
        "Error: fn-info requires a name"
      } else {
        stat_bridge_fn_info(rest[1L])
      }
    },
    "fn-search" = {
      if (length(rest) == 0L) {
        "Error: fn-search requires a query"
      } else {
        stat_bridge_fn_search(paste(rest, collapse = " "))
      }
    },
    "verify" = {
      df <- stat_bridge_verify()
      paste(
        apply(df, 1L, function(r) {
          sprintf("  %s  %s%s",
                  if (r[["ok"]] == "TRUE") "PASS" else "FAIL",
                  r[["name"]],
                  if (nzchar(r[["message"]])) sprintf(" -- %s", r[["message"]]) else "")
        }),
        collapse = "\
"
      )
    },
    sprintf("Unknown mode: %s", mode)
  )
  cat(out, "\
", sep = "")
  invisible(out)
}
