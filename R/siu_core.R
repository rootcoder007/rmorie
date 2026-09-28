# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R front end of the native SIU core (src/siu/, src/siu_core_*.cpp): the C++17
# parser, zero-wrong subject-official resolver, schema, libcurl fetch, a
# bring-your-own-model LLM backend and the Mixture-of-Agents audit panel. The
# same sources build the standalone `siu` CLI (tools/siu-cli), so the package
# and the CLI cannot drift.

# Which engine runs the deterministic SIU core: rmoriebricklayer (the shared
# compiled foundation) when it provides `fn`, else rmorie's own native copy of
# the same C++ core (src/siu/) -- a fallback so the SIU surface keeps working
# whether or not bricklayer is installed or current.
# `engine = "auto"` uses bricklayer only from the version that carries every
# fix in the native core: the parser and text extraction here normalise CRLF
# (live pages), read the real signature block ("Public Reports" navigation was
# taken for the director on every live report) and drop incident headlines as
# police-service names -- fixes not yet in bricklayer 0.5.x.
.siu_bricklayer_min <- c(bricklayer_parse_siu = "0.6.0", bricklayer_siu_text = "0.6.0")

.siu_use_bricklayer <- function(engine, fn) {
  engine <- match.arg(engine, c("auto", "bricklayer", "native"))
  if (engine == "native") return(FALSE)
  ok <- requireNamespace("rmoriebricklayer", quietly = TRUE) &&
    exists(fn, envir = asNamespace("rmoriebricklayer"), inherits = FALSE)
  if (engine == "auto" && ok && fn %in% names(.siu_bricklayer_min)) {
    ok <- utils::packageVersion("rmoriebricklayer") >= .siu_bricklayer_min[[fn]]
  }
  if (engine == "bricklayer" && !ok) {
    stop("engine = \"bricklayer\" needs rmoriebricklayer with ", fn, "()", call. = FALSE)
  }
  ok
}

.siu_backend_args <- function(api, base, key) {
  list(api = if (is.null(api)) "" else as.character(api),
       base = if (is.null(base)) "" else as.character(base),
       key = if (is.null(key)) "" else as.character(key))
}

#' SIU schema, text, dates and report fetch (bricklayer or native core)
#'
#' The deterministic SIU core runs in \pkg{rmoriebricklayer} when it is
#' installed and current (\code{engine = "auto"}), else in this package's own
#' compiled copy of the same C++ core -- \code{engine = "bricklayer"} or
#' \code{"native"} forces one. Text extraction and parsing need
#' bricklayer >= 0.6.0 under \code{"auto"}: the native core additionally
#' normalises CRLF line endings and reads the real signature block. \code{morie_siu_schema()} lists the 16 panel-reviewed fields
#' (count fields flagged: 0 is a real answer). \code{morie_siu_html_to_text()}
#' strips tags, scripts and entities while keeping line structure.
#' \code{morie_siu_to_iso_date()} converts "January 5, 2023" to
#' "2023-01-05". \code{morie_siu_strip_boilerplate()} removes the privacy
#' paragraph and the witness-officer glossary note (native core).
#' \code{morie_siu_report_url()} and \code{morie_siu_fetch_report()} build
#' and fetch (libcurl) a report page. Parsing and subject-official counts:
#' \code{\link{morie_siu_parse_report}}, \code{\link{morie_siu_resolve_so}}.
#'
#' @param html Report HTML (character scalar).
#' @param text Plain report text.
#' @param x Character vector of human-readable dates.
#' @param drid Director's report id (positive integer).
#' @param timeout Request timeout in seconds.
#' @param engine \code{"auto"}, \code{"bricklayer"} or \code{"native"}.
#' @return Data frame (schema) or character (text, dates, url, fetch).
#' @examples
#' morie_siu_schema(engine = "native")$name
#' morie_siu_to_iso_date(c("January 5, 2023", "not a date"), engine = "native")
#' @export
morie_siu_schema <- function(engine = "auto") {
  if (.siu_use_bricklayer(engine, "bricklayer_siu_schema")) {
    return(as.data.frame(rmoriebricklayer::bricklayer_siu_schema(), stringsAsFactors = FALSE))
  }
  .siu_core_schema()
}

#' @rdname morie_siu_schema
#' @export
morie_siu_html_to_text <- function(html, engine = "auto") {
  stopifnot(is.character(html), length(html) == 1L, !is.na(html))
  if (.siu_use_bricklayer(engine, "bricklayer_siu_text")) return(rmoriebricklayer::bricklayer_siu_text(html))
  .siu_core_html_to_text(html)
}

#' @rdname morie_siu_schema
#' @export
morie_siu_to_iso_date <- function(x, engine = "auto") {
  stopifnot(is.character(x))
  brick <- .siu_use_bricklayer(engine, "bricklayer_siu_iso_date")
  vapply(x, function(v) {
    if (is.na(v)) NA_character_ else if (brick) rmoriebricklayer::bricklayer_siu_iso_date(v) else .siu_core_to_iso_date(v)
  }, "", USE.NAMES = FALSE)
}

#' @rdname morie_siu_schema
#' @export
morie_siu_strip_boilerplate <- function(text) {
  stopifnot(is.character(text), length(text) == 1L, !is.na(text))
  .siu_core_strip_boilerplate(text)
}

#' @rdname morie_siu_schema
#' @export
morie_siu_report_url <- function(drid) {
  drid <- as.integer(drid)
  stopifnot(!anyNA(drid), all(drid > 0L))
  sprintf("https://www.siu.on.ca/en/directors_report_details.php?drid=%d", drid)
}

#' @rdname morie_siu_schema
#' @export
morie_siu_fetch_report <- function(drid, timeout = 30) {
  stopifnot(length(drid) == 1L)
  .siu_core_get(morie_siu_report_url(drid), timeout)
}

#' Bring-your-own language model: backend, model list and chat
#'
#' The SIU panel (and any caller) can use ANY model: an Ollama server (local,
#' on your network, or tunnelled), any OpenAI-compatible server (llama.cpp
#' \code{llama-server}, vLLM, LM Studio, LocalAI, text-generation-webui, TGI,
#' Jan, OpenRouter, OpenAI ...), or your own R function. Nothing is
#' hardcoded. Arguments left empty fall back to the environment:
#' \code{MORIE_LLM_API} (\code{"ollama"} or \code{"openai"}, default
#' \code{"ollama"}); \code{MORIE_LLM_BASE}, else \code{OLLAMA_HOST} /
#' \code{OLLAMA_BASE_URL} (ollama) or \code{OPENAI_BASE_URL} /
#' \code{LLM_API_BASE_URL} (openai), else localhost; \code{MORIE_LLM_KEY},
#' else \code{OLLAMA_API_KEY} or \code{OPENAI_API_KEY} / \code{LLM_API_KEY}.
#' An OpenAI-style base gets \code{/v1} appended unless it already ends in
#' \code{/v1}. \code{model = NULL} uses \code{MORIE_LLM_MODEL}, else
#' \code{OLLAMA_MODEL}, else the first model the server lists. A custom
#' \code{chat} is a \code{function(model, prompt)} returning the reply text.
#'
#' @param api \code{"ollama"} or \code{"openai"} (empty: environment).
#' @param base Server URL (empty: environment).
#' @param key Bearer token (empty: environment; may stay empty).
#' @param prompt Prompt text.
#' @param model Model name (\code{NULL}: default model).
#' @param timeout Request timeout in seconds.
#' @param temperature Sampling temperature (0 = deterministic).
#' @param chat Optional \code{function(model, prompt)} used instead of HTTP.
#' @return \code{morie_llm_backend()}: list (\code{api}, \code{base},
#'   \code{has_key}); \code{morie_llm_models()}: character vector;
#'   \code{morie_llm_chat()}: reply text.
#' @examples
#' b <- morie_llm_backend(api = "openai", base = "http://localhost:8080")
#' b$base
#' echo <- function(model, prompt) paste(model, "saw", nchar(prompt), "characters")
#' morie_llm_chat("hello", model = "my-model", chat = echo)
#' @export
morie_llm_backend <- function(api = "", base = "", key = "") {
  a <- .siu_backend_args(api, base, key)
  .siu_core_backend(a$api, a$base, a$key)
}

#' @rdname morie_llm_backend
#' @export
morie_llm_models <- function(api = "", base = "", key = "") {
  a <- .siu_backend_args(api, base, key)
  .siu_core_models(a$api, a$base, a$key)
}

#' @rdname morie_llm_backend
#' @export
morie_llm_chat <- function(prompt, model = NULL, api = "", base = "", key = "", timeout = 300, temperature = 0,
                           chat = NULL) {
  stopifnot(is.character(prompt), length(prompt) == 1L)
  if (!is.null(chat)) {
    stopifnot(is.function(chat))
    return(as.character(chat(if (is.null(model)) "custom" else model, prompt)))
  }
  a <- .siu_backend_args(api, base, key)
  if (is.null(model) || !nzchar(model)) model <- .siu_core_default_model(a$api, a$base, a$key)
  if (!nzchar(model)) stop("no model: name one or set MORIE_LLM_MODEL", call. = FALSE)
  .siu_core_chat(a$api, a$base, a$key, model, prompt, timeout, temperature)
}

#' Mixture-of-Agents audit panel for one SIU report (native, any model)
#'
#' The native panel: readers read the FULL report and answer every field with
#' a supporting quote and confidence; after a hard barrier the auditors (a
#' review chain, each auditing the previous stage) issue the final values.
#' \code{mode} 1 = one reader, no auditor; 2 = 1 reader + auditor; 3 = 2
#' readers + auditor; 4 = 3 readers + auditor. \code{num_readers} /
#' \code{num_auditors} scale beyond the presets. Readers run concurrently up
#' to \code{reader_concurrency} (0 = all) against an HTTP backend; with a
#' custom \code{chat} function they run one after another in this R session.
#' Models that return an empty reply are dropped first
#' (\code{health_check}). Works with any backend of
#' \code{\link{morie_llm_backend}}.
#'
#' @param report_text Report text (see \code{\link{morie_siu_html_to_text}}).
#' @param parsed Named list or character vector of parser guesses (default:
#'   \code{\link{morie_siu_parse_report}} of the text), or a JSON string.
#' @param mode Panel mode 1-4.
#' @param readers,auditors Model names (\code{NULL}: the default model).
#' @param num_readers,num_auditors Counts (0: from the mode / lists).
#' @param reader_concurrency Readers in flight (0 = all, 1 = sequential).
#' @param auditor_sequential Serialize the auditor tier.
#' @param reader_granularity,auditor_granularity \code{"all"} (one prompt for
#'   every field) or \code{"per-field"} (re-read the report once per field).
#' @param health_check Drop models that return an empty reply.
#' @param api,base,key,timeout,temperature,chat Backend, see
#'   \code{\link{morie_llm_backend}}.
#' @return A list: \code{fields} (the final record, parsed) and \code{json}
#'   (the raw JSON string).
#' @examples
#' fake <- function(model, prompt) {
#'   if (startsWith(prompt, "Reply with")) return("OK")
#'   if (grepl("You are the AUDITOR", prompt, fixed = TRUE)) return('{"police_service": "Barrie Police Service"}')
#'   '{"police_service": {"value": "Barrie", "quote": "Barrie", "confidence": "high"}}'
#' }
#' morie_siu_audit_panel("The Barrie Police Service ...", mode = 2, readers = "r1", chat = fake)$fields
#' @export
morie_siu_audit_panel <- function(report_text, parsed = NULL, mode = 4L, readers = NULL, auditors = NULL,
                                  num_readers = 0L, num_auditors = 0L, reader_concurrency = 0L,
                                  auditor_sequential = TRUE, reader_granularity = c("all", "per-field"),
                                  auditor_granularity = c("all", "per-field"), health_check = TRUE, api = "",
                                  base = "", key = "", timeout = 300, temperature = 0, chat = NULL) {
  stopifnot(is.character(report_text), length(report_text) == 1L)
  reader_granularity <- match.arg(reader_granularity)
  auditor_granularity <- match.arg(auditor_granularity)
  if (is.null(parsed)) parsed <- morie_siu_parse_report(paste0("<pre>", report_text, "</pre>"))
  pj <- if (is.character(parsed) && length(parsed) == 1L && is.null(names(parsed))) parsed else .siu_to_json(parsed)
  if (!is.null(chat)) stopifnot(is.function(chat))
  a <- .siu_backend_args(api, base, key)
  out <- .siu_core_panel(report_text, pj, as.integer(mode), as.character(readers), as.character(auditors),
                         as.integer(num_readers), as.integer(num_auditors), as.integer(reader_concurrency),
                         isTRUE(auditor_sequential), reader_granularity, auditor_granularity, isTRUE(health_check),
                         a$api, a$base, a$key, timeout, temperature, chat)
  list(fields = .jsonlt_parse(out), json = out)
}

.siu_json_str <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0("\"", s, "\"")
}

.siu_to_json <- function(x) {
  x <- as.list(x)
  if (!length(x)) return("{}")
  vals <- vapply(x, function(v) {
    v <- v[[1L]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) "null"
    else if (is.logical(v)) tolower(as.character(v))
    else if (is.numeric(v)) format(v, digits = 17)
    else .siu_json_str(as.character(v))
  }, "")
  paste0("{", paste0(.siu_json_str(names(x)), ": ", vals, collapse = ", "), "}")
}

#' SIU command-line front end inside R
#'
#' The \code{siu} CLI commands, run from R (or \code{Rscript -e
#' 'rmorie::morie_siu_cli()' siu ...}): \code{version}, \code{models},
#' \code{chat}, \code{fetch <drid>}, \code{parse <file>}, \code{resolve
#' <file>}, \code{audit <parsed.json> <report.txt>}, with the same
#' \code{--api/--base/--key/--model/--mode/--readers/--auditors/...} options
#' as the standalone C++ \code{siu} binary (tools/siu-cli).
#'
#' @param args Character vector of command-line arguments.
#' @return Invisibly, an exit status (0 success).
#' @examples
#' morie_siu_cli("version")
#' @export
morie_siu_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) && args[[1L]] == "siu") args <- args[-1L]
  opt <- function(name, default = "") {
    i <- match(name, args)
    if (is.na(i) || i == length(args)) default else args[[i + 1L]]
  }
  csv <- function(s) if (nzchar(s)) strsplit(s, ",", fixed = TRUE)[[1L]] else NULL
  slurp <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cmd <- if (length(args)) args[[1L]] else ""
  be <- list(api = opt("--api"), base = opt("--base"), key = opt("--key"))
  status <- switch(cmd,
    version = {
      cat("siu (morie) 1.0.0\n")
      0L
    },
    models = {
      b <- do.call(morie_llm_backend, be)
      m <- do.call(morie_llm_models, be)
      cat(sprintf("server: %s [%s] (%d models)\n", b$base, b$api, length(m)))
      if (length(m)) cat(paste0("  ", m, "\n"), sep = "")
      if (length(m)) 0L else 1L
    },
    chat = {
      model <- opt("--model", NA_character_)
      cat(do.call(morie_llm_chat, c(list(prompt = args[[length(args)]], model = if (is.na(model)) NULL else model,
                                         timeout = as.numeric(opt("--timeout", "300")),
                                         temperature = as.numeric(opt("--temperature", "0"))), be)), "\n")
      0L
    },
    fetch = {
      cat(morie_siu_html_to_text(morie_siu_fetch_report(as.integer(args[[2L]]))), "\n")
      0L
    },
    parse = {
      raw <- slurp(args[[2L]])
      f <- morie_siu_parse_report(if (grepl("<", raw, fixed = TRUE)) raw else paste0("<pre>", raw, "</pre>"))
      cat(.siu_to_json(as.list(f)), "\n")
      0L
    },
    resolve = {
      r <- morie_siu_resolve_so(text = slurp(args[[2L]]))
      cat(sprintf("subject_officers=%s  (%s)\n", if (is.na(r$count)) "UNRESOLVED" else r$count, r$reason))
      if (is.na(r$count)) 1L else 0L
    },
    audit = {
      res <- do.call(morie_siu_audit_panel, c(list(
        report_text = slurp(args[[3L]]), parsed = slurp(args[[2L]]), mode = as.integer(opt("--mode", "4")),
        readers = csv(opt("--readers")), auditors = csv(opt("--auditors", opt("--auditor"))),
        num_readers = as.integer(opt("--num-readers", "0")), num_auditors = as.integer(opt("--num-auditors", "0")),
        reader_concurrency = as.integer(opt("--reader-concurrency", "0")),
        auditor_sequential = opt("--auditor-sequential", "1") != "0",
        reader_granularity = opt("--reader-granularity", "all"),
        auditor_granularity = opt("--auditor-granularity", "all"),
        health_check = !("--no-health-check" %in% args), timeout = as.numeric(opt("--timeout", "300")),
        temperature = as.numeric(opt("--temperature", "0"))), be))
      cat(res$json, "\n")
      0L
    },
    {
      message("usage: siu <version|models|chat|fetch|parse|resolve|audit> [options]; see ?morie_siu_cli")
      2L
    }
  )
  invisible(status)
}
