# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Mixture-of-Agents reading panel for SIU director's reports -- the R
# first-class port of the pipeline that built the reviewed corpus:
# N readers each read the FULL report and answer every schema field with a
# supporting quote; after ALL readers finish (hard barrier) the auditor(s)
# read the report plus the readers' answers and issue the final value.
# Models come from the user's own Ollama server (OLLAMA_HOST / OLLAMA_MODEL,
# else auto-discovered) -- nothing is hardcoded.

#' Run the Mixture-of-Agents reading panel on SIU reports
#'
#' The model tier of the SIU mining pipeline, for reports that are not yet
#' in the panel-reviewed corpus (see [morie_siu_reports()]; reviewed reports
#' never need this -- their answers are already verified). Modes mirror the
#' original panel that built the corpus:
#'
#' * `mode = 1` -- one reader, no auditor (quick pass)
#' * `mode = 2` -- one reader + one auditor
#' * `mode = 3` -- two readers + one auditor
#' * `mode = 4` -- three readers + one auditor (highest confidence, default)
#'
#' Explicit `readers` / `auditors` vectors override the mode's counts, and
#' extra auditors form a sequential review chain (each sees its
#' predecessors' verdicts). Readers run concurrently up to
#' `reader_concurrency` (cloud tiers cap requests; local servers cap VRAM);
#' auditors are always sequential. The auditor never starts before every
#' reader has answered.
#'
#' The context prompt forbids lazy `"None"` answers: count-type fields must
#' be counted (0 is a real answer only for a witness-official-only case),
#' and every answer needs a verbatim supporting quote. With
#' `granularity = "per_field"` the model is asked one field at a time and
#' re-reads the whole report for each -- slower, but it stops a model
#' skimming once and hallucinating 60 answers.
#'
#' @param html Report HTML (character), or a file path, or a drid (numeric)
#'   fetched via the bricklayer engine.
#' @param mode Panel mode 1-4 (see above). Default 4.
#' @param readers Optional character vector of reader model names.
#' @param auditors Optional character vector of auditor model names
#'   (sequential chain).
#' @param reader_concurrency Max readers running at once (default 3).
#' @param granularity `"all_fields"` (one pass per reader) or `"per_field"`
#'   (one focused read per schema field). Applies to readers and auditors.
#' @param host Ollama server; default `Sys.getenv("OLLAMA_HOST")`.
#' @param timeout Per-call timeout in seconds (default 300).
#' @return A list: `fields` (named character vector, the auditor's final
#'   values), `readers` (each reader's raw answers), `audit_chain` (each
#'   auditor's verdicts), `models` (who served).
#' @examples
#' \dontrun{
#' # Needs a live Ollama server with at least one model.
#' res <- morie_siu_panel(5161, mode = 2)
#' res$fields["number_of_subject_officers"]
#' }
#' @export
morie_siu_panel <- function(html,
                            mode = 4L,
                            readers = NULL,
                            auditors = NULL,
                            reader_concurrency = 3L,
                            granularity = c("all_fields", "per_field"),
                            host = Sys.getenv("OLLAMA_HOST"),
                            timeout = 300) {
  granularity <- match.arg(granularity)
  stopifnot(mode %in% 1:4)

  # -- resolve the report text ------------------------------------------
  if (is.numeric(html)) {
    drid <- as.integer(html)
    if (!requireNamespace("rmoriebricklayer", quietly = TRUE)) {
      stop("fetching by drid needs rmoriebricklayer (the fetch engine)")
    }
    dest <- tempfile(fileext = ".html")
    on.exit(unlink(dest), add = TRUE)
    rmoriebricklayer::bricklayer_fetch_siu(drid, dest)
    html <- paste(readLines(dest, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  } else if (length(html) == 1L && !grepl("<", html, fixed = TRUE) &&
             file.exists(html)) {
    html <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  }
  text <- if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    rmoriebricklayer::bricklayer_siu_text(html)
  } else {
    .siu_html_to_text(html)
  }

  schema <- if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    rmoriebricklayer::bricklayer_siu_schema()
  } else {
    stop("morie_siu_panel() needs rmoriebricklayer for the field schema")
  }

  # -- models ------------------------------------------------------------
  if (!nzchar(host)) {
    stop("no Ollama server: set OLLAMA_HOST (local, tailnet, or a ",
         "Cloudflare-tunnelled endpoint)")
  }
  # The llm layer reads OLLAMA_HOST from the environment; honour an
  # explicit host= for this call only.
  old_host <- Sys.getenv("OLLAMA_HOST")
  Sys.setenv(OLLAMA_HOST = host)
  on.exit(Sys.setenv(OLLAMA_HOST = old_host), add = TRUE)
  available <- morie_llm_ollama_models()$name
  if (!length(available)) stop("Ollama at ", host, " serves no models")
  default_model <- Sys.getenv("OLLAMA_MODEL", unset = available[[1L]])
  n_readers <- c(1L, 1L, 2L, 3L)[mode]
  n_auditors <- c(0L, 1L, 1L, 1L)[mode]
  if (is.null(readers)) {
    readers <- rep_len(unique(c(default_model, available)), n_readers)
  }
  if (is.null(auditors)) {
    auditors <- if (n_auditors > 0L) default_model else character(0)
  }

  ask <- function(model, prompt) {
    morie_llm_ask(prompt, model = model, provider = "ollama",
                  timeout = timeout)
  }

  field_block <- function(f) {
    sprintf("- %s%s: %s", f["name"],
            if (identical(f["is_count"], "TRUE")) " (COUNT; 0 only for a witness-official-only case)" else "",
            f["description"])
  }
  schema_txt <- paste(apply(schema, 1L, field_block), collapse = "\n")

  base_rules <- paste(
    "You are auditing an Ontario Special Investigations Unit director's",
    "report. Read the ENTIRE report before answering. Never answer 'None'",
    "or 'not stated' unless you have read the full report and the value is",
    "genuinely absent. Count fields must be COUNTED from the report (zero",
    "is a real answer only when the report is witness-official-only).",
    "Every answer MUST carry a short verbatim quote from the report as",
    "evidence. Answer as strict JSON: {\"field\": {\"value\": ...,",
    "\"quote\": ...}, ...}.")

  read_once <- function(model) {
    if (granularity == "per_field") {
      out <- list()
      for (i in seq_len(nrow(schema))) {
        f <- schema[i, ]
        p <- paste0(base_rules, "\n\nAnswer ONLY this field:\n",
                    field_block(unlist(f)), "\n\nREPORT:\n", text)
        out[[f$name]] <- ask(model, p)
      }
      out
    } else {
      p <- paste0(base_rules, "\n\nFields:\n", schema_txt,
                  "\n\nREPORT:\n", text)
      ask(model, p)
    }
  }

  # -- readers (concurrent up to the cap), then the BARRIER --------------
  n_cores <- max(1L, min(as.integer(reader_concurrency), length(readers)))
  reader_out <- if (n_cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(readers, read_once, mc.cores = n_cores)
  } else {
    lapply(readers, read_once)
  }
  names(reader_out) <- paste0("reader_", seq_along(readers), ":", readers)
  # (mclapply/lapply both return only when EVERY reader is done -- the
  # barrier is structural, the auditor cannot start early.)

  # -- auditor chain (sequential) ----------------------------------------
  audit_chain <- list()
  prior <- ""
  final_raw <- NULL
  for (a in seq_along(auditors)) {
    p <- paste0(
      base_rules, "\n\nYou are the AUDITOR. ", length(readers),
      " reader(s) answered every field; their raw answers follow. Read the",
      " report yourself, weigh their answers and quotes, and issue the",
      " FINAL value for every field under its canonical key",
      " (number_of_subject_officers, never a variant spelling).",
      if (nzchar(prior)) "\nPrevious auditor verdicts:\n" else "", prior,
      "\n\nReader answers:\n",
      paste(vapply(seq_along(reader_out), function(i) {
        paste0(names(reader_out)[i], ":\n",
               paste(unlist(reader_out[[i]]), collapse = "\n"))
      }, character(1)), collapse = "\n\n"),
      "\n\nFields:\n", schema_txt, "\n\nREPORT:\n", text)
    final_raw <- ask(auditors[[a]], p)
    audit_chain[[paste0("auditor_", a, ":", auditors[[a]])]] <- final_raw
    prior <- paste(prior, final_raw, sep = "\n")
  }

  fields <- .siu_panel_extract(if (is.null(final_raw)) {
    reader_out[[1L]]
  } else {
    final_raw
  }, schema$name)

  list(fields = fields, readers = reader_out, audit_chain = audit_chain,
       models = list(readers = readers, auditors = auditors, host = host))
}

# Pull {"field": {"value": ...}} JSON out of a model reply (or a per-field
# list of replies); tolerate prose around the JSON.
#' Internal helper: Siu Panel Extract
#' @noRd
.siu_panel_extract <- function(raw, field_names) {
  out <- setNames(rep(NA_character_, length(field_names)), field_names)
  parse_one <- function(txt) {
    m <- regmatches(txt, regexpr("\\{[\\s\\S]*\\}", txt, perl = TRUE))
    if (!length(m)) return(NULL)
    tryCatch(jsonlite::fromJSON(m[[1L]]), error = function(e) NULL)
  }
  if (is.list(raw)) {
    for (fn in intersect(names(raw), field_names)) {
      j <- parse_one(raw[[fn]])
      v <- if (is.list(j) && !is.null(j$value)) j$value else
        if (is.list(j) && length(j)) j[[1L]]$value %||% j[[1L]] else NULL
      if (!is.null(v)) out[fn] <- as.character(v)[1L]
    }
    return(out)
  }
  j <- parse_one(raw)
  if (is.list(j)) {
    for (fn in intersect(names(j), field_names)) {
      v <- j[[fn]]
      out[fn] <- as.character(if (is.list(v)) v$value %||% v[[1L]] else v)[1L]
    }
  }
  out
}
