# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Mixture-of-Agents reading panel for SIU director's reports -- the R
# first-class port of the pipeline that built the reviewed corpus:
# N readers each read the FULL report and answer every schema field with a
# supporting quote; after ALL readers finish (hard barrier) the auditor(s)
# read the report plus the readers' answers and issue the final value.
# Models come from ANY backend the user points at (see morie_llm_backend()):
# an Ollama server, any OpenAI-compatible server, or the user's own R
# function -- nothing is hardcoded.

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
#'   fetched via the bricklayer engine (or rmorie's native fetch).
#' @param mode Panel mode 1-4 (see above). Default 4.
#' @param readers Optional character vector of reader model names.
#' @param auditors Optional character vector of auditor model names
#'   (sequential chain).
#' @param reader_concurrency Max readers running at once (default 3).
#' @param granularity `"all_fields"` (one pass per reader) or `"per_field"`
#'   (one focused read per schema field). Applies to readers and auditors.
#' @param host Server URL (kept for compatibility; the same as `base`);
#'   default `Sys.getenv("OLLAMA_HOST")`.
#' @param timeout Per-call timeout in seconds (default 300).
#' @param api `"ollama"` or `"openai"` (any OpenAI-compatible server:
#'   llama.cpp, vLLM, LM Studio, LocalAI, TGI, OpenRouter ...); empty uses
#'   `MORIE_LLM_API`, else `"ollama"`. See [morie_llm_backend()].
#' @param base Server URL; empty falls back to the environment
#'   (`MORIE_LLM_BASE`, `OLLAMA_HOST`, `OPENAI_BASE_URL`, `LLM_API_BASE_URL`).
#' @param key Bearer token; empty falls back to `MORIE_LLM_KEY` or the
#'   provider's key variable.
#' @param chat Optional `function(model, prompt)` returning the reply text --
#'   your own model, used instead of a server.
#' @return A list: `fields` (named character vector, the auditor's final
#'   values), `readers` (each reader's raw answers), `audit_chain` (each
#'   auditor's verdicts), `models` (who served, and the backend).
#' @examples
#' \dontshow{if (morie_llm_probe_ollama()) withAutoprint(\{ # examplesIf}
#' # Runs when a local Ollama server is reachable (free default).
#' res <- morie_siu_panel(5161, mode = 2)
#' res$fields["number_of_subject_officers"]
#' \dontshow{\}) # examplesIf}
#' @export
morie_siu_panel <- function(html,
                            mode = 4L,
                            readers = NULL,
                            auditors = NULL,
                            reader_concurrency = 3L,
                            granularity = c("all_fields", "per_field"),
                            host = Sys.getenv("OLLAMA_HOST"),
                            timeout = 300,
                            api = "",
                            base = host,
                            key = "",
                            chat = NULL) {
  granularity <- match.arg(granularity)
  stopifnot(mode %in% 1:4)

  # -- resolve the report text ------------------------------------------
  if (is.numeric(html)) {
    drid <- as.integer(html)
    if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
      dest <- tempfile(fileext = ".html")
      on.exit(unlink(dest), add = TRUE)
      rmoriebricklayer::bricklayer_fetch_siu(drid, dest)
      html <- paste(readLines(dest, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
    } else {
      html <- morie_siu_fetch_report(drid)
    }
  } else if (length(html) == 1L && !grepl("<", html, fixed = TRUE) &&
             file.exists(html)) {
    html <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
  }
  text <- morie_siu_html_to_text(html)
  schema <- morie_siu_schema()

  # -- models: any backend (Ollama, OpenAI-compatible, or chat=) ---------
  if (!is.null(chat)) stopifnot(is.function(chat))
  be <- .siu_backend_args(api, base, key)
  available <- if (is.null(chat)) .siu_core_models(be$api, be$base, be$key) else character(0)
  env_model <- Sys.getenv("MORIE_LLM_MODEL", unset = Sys.getenv("OLLAMA_MODEL"))
  default_model <- if (nzchar(env_model)) env_model else if (length(available)) available[[1L]] else "custom"
  if (is.null(chat) && !length(available) && is.null(readers)) {
    b <- morie_llm_backend(be$api, be$base, be$key)
    stop("no models at ", b$base, " [", b$api, "]: start a server, set MORIE_LLM_BASE / ",
         "OLLAMA_HOST / OPENAI_BASE_URL, pass `readers`, or supply `chat`", call. = FALSE)
  }
  n_readers <- c(1L, 1L, 2L, 3L)[mode]
  n_auditors <- c(0L, 1L, 1L, 1L)[mode]
  if (is.null(readers)) {
    readers <- rep_len(unique(c(default_model, available)), n_readers)
  }
  if (is.null(auditors)) {
    auditors <- if (n_auditors > 0L) default_model else character(0)
  }

  ask <- function(model, prompt) {
    if (!is.null(chat)) return(as.character(chat(model, prompt)))
    .siu_core_chat(be$api, be$base, be$key, model, prompt, timeout, 0)
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
       models = list(readers = readers, auditors = auditors,
                     backend = if (is.null(chat)) morie_llm_backend(be$api, be$base, be$key) else "custom"))
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
    tryCatch(.morie_from_json(m[[1L]]), error = function(e) NULL)
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
