# SPDX-License-Identifier: AGPL-3.0-or-later
#
# llm.R -- Provider chain for MORIE's LLM integration layer.
#
# R port of src/morie/llm.py. Implements the same provider priority:
#   1. Ollama (local, no key)
#   2. Gemini (Google AI Studio, OpenAI-compatible endpoint)
#   3. Generic OpenAI-compatible endpoint (LLM_API_BASE_URL/LLM_API_KEY)
#   4. Official OpenAI API
#   5. Local fallback help text (no network)
#
# OllamaFreeAPI (Python SDK) is intentionally NOT ported -- R has no
# equivalent client. HTTP providers use `httr2` + `jsonlite`. All
# public functions are prefixed `morie_llm_*` and exported. Streaming
# is not supported in this R port (always returns the full string).

DEFAULT_OLLAMA_BASE_URL <- "http://localhost:11434"
DEFAULT_GEMINI_MODEL    <- "gemini-2.5-flash"
DEFAULT_API_MODEL       <- "google/gemma-3-27b-it"
DEFAULT_OPENAI_MODEL    <- "gpt-4o-mini"
OPENAI_BASE_URL <- "https://api.openai.com"
GEMINI_BASE_URL <- "https://generativelanguage.googleapis.com/v1beta/openai"

`%||%` <- function(a, b) if (is.null(a)) b else a

.morie_llm_env <- function(name, default = "") {
  v <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(v)) v else default
}
.morie_llm_ollama_base <- function() {
  sub("/+$", "", .morie_llm_env("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE_URL))
}
.morie_llm_gemini_key  <- function() { v <- .morie_llm_env("GEMINI_API_KEY")
if (nzchar(v)) v else NULL }
.morie_llm_openai_key  <- function() { v <- .morie_llm_env("OPENAI_API_KEY")
if (nzchar(v)) v else NULL }
.morie_llm_api_base    <- function() { v <- .morie_llm_env("LLM_API_BASE_URL")
if (nzchar(v)) sub("/+$", "", v) else NULL }
.morie_llm_api_key     <- function() { v <- .morie_llm_env("LLM_API_KEY")
if (nzchar(v)) v else NULL }
.morie_llm_gemini_model <- function() .morie_llm_env("GEMINI_MODEL", DEFAULT_GEMINI_MODEL)

#' Probe a local Ollama instance
#' @param timeout Probe timeout in seconds.
#' @return Logical scalar -- TRUE when reachable.
#' @export
morie_llm_probe_ollama <- function(timeout = 2) {
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
  cache <- getOption("morie.llm.ollama_cached", default = NULL)
  if (!is.null(cache)) return(cache)
  out <- tryCatch({
    req <- httr2::request(paste0(.morie_llm_ollama_base(), "/api/tags"))
    req <- httr2::req_timeout(req, timeout)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) < 400
  }, error = function(e) FALSE)
  options(morie.llm.ollama_cached = out)
  out
}

#' Detect the active LLM provider
#' @return Character scalar provider key: ollama / gemini / api / openai / local.
#' @export
morie_llm_detect_provider <- function() {
  if (morie_llm_probe_ollama())                                 return("ollama")
  if (!is.null(.morie_llm_gemini_key()))                        return("gemini")
  if (!is.null(.morie_llm_api_base()) && !is.null(.morie_llm_api_key()))
                                                                return("api")
  if (!is.null(.morie_llm_openai_key()))                        return("openai")
  "local"
}

.morie_llm_system_prompt <- function(context_block = "") {
  paste0(
    "You are the MORIE agent for methods for observational inference and ",
    "robust analysis of interventions in sociolegal studies.\
\
",
    "MORIE is a Python+R toolkit for Canadian public-health and carceral ",
    "data analysis, causal inference, and reproducible research.\
\
",
    context_block
  )
}

.morie_llm_messages <- function(prompt, context = NULL, system_prompt = NULL) {
  if (is.null(system_prompt)) {
    ctx_block <- if (is.null(context)) "" else paste(
      vapply(names(context),
             function(k) paste0(k, ": ", as.character(context[[k]])),
             character(1)),
      collapse = "\
")
    system_prompt <- .morie_llm_system_prompt(ctx_block)
  }
  list(
    list(role = "system", content = system_prompt),
    list(role = "user",   content = prompt)
  )
}

#' POST a chat-completion request to an OpenAI-compatible endpoint
#' @param base_url Provider base URL.
#' @param model Model identifier.
#' @param messages List of role/content lists.
#' @param api_key Optional bearer token (NULL for local Ollama).
#' @param timeout Seconds. Default 120.
#' @return Parsed JSON list (the response body).
#' @export
morie_llm_request_completion <- function(base_url, model, messages,
                                         api_key = NULL, timeout = 120) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("morie_llm_request_completion requires httr2 and jsonlite.")
  }
  url <- paste0(base_url, "/v1/chat/completions")
  payload <- list(model = model, messages = messages, stream = FALSE)
  if (grepl("localhost|127\\.0\\.0\\.1", base_url)) {
    payload$max_tokens <- 4096L
    timeout <- max(timeout, 300)
  }
  req <- httr2::request(url)
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
  }
  req <- httr2::req_body_raw(req,
    jsonlite::toJSON(payload, auto_unbox = TRUE),
    type = "application/json")
  req <- httr2::req_timeout(req, timeout)
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

.morie_llm_extract_text <- function(data) {
  choices <- data$choices
  if (length(choices) == 0L) return("")
  msg <- choices[[1]]$message
  if (is.null(msg)) "" else (msg$content %||% "")
}

.morie_llm_local_fallback <- function(prompt) {
  paste0(
    "MORIE is running in local-only mode (no LLM provider detected).\
\
",
    "Available capabilities without an LLM:\
",
    "  - morie list-modules        List analysis modules\
",
    "  - morie run-module <name>   Run a specific module\
",
    "  - morie pipeline --all -y   Run the full analysis pipeline\
\
",
    "Enable an LLM by setting one of GEMINI_API_KEY, ",
    "LLM_API_BASE_URL + LLM_API_KEY, or OPENAI_API_KEY, ",
    "or by running a local Ollama instance."
  )
}

#' Send a prompt to the best available LLM provider
#'
#' R port of `morie.llm.ask`. Tries each provider in priority order; on
#' HTTP/timeout failure falls through to the next, and finally to a
#' static local help string.
#'
#' @param prompt User question or instruction.
#' @param context Optional named list injected as text into the system prompt.
#' @param model Optional model override.
#' @param provider Optional provider override (ollama/gemini/api/openai/local).
#'   NULL = auto-detect.
#' @param system_prompt Optional full system-prompt override.
#' @param timeout HTTP timeout in seconds. Default 120.
#' @return Character scalar response text, or local-fallback text when all
#'   providers fail.
#' @export
morie_llm_ask <- function(prompt, context = NULL, model = NULL,
                          provider = NULL, system_prompt = NULL,
                          timeout = 120) {
  if (is.null(provider)) provider <- morie_llm_detect_provider()
  if (identical(provider, "local")) return(.morie_llm_local_fallback(prompt))

  messages <- .morie_llm_messages(prompt, context = context,
                                  system_prompt = system_prompt)
  attempts <- list()
  add <- function(base, mdl, key) attempts[[length(attempts) + 1L]] <<-
    list(base = base, model = mdl, key = key)
  if (provider == "ollama") {
    add(.morie_llm_ollama_base(), model %||% "llama3", NULL)
  }
  if (provider %in% c("ollama", "gemini") && !is.null(.morie_llm_gemini_key())) {
    add(GEMINI_BASE_URL, model %||% .morie_llm_gemini_model(),
        .morie_llm_gemini_key())
  }
  if (!is.null(.morie_llm_api_base()) && !is.null(.morie_llm_api_key())) {
    add(.morie_llm_api_base(), model %||% DEFAULT_API_MODEL,
        .morie_llm_api_key())
  }
  if (!is.null(.morie_llm_openai_key())) {
    add(OPENAI_BASE_URL, model %||% DEFAULT_OPENAI_MODEL,
        .morie_llm_openai_key())
  }
  if (length(attempts) == 0L) return(.morie_llm_local_fallback(prompt))

  for (a in attempts) {
    out <- tryCatch(
      .morie_llm_extract_text(
        morie_llm_request_completion(a$base, a$model, messages,
                                     api_key = a$key, timeout = timeout)),
      error = function(e) NULL)
    if (!is.null(out) && nzchar(out)) return(out)
  }
  .morie_llm_local_fallback(prompt)
}

#' Return TRUE when at least one live LLM provider is available
#' @return Logical scalar.
#' @export
morie_llm_agent_available <- function() {
  morie_llm_detect_provider() != "local"
}


# --- APPENDED 2026-05-22 -----------------------------------------------------
# FreeAPI / multi-provider additions.  Mirrors src/morie/llm.py
# Adds _probe_freeapi / list_freeapi_models / ask_multi to the R surface and
# inserts FreeAPI in position 2 of the provider chain (between ollama and
# gemini), matching the Python provider priority.
# ----------------------------------------------------------------------------

DEFAULT_FREEAPI_MODEL <- "mistral-nemo:custom"
FREEAPI_BASE_URL      <- "https://ollamafreeapi.duckdns.org"  # community-hosted

.morie_llm_freeapi_model <- function() {
  v <- trimws(Sys.getenv("moriefam", unset = ""))
  if (nzchar(v)) v else DEFAULT_FREEAPI_MODEL
}

#' Probe an OllamaFreeAPI community server
#'
#' R port of the Python ``_probe_freeapi`` helper.  Returns ``TRUE`` when at
#' least one free remote model is reachable.  The result is cached in
#' ``options(morie.llm.freeapi_cached)`` for the process lifetime.  A single
#' one-second retry is performed because community servers can be slow.
#'
#' @param timeout Probe timeout in seconds.  Default 4.
#' @return Logical scalar.
#' @export
morie_llm_probe_freeapi <- function(timeout = 4) {
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
  cache <- getOption("morie.llm.freeapi_cached", default = NULL)
  if (!is.null(cache)) return(cache)
  try_probe <- function() {
    req <- httr2::request(paste0(FREEAPI_BASE_URL, "/api/tags"))
    req <- httr2::req_timeout(req, timeout)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) < 400
  }
  out <- tryCatch(try_probe(), error = function(e) NA)
  if (isTRUE(is.na(out)) || !isTRUE(out)) {
    Sys.sleep(1)
    out <- tryCatch(try_probe(), error = function(e) FALSE)
  }
  out <- isTRUE(out)
  options(morie.llm.freeapi_cached = out)
  out
}

#' List vendored OllamaFreeAPI model catalogue
#'
#' R port of ``list_freeapi_models``.  Walks any JSON files in
#' ``inst/ollama_json/`` (mirroring the Python ``morie/ollama_json/``
#' vendoring) and emits a data.frame with one row per unique model.  When the
#' catalogue directory is absent (R-only install), a single fallback row for
#' the default model is returned so downstream callers always get a usable
#' table.
#'
#' @return data.frame with columns model / family / size / label / alias.
#' @export
morie_llm_list_freeapi_models <- function() {
  json_dir <- system.file("ollama_json", package = "morie")
  rows <- list()
  seen <- character(0)
  if (nzchar(json_dir) && dir.exists(json_dir) &&
      requireNamespace("jsonlite", quietly = TRUE)) {
    for (jf in sort(list.files(json_dir, pattern = "\\\\.json$",
                               full.names = TRUE))) {
      data <- tryCatch(jsonlite::fromJSON(jf, simplifyVector = FALSE),
                       error = function(e) NULL)
      models <- tryCatch(data$props$pageProps$models, error = function(e) NULL)
      if (is.null(models)) next
      for (m in models) {
        name <- if (!is.null(m$model_name)) m$model_name else m$model
        if (is.null(name) || !nzchar(name) || name %in% seen) next
        seen <- c(seen, name)
        family <- as.character(m$family %||% "")
        size   <- as.character(m$parameter_size %||% "")
        label  <- if (nzchar(family) && nzchar(size)) {
          paste0(toupper(substr(family, 1, 1)), substring(family, 2),
                 ":", tolower(size))
        } else name
        rows[[length(rows) + 1L]] <-
          data.frame(model = name, family = family, size = size,
                     label = label, alias = "", stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(model = DEFAULT_FREEAPI_MODEL,
                      family = "mistral", size = "nemo",
                      label = "Mistral:nemo", alias = "mn",
                      stringsAsFactors = FALSE))
  }
  df <- do.call(rbind, rows)
  # Two-letter alias assignment (mirrors the Python alias loop).
  used <- character(0)
  alias <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    base <- sub(".*/", "", sub(":.*", "", df$model[i]))
    fam  <- if (nzchar(df$family[i])) df$family[i] else base
    cand <- tolower(paste0(substr(base, 1, 1), substr(fam, 1, 1)))
    if (cand %in% used) cand <- tolower(substr(base, 1, 2))
    if (cand %in% used) {
      digit <- gsub("[^0-9]", "", df$size[i])
      digit <- if (nzchar(digit)) substr(digit, 1, 1) else "x"
      cand <- tolower(paste0(substr(base, 1, 1), digit))
    }
    if (cand %in% used) cand <- tolower(substr(base, 1, 3))
    used <- c(used, cand)
    alias[i] <- cand
  }
  df$alias <- alias
  df
}

# --- Provider chain: re-define detect_provider to inject FreeAPI in slot 2 ---
# We deliberately overwrite the earlier morie_llm_detect_provider() so the
# ordering matches Python: ollama -> freeapi -> gemini -> api -> openai -> local.

morie_llm_detect_provider <- function() {
  if (morie_llm_probe_ollama())                  return("ollama")
  if (morie_llm_probe_freeapi())                 return("freeapi")
  if (!is.null(.morie_llm_gemini_key()))         return("gemini")
  if (!is.null(.morie_llm_api_base()) &&
      !is.null(.morie_llm_api_key()))            return("api")
  if (!is.null(.morie_llm_openai_key()))         return("openai")
  "local"
}

.morie_llm_messages_to_prompt <- function(messages) {
  parts <- vapply(messages, function(m) {
    role <- m$role %||% "user"
    content <- m$content %||% ""
    if (identical(role, "system"))    paste0("[System: ", content, "]")
    else if (identical(role, "assistant")) paste0("Assistant: ", content)
    else                              content
  }, character(1))
  paste(parts, collapse = "\
\
")
}

.morie_llm_strip_think <- function(text) {
  trimws(gsub("(?s)<think>.*?</think>\\s*", "", text, perl = TRUE))
}

# Non-streaming FreeAPI completion via the same OpenAI-compatible endpoint
# that ollama exposes.  R has no native ``ollamafreeapi`` SDK, so we POST
# /v1/chat/completions to FREEAPI_BASE_URL.  Returns "" on failure.
.morie_llm_freeapi_completion <- function(messages, model = NULL, timeout = 180) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) return("")
  body <- list(model = model %||% .morie_llm_freeapi_model(),
               messages = messages, stream = FALSE)
  out <- tryCatch({
    req <- httr2::request(paste0(FREEAPI_BASE_URL, "/v1/chat/completions"))
    req <- httr2::req_headers(req, `Content-Type` = "application/json")
    req <- httr2::req_body_raw(req, jsonlite::toJSON(body, auto_unbox = TRUE),
                               type = "application/json")
    req <- httr2::req_timeout(req, timeout)
    resp <- httr2::req_perform(req)
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                                 simplifyVector = FALSE)
    .morie_llm_extract_text(parsed)
  }, error = function(e) "")
  .morie_llm_strip_think(out %||% "")
}

#' Ask the best available LLM provider, accepting a multi-turn messages list
#'
#' R port of ``morie.llm.ask_multi``.  Unlike :func:`morie_llm_ask`, this
#' accepts a pre-built ``messages`` list (each element: ``role``/``content``)
#' enabling multi-turn conversation.  Streaming is not supported in the R
#' port -- this always returns a single character scalar.
#'
#' Provider fall-through order mirrors :func:`morie_llm_detect_provider`:
#' ollama -> freeapi -> gemini -> api -> openai -> local.
#'
#' @param messages list of role/content lists.
#' @param providers Optional character vector forcing a specific provider
#'   ordering.  When NULL the auto-detected provider is tried first, then
#'   the remaining providers in priority order.
#' @param model Optional model identifier.
#' @param timeout HTTP timeout in seconds.
#' @return Character scalar response text.
#' @export
morie_llm_ask_multi <- function(messages, providers = NULL,
                                model = NULL, timeout = 120) {
  stopifnot(is.list(messages))

  if (is.null(providers)) {
    detected <- morie_llm_detect_provider()
    providers <- unique(c(detected,
                          "ollama", "freeapi", "gemini",
                          "api", "openai", "local"))
  }

  fallback_prompt <- function() {
    user_msgs <- Filter(function(m) identical(m$role %||% "", "user"), messages)
    if (length(user_msgs) == 0L) "" else user_msgs[[length(user_msgs)]]$content %||% ""
  }

  for (prov in providers) {
    if (identical(prov, "local")) {
      return(.morie_llm_local_fallback(fallback_prompt()))
    }
    if (identical(prov, "freeapi")) {
      out <- .morie_llm_freeapi_completion(messages, model = model,
                                           timeout = timeout)
      if (nzchar(out)) return(out)
      next
    }
    # HTTP-OpenAI-compatible providers
    cfg <- switch(prov,
      ollama = list(base = .morie_llm_ollama_base(), mdl = model %||% "llama3",
                    key = NULL),
      gemini = if (!is.null(.morie_llm_gemini_key()))
                 list(base = GEMINI_BASE_URL,
                      mdl = model %||% .morie_llm_gemini_model(),
                      key = .morie_llm_gemini_key()),
      api    = if (!is.null(.morie_llm_api_base()) &&
                   !is.null(.morie_llm_api_key()))
                 list(base = .morie_llm_api_base(),
                      mdl = model %||% DEFAULT_API_MODEL,
                      key = .morie_llm_api_key()),
      openai = if (!is.null(.morie_llm_openai_key()))
                 list(base = OPENAI_BASE_URL,
                      mdl = model %||% DEFAULT_OPENAI_MODEL,
                      key = .morie_llm_openai_key()),
      NULL)
    if (is.null(cfg)) next
    out <- tryCatch(
      .morie_llm_extract_text(
        morie_llm_request_completion(cfg$base, cfg$mdl, messages,
                                     api_key = cfg$key, timeout = timeout)),
      error = function(e) NULL)
    if (!is.null(out) && nzchar(out)) return(out)
  }
  .morie_llm_local_fallback(fallback_prompt())
}
