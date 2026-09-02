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
# OllamaFreeAPI is intentionally NOT supported -- the community server
# registry it depended on was chronically down (1/59 hosts live, default
# model served nowhere) and its parallel network probe hung R CMD check
# examples on CI runners, so the whole provider was removed 2026-07.
# HTTP providers use `httr2` + `jsonlite`. All
# public functions are prefixed `morie_llm_*` and exported. Streaming
# is not supported in this R port (always returns the full string).

# Process-lifetime probe cache. Uses a package-internal environment rather
# than options() so we never modify the user's global options (CRAN policy);
# matches the .morie_*_env pattern used elsewhere in the package.
.morie_llm_cache <- new.env(parent = emptyenv())

DEFAULT_OLLAMA_BASE_URL <- "http://localhost:11434"
DEFAULT_GEMINI_MODEL    <- "gemini-2.5-flash"
DEFAULT_API_MODEL       <- "google/gemma-3-27b-it"
DEFAULT_OPENAI_MODEL    <- "gpt-4o-mini"
OPENAI_BASE_URL <- "https://api.openai.com"
GEMINI_BASE_URL <- "https://generativelanguage.googleapis.com/v1beta/openai"

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Internal helper: Morie Llm Env
#' @noRd
.morie_llm_env <- function(name, default = "") {
  v <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(v)) v else default
}
#' Internal helper: Morie Llm Ollama Base
#'
#' The Ollama endpoint. Point \code{OLLAMA_HOST} (or \code{OLLAMA_BASE_URL})
#' at ANY reachable Ollama-compatible server: a local daemon
#' (\code{http://localhost:11434}), a box on your network, or a remote /
#' Cloudflare-tunnelled gateway (e.g. a shared team server). No model is
#' bundled or assumed -- you bring your own.
#' @noRd
.morie_llm_ollama_base <- function() {
  host <- .morie_llm_env("OLLAMA_HOST")
  if (nzchar(host)) {
    if (!grepl("^https?://", host)) host <- paste0("http://", host)
    return(sub("/+$", "", host))
  }
  sub("/+$", "", .morie_llm_env("OLLAMA_BASE_URL", DEFAULT_OLLAMA_BASE_URL))
}

#' List the models an Ollama server is serving
#'
#' Connects to an Ollama server over its REST API (\code{GET /api/tags}) and
#' returns the models it has available. Use it to confirm rmorie can reach
#' your Ollama instance and to see the exact model names you can pass as the
#' \code{model} argument or via the \code{OLLAMA_MODEL} environment variable
#' -- rmorie bundles no model and assumes none, so you bring your own.
#'
#' Point \code{OLLAMA_HOST} (or \code{OLLAMA_BASE_URL}) at ANY reachable
#' Ollama-compatible server: a local daemon (\code{http://localhost:11434},
#' the default), a machine on your network, or a remote / Cloudflare-tunnelled
#' gateway. A local daemon needs no key; set \code{OLLAMA_API_KEY} for gateways
#' that require a bearer token.
#'
#' @param base Ollama base URL. Defaults to \code{OLLAMA_HOST} /
#'   \code{OLLAMA_BASE_URL}, else \code{http://localhost:11434}.
#' @param timeout Request timeout in seconds. Default 5.
#' @return A \code{data.frame}, one row per model, with columns \code{name},
#'   \code{size_gb}, \code{family}, \code{parameter_size} and
#'   \code{quantization}. Zero rows when the server is unreachable or serves
#'   no models (never errors), so it doubles as a connectivity test.
#' @examples
#' \donttest{
#' # Point at your own Ollama server, then see what it serves:
#' Sys.setenv(OLLAMA_HOST = "http://localhost:11434")
#' models <- morie_llm_ollama_models()
#' models$name
#' }
#' @export
morie_llm_ollama_models <- function(base = .morie_llm_ollama_base(),
                                    timeout = 5) {
  empty <- data.frame(name = character(), size_gb = numeric(),
                      family = character(), parameter_size = character(),
                      quantization = character(), stringsAsFactors = FALSE)
  if (!requireNamespace("httr2", quietly = TRUE)) return(empty)
  models <- tryCatch({
    req <- httr2::req_timeout(httr2::request(paste0(base, "/api/tags")), timeout)
    key <- .morie_llm_env("OLLAMA_API_KEY")
    if (nzchar(key)) req <- httr2::req_headers(req,
                                               Authorization = paste("Bearer", key))
    body <- .morie_from_json(httr2::resp_body_string(httr2::req_perform(req)),
                             simplifyVector = FALSE)
    body$models %||% list()
  }, error = function(e) list())
  if (!length(models)) return(empty)
  det <- function(m, k) as.character((m$details %||% list())[[k]] %||% NA)
  data.frame(
    name           = vapply(models, function(m)
      as.character(m$name %||% m$model %||% NA), ""),
    size_gb        = vapply(models, function(m)
      round(as.numeric(m$size %||% NA_real_) / 1e9, 2), numeric(1)),
    family         = vapply(models, det, "", k = "family"),
    parameter_size = vapply(models, det, "", k = "parameter_size"),
    quantization   = vapply(models, det, "", k = "quantization_level"),
    stringsAsFactors = FALSE)
}

#' Internal helper: resolve the Ollama model to use
#'
#' No hardcoded model name -- models are pulled per-machine and upstream tags
#' get retired (llama3.2, gemma3), so an assumed default errors on most
#' servers. Resolution order: (1) \code{OLLAMA_MODEL} if the user named one;
#' (2) otherwise ask the server what it actually serves and use the first
#' model. Returns \code{NA_character_} when neither is available so callers
#' can raise a clear "pull a model or set OLLAMA_MODEL" error.
#' @noRd
.morie_llm_ollama_default_model <- function(base = .morie_llm_ollama_base()) {
  env <- .morie_llm_env("OLLAMA_MODEL")
  if (nzchar(env)) return(env)
  if (.morie_llm_no_net()) return(NA_character_)
  m <- morie_llm_ollama_models(base)
  if (nrow(m) && !is.na(m$name[1L]) && nzchar(m$name[1L])) m$name[1L]
  else NA_character_
}
#' Internal helper: Morie Llm Gemini Key
#' @noRd
.morie_llm_gemini_key  <- function() { v <- .morie_llm_env("GEMINI_API_KEY")
if (nzchar(v)) v else NULL }
#' Internal helper: Morie Llm Openai Key
#' @noRd
.morie_llm_openai_key  <- function() { v <- .morie_llm_env("OPENAI_API_KEY")
if (nzchar(v)) v else NULL }
#' Internal helper: Morie Llm Api Base
#' @noRd
.morie_llm_api_base    <- function() { v <- .morie_llm_env("LLM_API_BASE_URL")
if (nzchar(v)) sub("/+$", "", v) else NULL }
#' Internal helper: Morie Llm Api Key
#' @noRd
.morie_llm_api_key     <- function() { v <- .morie_llm_env("LLM_API_KEY")
if (nzchar(v)) v else NULL }
#' Internal helper: Morie Llm Gemini Model
#' @noRd
.morie_llm_gemini_model <- function() .morie_llm_env("GEMINI_MODEL", DEFAULT_GEMINI_MODEL)

# TRUE when live-network probes must be suppressed: under R CMD check /
# examples / covr the result must be deterministic and must never hang on a
# dead-but-open host. A caller (or a test) can force a real probe with
# options(morie.llm.allow_net_probe = TRUE).
#' @noRd
.morie_llm_no_net <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
    !isTRUE(getOption("morie.llm.allow_net_probe"))
}

#' Probe a local Ollama instance
#' @param timeout Probe timeout in seconds.
#' @return Logical scalar -- TRUE when reachable.
#' @examples
#' old <- options(morie.llm.ollama_cached = FALSE)
#' morie_llm_probe_ollama()
#' options(old)
#' @export
morie_llm_probe_ollama <- function(timeout = 2) {
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
  if (.morie_llm_no_net()) return(FALSE)
  cache <- .morie_llm_cache$ollama_cached
  if (!is.null(cache)) return(cache)
  out <- tryCatch({
    req <- httr2::request(paste0(.morie_llm_ollama_base(), "/api/tags"))
    req <- httr2::req_timeout(req, timeout)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp) < 400
  }, error = function(e) FALSE)
  .morie_llm_cache$ollama_cached <- out
  out
}

#' Detect the active LLM provider
#' @return Character scalar provider key: ollama / gemini / api / openai / local.
#' @examples
#' old <- options(morie.llm.ollama_cached = FALSE)
#' morie_llm_detect_provider()
#' options(old)
#' @export
morie_llm_detect_provider <- function() {
  if (morie_llm_probe_ollama())                                 return("ollama")
  if (!is.null(.morie_llm_gemini_key()))                        return("gemini")
  if (!is.null(.morie_llm_api_base()) && !is.null(.morie_llm_api_key()))
                                                                return("api")
  if (!is.null(.morie_llm_openai_key()))                        return("openai")
  "local"
}

#' Internal helper: Morie Llm System Prompt
#' @noRd
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

#' Internal helper: Morie Llm Messages
#' @noRd
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
#' @examples
#' \donttest{
#' msgs <- list(list(role = "user", content = "Say hello"))
#' # Second arg is whatever model your Ollama server serves (see `ollama list`).
#' res <- try(morie_llm_request_completion("http://localhost:11434", "your-model", msgs))
#' }
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
    .morie_to_json(payload, auto_unbox = TRUE),
    type = "application/json")
  req <- httr2::req_timeout(req, timeout)
  resp <- httr2::req_perform(req)
  .morie_from_json(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

#' Internal helper: Morie Llm Extract Text
#' @noRd
.morie_llm_extract_text <- function(data) {
  choices <- data$choices
  if (length(choices) == 0L) return("")
  msg <- choices[[1]]$message
  if (is.null(msg)) "" else (msg$content %||% "")
}

#' Internal helper: Morie Llm Local Fallback
#' @noRd
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
#' @examples
#' \donttest{
#' old <- options(morie.llm.ollama_cached = FALSE)
#' out <- try(morie_llm_ask("In one word, what is 2 + 2?"))
#' print(out)
#' options(old)
#' }
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
    add(.morie_llm_ollama_base(), model %||% .morie_llm_ollama_default_model(), NULL)
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
#' @examples
#' old <- options(morie.llm.ollama_cached = FALSE)
#' morie_llm_agent_available()
#' options(old)
#' @export
morie_llm_agent_available <- function() {
  morie_llm_detect_provider() != "local"
}



#' Ask the best available LLM provider, accepting a multi-turn messages list
#'
#' R port of ``morie.llm.ask_multi``.  Unlike :func:`morie_llm_ask`, this
#' accepts a pre-built ``messages`` list (each element: ``role``/``content``)
#' enabling multi-turn conversation.  Streaming is not supported in the R
#' port -- this always returns a single character scalar.
#'
#' Provider fall-through order mirrors :func:`morie_llm_detect_provider`:
#' ollama -> gemini -> api -> openai -> local.
#'
#' @param messages list of role/content lists.
#' @param providers Optional character vector forcing a specific provider
#'   ordering.  When NULL the auto-detected provider is tried first, then
#'   the remaining providers in priority order.
#' @param model Optional model identifier.
#' @param timeout HTTP timeout in seconds.
#' @return Character scalar response text.
#' @examples
#' \donttest{
#' old <- options(morie.llm.ollama_cached = FALSE)
#' msgs <- list(list(role = "user", content = "hello"))
#' out <- try(morie_llm_ask_multi(msgs))
#' print(out)
#' options(old)
#' }
#' @export
morie_llm_ask_multi <- function(messages, providers = NULL,
                                model = NULL, timeout = 120) {
  stopifnot(is.list(messages))

  if (is.null(providers)) {
    detected <- morie_llm_detect_provider()
    providers <- unique(c(detected,
                          "ollama", "gemini",
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
    # HTTP-OpenAI-compatible providers
    cfg <- switch(prov,
      ollama = list(base = .morie_llm_ollama_base(),
                    mdl = model %||% .morie_llm_ollama_default_model(),
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
