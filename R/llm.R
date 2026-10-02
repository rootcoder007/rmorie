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
# There is deliberately no anonymous community-server tier: sending prompts
# to unvetted volunteer hosts found through a public registry is a security
# risk, so that provider was removed from both the Python and R arms.
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
#' # \dontrun (not \donttest): this reaches the network. \donttest
#' # examples ARE run by pkgdown and by CRAN's --run-donttest.
#' \dontrun{
#' if (requireNamespace("httr2", quietly = TRUE)) {
#'   # Point at your own Ollama server, then see what it serves:
#'   Sys.setenv(OLLAMA_HOST = "http://localhost:11434")
#'   models <- morie_llm_ollama_models()
#'   models$name
#'   }
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
.morie_llm_api_base    <- function() {
  v <- .morie_llm_env("LLM_API_BASE_URL")
  if (!nzchar(v)) v <- .morie_llm_stored_provider("api_base_url")
  if (nzchar(v)) sub("/+$", "", v) else NULL
}
#' Internal helper: Morie Llm Api Key
#' @noRd
.morie_llm_api_key     <- function() {
  v <- .morie_llm_env("LLM_API_KEY")
  if (!nzchar(v)) v <- .morie_llm_stored_provider("api_key")
  if (nzchar(v)) v else NULL
}
#' Internal helper: the model for the attached endpoint
#' @noRd
.morie_llm_api_model   <- function() {
  v <- .morie_llm_env("MORIE_API_MODEL")
  if (!nzchar(v)) v <- .morie_llm_stored_provider("api_model")
  if (nzchar(v)) v else DEFAULT_API_MODEL
}
#' Internal helper: one field of the endpoint attached with `provider set`
#' @noRd
.morie_llm_stored_provider <- function(field) {
  d <- tryCatch(.morie_llm_read_credentials(), error = function(e) list())
  v <- d[[field]]
  if (is.character(v) && length(v) == 1L && !is.na(v)) trimws(v) else ""
}
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
#' # \dontrun (not \donttest): this reaches the network. \donttest
#' # examples ARE run by pkgdown and by CRAN's --run-donttest.
#' \dontrun{
#' if (requireNamespace("httr2", quietly = TRUE)) {
#'   old <- options(morie.llm.ollama_cached = FALSE)
#'   morie_llm_probe_ollama()
#'   options(old)
#' }
#' }
#' @export
morie_llm_probe_ollama <- function(timeout = 2) {
  cache <- .morie_llm_cache$ollama_cached
  if (!is.null(cache)) return(cache)  # a cached answer needs no HTTP client
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
  if (.morie_llm_no_net()) return(FALSE)
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
#' @return Character scalar provider key: ollama / hosted / gemini / api / openai / local.
#' The hosted tier (llm.rmorie.com) answers only after \code{morie_llm_login()}.
#' @examples
#' old <- options(morie.llm.ollama_cached = FALSE)
#' morie_llm_detect_provider()
#' options(old)
#' @export
morie_llm_detect_provider <- function() {
  if (morie_llm_probe_ollama())                                 return("ollama")
  if (morie_llm_probe_hosted())                                 return("hosted")
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
#' if (requireNamespace("httr2", quietly = TRUE)) {
#'   \donttest{
#'   msgs <- list(list(role = "user", content = "Say hello"))
#'   # Second arg is whatever model your Ollama server serves (see `ollama list`).
#'   res <- try(morie_llm_request_completion("http://localhost:11434", "your-model", msgs))
#'   }
#' }
#' @export
morie_llm_request_completion <- function(base_url, model, messages,
                                         api_key = NULL, timeout = 120) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("morie_llm_request_completion requires httr2 and jsonlite.")
  }
  url <- paste0(base_url, "/v1/chat/completions")
  # reasoning models spend tokens thinking first: without room the answer is an empty content
  payload <- list(model = model, messages = messages, stream = FALSE, max_tokens = 4096L)
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
    "MORIE is running in local-only mode (no LLM provider detected).\n\n",
    "The analyses do not need a model: morie_run_pipeline(), ",
    "morie_run_morie_module() and every morie_* estimator work as they are. ",
    "To get answers from a model, enable one of these (tried in this order):\n",
    "  1. a local Ollama: curl -fsSL https://ollama.com/install.sh | sh\n",
    "  2. the hosted MORIE tier at https://llm.rmorie.com: ",
    "morie_llm_login() in R, or `rmorie login` from the shell after ",
    "install_cli()\n",
    "  3. your own key: GEMINI_API_KEY, LLM_API_BASE_URL + LLM_API_KEY, ",
    "or OPENAI_API_KEY\n",
    "morie_llm_detect_provider() reports what is reachable from here."
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
  if (provider %in% c("ollama", "hosted") && !is.null(.morie_llm_hosted_base()) &&
      !is.null(.morie_llm_hosted_key())) {
    add(.morie_llm_hosted_base(), model %||% .morie_llm_hosted_model_available(),
        .morie_llm_hosted_key())
  }
  if (provider %in% c("ollama", "hosted", "gemini") && !is.null(.morie_llm_gemini_key())) {
    add(GEMINI_BASE_URL, model %||% .morie_llm_gemini_model(),
        .morie_llm_gemini_key())
  }
  if (!is.null(.morie_llm_api_base()) && !is.null(.morie_llm_api_key())) {
    add(.morie_llm_api_base(), model %||% .morie_llm_api_model(),
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
                      mdl = model %||% .morie_llm_api_model(),
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

# ---- The hosted MORIE inference tier (llm.rmorie.com) -----------------------
#
# An authenticated, rate-limited OpenAI-compatible endpoint run by the
# project. It sits second in the provider chain, after a local Ollama and
# before any cloud API key, and only speaks once the user has logged in:
# morie_llm_login() runs the GitHub device flow, receives a per-user key and
# stores it with mode 0600 in $XDG_CONFIG_HOME/morie/credentials.json -- the
# same file the Python package reads, so one login serves both.

DEFAULT_HOSTED_BASE_URL <- "https://llm.rmorie.com"
DEFAULT_HOSTED_AUTH_URL <- "https://llm.rmorie.com/auth"
DEFAULT_HOSTED_MODEL    <- "minimax-m3:cloud"

#' Internal helper: the hosted endpoint, or NULL when disabled by an override
#' of "" (POSIX) or "off" (any platform; Windows cannot hold an empty variable)
#' @noRd
.morie_llm_hosted_base <- function() {
  if (nzchar(Sys.getenv("MORIE_HOSTED_BASE_URL", unset = "")) ||
      "MORIE_HOSTED_BASE_URL" %in% names(Sys.getenv())) {
    v <- sub("/+$", "", trimws(Sys.getenv("MORIE_HOSTED_BASE_URL")))
    return(if (nzchar(v) && !tolower(v) %in% c("off", "none", "disabled")) v else NULL)
  }
  DEFAULT_HOSTED_BASE_URL
}

#' Internal helper: the sign-in service of the hosted tier
#' @noRd
.morie_llm_hosted_auth <- function() {
  sub("/+$", "", .morie_llm_env("MORIE_HOSTED_AUTH_URL", DEFAULT_HOSTED_AUTH_URL))
}

#' Internal helper: the hosted model name
#' @noRd
.morie_llm_hosted_model <- function() .morie_llm_env("MORIE_HOSTED_MODEL", DEFAULT_HOSTED_MODEL)

#' Internal helper: the credentials file shared with the Python package
#' @noRd
.morie_llm_credentials_path <- function() {
  base <- trimws(Sys.getenv("XDG_CONFIG_HOME", unset = ""))
  if (!nzchar(base)) base <- file.path(path.expand("~"), ".config")
  file.path(base, "morie", "credentials.json")
}

#' Internal helper: read the credentials file (a named list, empty when absent)
#' @noRd
.morie_llm_read_credentials <- function() {
  p <- .morie_llm_credentials_path()
  if (!file.exists(p)) return(list())
  out <- tryCatch(.morie_from_json(paste(readLines(p, warn = FALSE), collapse = "\n"),
                                   simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.list(out)) out else list()
}

#' Internal helper: write the credentials file with owner-only permissions
#' @noRd
.morie_llm_write_credentials <- function(data) {
  p <- .morie_llm_credentials_path()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(p, ".tmp")
  writeLines(.morie_to_json(data, auto_unbox = TRUE, pretty = TRUE), tmp)
  Sys.chmod(tmp, mode = "0600")
  file.rename(tmp, p)
  invisible(p)
}

#' Internal helper: the user's hosted key -- MORIE_HOSTED_KEY, else the stored credential
#' @noRd
.morie_llm_hosted_key <- function() {
  v <- .morie_llm_env("MORIE_HOSTED_KEY")
  if (nzchar(v)) return(v)
  k <- .morie_llm_read_credentials()$hosted_key
  if (is.character(k) && length(k) == 1L && nzchar(trimws(k))) trimws(k) else NULL
}

#' Probe the hosted MORIE tier
#'
#' TRUE when the user is logged in and llm.rmorie.com accepts the key. The
#' answer is cached for the session, and no request is made without a key.
#' @param timeout Probe timeout in seconds.
#' @return Logical scalar.
#' @examples
#' morie_llm_probe_hosted()
#' @export
morie_llm_probe_hosted <- function(timeout = 2) {
  cache <- .morie_llm_cache$hosted_cached
  if (!is.null(cache)) return(cache)
  base <- .morie_llm_hosted_base()
  key <- .morie_llm_hosted_key()
  if (is.null(base) || is.null(key) || !requireNamespace("httr2", quietly = TRUE) ||
      .morie_llm_no_net()) {
    .morie_llm_cache$hosted_cached <- FALSE
    return(FALSE)
  }
  out <- tryCatch({
    req <- httr2::request(paste0(base, "/v1/models"))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", key))
    req <- httr2::req_timeout(req, timeout)
    resp <- httr2::req_perform(req)
    ok <- httr2::resp_status(resp) < 400
    if (ok) {
      ids <- tryCatch(vapply(httr2::resp_body_json(resp)$data, function(m) as.character(m$id %||% ""), ""),
                      error = function(e) character())
      ids <- ids[nzchar(ids)]
      .morie_llm_cache$hosted_models <- if (length(ids)) ids else NULL
    }
    ok
  }, error = function(e) FALSE)
  .morie_llm_cache$hosted_cached <- out
  out
}

#' Internal helper: the configured hosted model, or the gateway's first model
#' when the configured one is not offered (cloud models get retired upstream;
#' a stale default must not turn every request into a 400)
#' @noRd
.morie_llm_hosted_model_available <- function() {
  wanted <- .morie_llm_hosted_model()
  morie_llm_probe_hosted()
  listed <- .morie_llm_cache$hosted_models
  if (length(listed) && !wanted %in% listed) return(listed[[1L]])
  wanted
}

#' Models offered by the hosted MORIE LLM tier
#'
#' Asks the gateway (\code{GET /v1/models}) which models the stored key may
#' use. The first call probes the tier; later calls reuse the cached answer
#' unless \code{refresh = TRUE}.
#'
#' @param refresh Logical; ask the gateway again instead of using the cache.
#' @return A character vector of model names, empty when not logged in or the
#'   gateway is unreachable, with attribute \code{"default"}: the model
#'   \code{morie_llm_ask()} uses when none is named.
#' @examples
#' \donttest{
#' m <- morie_llm_hosted_models()
#' attr(m, "default")
#' }
#' @export
morie_llm_hosted_models <- function(refresh = FALSE) {
  if (isTRUE(refresh)) {
    .morie_llm_cache$hosted_cached <- NULL
    .morie_llm_cache$hosted_models <- NULL
  }
  if (!morie_llm_probe_hosted()) return(structure(character(), default = NULL))
  ids <- .morie_llm_cache$hosted_models %||% character()
  structure(ids, default = .morie_llm_hosted_model_available())
}

#' Sign in to the hosted MORIE LLM tier
#'
#' Runs the GitHub device flow against the sign-in service of
#' llm.rmorie.com: a short code and a URL are printed (and the browser
#' opened when possible); once the sign-in is approved the service mints a
#' per-user, rate-limited key, which is stored with mode 0600 in
#' \code{$XDG_CONFIG_HOME/morie/credentials.json} and used by both the R
#' and the Python package. Nothing is sent to the tier before that.
#' @param open_browser Open the verification URL in the browser.
#' @param poll_max_seconds Give up after this many seconds.
#' @param email Sign in with a one-time code emailed to this address instead
#'   of GitHub; the code is read from the console (or from \code{code}).
#' @param code The emailed code, when not read interactively.
#' @param token A key obtained elsewhere (the website, or one the gateway
#'   emailed); stored directly, no sign-in round trip.
#' @param to_email With \code{email}: have the gateway email the key instead of
#'   returning it. Nothing is stored; paste it later with \code{token}.
#' @return The key, invisibly.
#' @examples
#' \dontrun{
#' morie_llm_login()
#' morie_llm_login(email = "you@example.com")
#' }
#' @export
morie_llm_login <- function(open_browser = interactive(), poll_max_seconds = 600,
                            email = NULL, code = NULL, token = NULL, to_email = FALSE) {
  if (!is.null(token)) return(.morie_llm_store_token(token))
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("morie_llm_login() needs the httr2 package")
  auth <- .morie_llm_hosted_auth()
  if (!is.null(email)) return(.morie_llm_login_email(auth, email, code, to_email))
  start <- httr2::req_perform(httr2::req_method(httr2::request(paste0(auth, "/device/code")), "POST"))
  info <- httr2::resp_body_json(start)
  message(sprintf("Sign in at %s and enter the code: %s", info$verification_uri, info$user_code))
  if (isTRUE(open_browser)) try(utils::browseURL(info$verification_uri), silent = TRUE)
  interval <- as.numeric(info$interval %||% 5)
  deadline <- Sys.time() + poll_max_seconds
  while (Sys.time() < deadline) {
    Sys.sleep(interval)
    req <- httr2::req_body_json(httr2::request(paste0(auth, "/device/token")),
                                list(device_code = info$device_code))
    resp <- httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
    st <- httr2::resp_status(resp)
    if (st == 200L) {
      body <- httr2::resp_body_json(resp)
      if (!is.null(body$api_key)) {
        data <- .morie_llm_read_credentials()
        data$hosted_key <- body$api_key
        data$hosted_user <- body$user %||% ""
        data$hosted_base_url <- .morie_llm_hosted_base()
        p <- .morie_llm_write_credentials(data)
        .morie_llm_cache$hosted_cached <- NULL
        .morie_llm_cache$hosted_models <- NULL
        message(sprintf("Logged in as %s; key stored in %s", body$user %||% "user", p))
        return(invisible(body$api_key))
      }
    } else if (st == 428L) {
      interval <- as.numeric(tryCatch(httr2::resp_body_json(resp)$interval, error = function(e) NULL) %||% interval)
    } else {
      stop(sprintf("the sign-in service answered %d", st))
    }
  }
  stop("the sign-in was not completed in time; run morie_llm_login() again")
}

#' Internal helper: email sign-in (code request, then verification)
#' @noRd
.morie_llm_store_token <- function(token) {
  token <- trimws(as.character(token))
  if (!length(token) || !nzchar(token)) stop("an empty token cannot be stored")
  data <- .morie_llm_read_credentials()
  data$hosted_key <- token
  data$hosted_base_url <- .morie_llm_hosted_base()
  p <- .morie_llm_write_credentials(data)
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$hosted_models <- NULL
  if (isTRUE(morie_llm_probe_hosted())) {
    message(sprintf("Token stored in %s; the gateway accepts it.", p))
  } else {
    message(sprintf("Token stored in %s, but the gateway did not accept it (check the key).", p))
  }
  invisible(token)
}

.morie_llm_login_email <- function(auth, email, code = NULL, to_email = FALSE) {
  email <- trimws(email)
  if (!grepl("@", email, fixed = TRUE)) stop("an email address is required")
  perform <- function(path, body) {
    req <- httr2::req_body_json(httr2::request(paste0(auth, path)), body)
    httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
  }
  if (is.null(code)) {
    resp <- perform("/email/code", list(email = email))
    if (httr2::resp_status(resp) != 200L) {
      err <- tryCatch(httr2::resp_body_json(resp)$error, error = function(e) NULL)
      stop(err %||% sprintf("the sign-in service answered %d", httr2::resp_status(resp)))
    }
    message(sprintf("A 6-digit code was sent to %s (valid for 10 minutes).", email))
    code <- trimws(readline("Enter the code: "))
  }
  payload <- list(email = email, code = trimws(code))
  if (isTRUE(to_email)) payload$deliver <- "email"
  resp <- perform("/email/verify", payload)
  if (httr2::resp_status(resp) != 200L) {
    err <- tryCatch(httr2::resp_body_json(resp)$error, error = function(e) NULL)
    stop(err %||% sprintf("the sign-in service answered %d", httr2::resp_status(resp)))
  }
  body <- httr2::resp_body_json(resp)
  if (isTRUE(to_email)) {
    if (!isTRUE(body$sent)) stop("the sign-in service did not confirm the email")
    message(sprintf("Your key was emailed to %s. Store it with morie_llm_login(token = ) or: rmorie login --token", email))
    return(invisible(""))
  }
  data <- .morie_llm_read_credentials()
  data$hosted_key <- body$api_key
  data$hosted_user <- body$user %||% ""
  data$hosted_base_url <- .morie_llm_hosted_base()
  p <- .morie_llm_write_credentials(data)
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$hosted_models <- NULL
  message(sprintf("Logged in; key stored in %s", p))
  invisible(body$api_key)
}

#' Forget the hosted MORIE LLM key
#' @return TRUE (invisibly) when a key was removed.
#' @examples
#' \dontrun{
#' morie_llm_logout()
#' }
#' @export
morie_llm_logout <- function() {
  data <- .morie_llm_read_credentials()
  had <- !is.null(data$hosted_key)
  data$hosted_key <- NULL
  data$hosted_user <- NULL
  if (length(data)) .morie_llm_write_credentials(data) else unlink(.morie_llm_credentials_path())
  .morie_llm_cache$hosted_cached <- NULL
  .morie_llm_cache$hosted_models <- NULL
  message(if (had) "Hosted key removed." else "No hosted key was stored.")
  invisible(had)
}

#' Attach your own model endpoint
#'
#' Stores an OpenAI-compatible endpoint (chat completions at
#' \code{BASE_URL/chat/completions}) in the credentials file, where both this
#' package and the Python package read it: OpenAI, Anthropic's compatibility
#' endpoint (\code{https://api.anthropic.com/v1}), OpenRouter, Mistral, Groq, a
#' local LM Studio / vLLM / llama.cpp server, and so on. The environment
#' variables \code{LLM_API_BASE_URL}, \code{LLM_API_KEY} and
#' \code{MORIE_API_MODEL} take precedence when set.
#'
#' @param base_url The endpoint, e.g. \code{"https://api.openai.com/v1"}.
#' @param key The API key for it.
#' @param model Optional model name to ask by default.
#' @return \code{morie_llm_provider_set()} and \code{morie_llm_provider_show()}
#'   return the stored fields invisibly; \code{morie_llm_provider_unset()}
#'   returns \code{TRUE} when something was detached.
#' @examples
#' \dontrun{
#' morie_llm_provider_set("https://api.openai.com/v1", "sk-...", model = "gpt-4o-mini")
#' morie_llm_provider_show()
#' morie_llm_provider_unset()
#' }
#' @export
morie_llm_provider_set <- function(base_url, key, model = NULL) {
  base_url <- sub("/+$", "", trimws(base_url))
  if (!grepl("^https?://", base_url)) stop("base_url must start with http:// or https://", call. = FALSE)
  if (!nzchar(trimws(key))) stop("the key is empty", call. = FALSE)
  d <- .morie_llm_read_credentials()
  d$api_base_url <- base_url
  d$api_key <- trimws(key)
  if (!is.null(model) && nzchar(trimws(model))) d$api_model <- trimws(model) else d$api_model <- NULL
  .morie_llm_write_credentials(d)
  message("Endpoint attached: ", base_url, if (!is.null(d$api_model)) paste0(" (model ", d$api_model, ")") else "")
  invisible(d[c("api_base_url", "api_key", "api_model")])
}

#' @rdname morie_llm_provider_set
#' @export
morie_llm_provider_show <- function() {
  d <- .morie_llm_read_credentials()
  if (is.null(d$api_base_url)) {
    message("No endpoint attached. Attach one with morie_llm_provider_set(base_url, key) or: rmorie provider set --base-url URL --key KEY")
  } else {
    k <- as.character(d$api_key %||% "")
    message("Endpoint: ", d$api_base_url, "\nModel:    ", d$api_model %||% "server default",
            "\nKey:      ", substr(k, 1, 4), "...", substr(k, nchar(k) - 2, nchar(k)), " (", nchar(k), " chars)")
  }
  invisible(d[c("api_base_url", "api_key", "api_model")])
}

#' @rdname morie_llm_provider_set
#' @export
morie_llm_provider_unset <- function() {
  d <- .morie_llm_read_credentials()
  had <- any(c("api_base_url", "api_key", "api_model") %in% names(d))
  d$api_base_url <- NULL
  d$api_key <- NULL
  d$api_model <- NULL
  .morie_llm_write_credentials(d)
  message(if (had) "Endpoint detached." else "No endpoint was attached.")
  invisible(had)
}


# Does the gateway accept this key? (asked before a key is stored)
.morie_llm_probe_token <- function(token) {
  base <- .morie_llm_hosted_base()
  if (is.null(base) || !nzchar(base)) return(FALSE)
  res <- tryCatch({
    req <- httr2::request(paste0(sub("/+$", "", base), "/v1/models"))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", trimws(token)))
    req <- httr2::req_timeout(req, 20)
    httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
  }, error = function(e) NULL)
  !is.null(res) && httr2::resp_status(res) < 300
}

# The stored key reaches the gateway but the gateway refuses it (401/403), as opposed to no gateway.
.morie_llm_hosted_rejected <- function() {
  key <- .morie_llm_hosted_key()
  base <- .morie_llm_hosted_base()
  if (is.null(key) || is.null(base) || !nzchar(base)) return(FALSE)
  res <- tryCatch({
    req <- httr2::request(paste0(sub("/+$", "", base), "/v1/models"))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", key))
    req <- httr2::req_timeout(req, 20)
    httr2::req_perform(httr2::req_error(req, is_error = function(r) FALSE))
  }, error = function(e) NULL)
  !is.null(res) && httr2::resp_status(res) %in% c(401L, 403L)
}
