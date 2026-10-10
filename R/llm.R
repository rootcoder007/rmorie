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
  # OLLAMA_HOST, OLLAMA_BASE_URL, then `rmorie config set ollama.url` (llm.json)
  host <- .morie_llm_setting("ollama.url", also = "OLLAMA_BASE_URL") %||% DEFAULT_OLLAMA_BASE_URL
  if (.morie_llm_off(host)) host <- DEFAULT_OLLAMA_BASE_URL  # shown, never probed (.morie_llm_ollama_off)
  if (!grepl("^https?://", host)) host <- paste0("http://", host)
  sub("/+$", "", host)
}

#' Internal helper: TRUE when Ollama is switched off (ollama.url / OLLAMA_HOST = off)
#' @noRd
.morie_llm_ollama_off <- function() {
  v <- .morie_llm_setting("ollama.url", also = "OLLAMA_BASE_URL")
  !is.null(v) && .morie_llm_off(v)
}

#' Internal helper: the bearer key of an Ollama server that wants one
#' @noRd
.morie_llm_ollama_key <- function() .morie_llm_setting("ollama.key")

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
  if (missing(base) && .morie_llm_ollama_off()) return(empty)
  res <- .morie_llm_http(paste0(base, "/api/tags"), headers = .morie_llm_bearer(.morie_llm_ollama_key()),
                         timeout = timeout)
  models <- if (res$status == 200L) .morie_llm_http_json(res)$models %||% list() else list()
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
  env <- .morie_llm_setting("ollama.model")  # OLLAMA_MODEL, else `rmorie config set ollama.model`
  if (!is.null(env)) return(env)
  # the probe already asked the server what it serves
  probed <- attr(.morie_llm_cache$ollama_cached, "models")
  if (missing(base) && !is.null(probed)) return(if (length(probed)) probed[[1L]] else NA_character_)
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
  # MORIE_LLM_BASE_URL / LLM_API_BASE_URL, then `rmorie config set own.url`, then `rmorie provider set`
  v <- .morie_llm_setting("own.url", also = "LLM_API_BASE_URL") %||% .morie_llm_stored_provider("api_base_url")
  if (nzchar(v) && !.morie_llm_off(v)) sub("/+$", "", v) else NULL
}
#' Internal helper: Morie Llm Api Key
#' @noRd
.morie_llm_api_key     <- function() {
  v <- .morie_llm_setting("own.key", also = "LLM_API_KEY") %||% .morie_llm_stored_provider("api_key")
  if (nzchar(v)) v else NULL
}
#' Internal helper: the model for the attached endpoint
#' @noRd
.morie_llm_api_model   <- function() {
  v <- .morie_llm_setting("own.model", also = "MORIE_API_MODEL") %||% .morie_llm_stored_provider("api_model")
  if (nzchar(v)) v else DEFAULT_API_MODEL
}

#' Internal helper: TRUE when your own endpoint can be asked -- an address plus
#' a key, or an address on this machine (LM Studio, llama.cpp need no key)
#' @noRd
.morie_llm_api_usable <- function() {
  base <- .morie_llm_api_base()
  !is.null(base) && (!is.null(.morie_llm_api_key()) ||
                       grepl("^https?://(localhost|127\\.0\\.0\\.1|\\[::1\\])(:[0-9]+)?(/|$)", base))
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
  if (!is.null(cache)) return(isTRUE(as.vector(cache)))  # a cached answer needs no HTTP client
  if (.morie_llm_no_net() || .morie_llm_ollama_off()) return(FALSE)
  res <- .morie_llm_http(paste0(.morie_llm_ollama_base(), "/api/tags"),
                         headers = .morie_llm_bearer(.morie_llm_ollama_key()), timeout = timeout)
  out <- res$status > 0L && res$status < 400L
  # what the server serves rides along: a server with nothing pulled cannot answer a question
  models <- if (out) {
    tryCatch(vapply(.morie_llm_http_json(res)$models %||% list(),
                    function(m) as.character(m$name %||% m$model %||% ""), ""),
             error = function(e) character())
  }
  .morie_llm_cache$ollama_cached <- structure(out, models = if (out) models[nzchar(models)])
  out
}

#' Internal helper: TRUE when the automatic route may use Ollama -- it answers
#' AND has a model to use (OLLAMA_MODEL / ollama.model named, or at least one
#' pulled). A running server with nothing pulled is skipped, so a logged-in
#' hosted tier behind it is reached; a cached answer without a model list
#' (from before the probe recorded one) counts as usable.
#' @noRd
.morie_llm_ollama_usable <- function() {
  if (!isTRUE(morie_llm_probe_ollama())) return(FALSE)
  if (!is.null(.morie_llm_setting("ollama.model"))) return(TRUE)
  models <- attr(.morie_llm_cache$ollama_cached, "models")
  is.null(models) || length(models) > 0L
}

#' Detect the active LLM provider
#' @param route \code{NULL} (the saved setting: \code{MORIE_LLM_ROUTE}, else
#'   \code{\link{morie_llm_config}}'s \code{route}, else \code{"auto"}),
#'   \code{"auto"}, \code{"own"}, \code{"ollama"} or \code{"hosted"}.
#' @return Character scalar provider key: ollama / gemini / api / openai / hosted / local.
#' With the automatic route, in that order of preference: a local Ollama that
#' has a model (a server with nothing pulled is skipped), then your own keys
#' and endpoint, then the hosted MORIE tier as a last resort (it answers only
#' after \code{morie_llm_login()}; keys are issued on request at
#' \url{https://rmorie.com/access}). A forced route returns its provider
#' (\code{"api"} for \code{"own"}) when it is set up, else \code{"local"}.
#' @examples
#' old <- options(morie.llm.ollama_cached = FALSE)
#' morie_llm_detect_provider()
#' options(old)
#' @export
morie_llm_detect_provider <- function(route = NULL) {
  route <- .morie_llm_route(route)
  if (!identical(route, "auto")) {
    ok <- switch(route,
      own = .morie_llm_api_usable(),
      ollama = morie_llm_probe_ollama(),  # forced: an empty server is reported by ask, not skipped
      hosted = morie_llm_probe_hosted())
    return(if (isTRUE(ok)) .morie_llm_route_provider(route) else "local")
  }
  if (.morie_llm_ollama_usable())                               return("ollama")
  if (!is.null(.morie_llm_gemini_key()))                        return("gemini")
  if (.morie_llm_api_usable())                                  return("api")
  if (!is.null(.morie_llm_openai_key()))                        return("openai")
  # the hosted tier is a last resort, behind every key of the user's own
  if (morie_llm_probe_hosted())                                 return("hosted")
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
  url <- paste0(.morie_llm_v1(base_url), "/chat/completions")
  # reasoning models spend tokens thinking first: without room the answer is an empty content
  payload <- list(model = model, messages = messages, stream = FALSE, max_tokens = 4096L)
  if (grepl("localhost|127\\.0\\.0\\.1", base_url)) {
    payload$max_tokens <- 4096L
    timeout <- max(timeout, 300)
  }
  res <- .morie_llm_http(url, body = .morie_to_json(payload, auto_unbox = TRUE),
                         headers = .morie_llm_bearer(api_key), timeout = timeout)
  if (res$status == 0L) {
    stop(sprintf("%s did not answer (%s)", base_url, .morie_llm_redact(res$body, api_key)), call. = FALSE)
  }
  if (res$status >= 400L) {
    err <- .morie_llm_http_json(res)$error
    msg <- if (is.list(err)) err$message %||% "" else as.character(err %||% "")
    msg <- .morie_llm_redact(msg, api_key)
    stop(sprintf("HTTP %d from %s%s", res$status, base_url, if (nzchar(msg)) paste0(": ", msg) else ""), call. = FALSE)
  }
  .morie_from_json(res$body, simplifyVector = FALSE)
}

# The OpenAI-style API root of an endpoint: a base that already names its version
# (https://api.example.org/v1, Gemini's .../v1beta/openai) is used as it is, a bare
# host (https://llm.rmorie.com, an Ollama server) gets /v1. `provider set` and
# own.url are documented as the part before /chat/completions, so .../v1 must not
# become .../v1/v1.
.morie_llm_v1 <- function(base) {
  base <- sub("/+$", "", base)
  if (grepl("/v[0-9]+[a-z0-9]*(/openai)?$", base)) base else paste0(base, "/v1")
}

# One HTTP request through the package's own libcurl backend (no httr2): list(status, body).
# status 0 means no answer (no server, timeout, DNS).
.morie_llm_http <- function(url, body = NULL, headers = character(), timeout = 30) {
  t <- as.integer(ceiling(timeout))
  r <- tryCatch(
    if (is.null(body)) {
      .morie_http_get_with_status(url, timeout_s = t, headers = as.character(headers))
    } else {
      .morie_http_post_with_status(url, as.character(body), "application/json", timeout_s = t,
                                   headers = as.character(headers))
    },
    error = function(e) list(body = conditionMessage(e), status_code = 0L)
  )
  list(status = as.integer(r$status_code %||% 0L), body = paste(as.character(r$body %||% ""), collapse = ""))
}

.morie_llm_http_json <- function(res) {
  tryCatch(.morie_from_json(res$body, simplifyVector = FALSE), error = function(e) list())
}

.morie_llm_bearer <- function(key) if (!is.null(key) && nzchar(key)) paste("Authorization: Bearer", trimws(key)) else character()

# The sign-in service names the page to open: only an https address of a public host is
# handed to the browser (bricklayer's fourth review: the auth host chose any URL, any scheme).
.morie_llm_browsable <- function(uri) {
  is.character(uri) && length(uri) == 1L &&
    grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?(/|$)", uri) &&
    !grepl("^https://(localhost|127\\.|10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[01])\\.|169\\.254\\.|0\\.|100\\.(6[4-9]|[7-9][0-9]|1[01][0-9]|12[0-7])\\.)",
           uri, ignore.case = TRUE) &&
    !grepl("^https://[^/]*\\.(local|internal|localhost|lan|home|corp)(:[0-9]+)?(/|$)", uri, ignore.case = TRUE) &&
    !grepl("^https://[^/.]+(:[0-9]+)?(/|$)", uri)  # a single-label name is not a public host
}

# A server's error text can quote the key it was sent, in any phrasing: the key itself is
# redacted by value, the usual bearer / sk- spellings by shape, the LiteLLM phrasings cut.
.morie_llm_redact <- function(msg, secret = NULL) {
  msg <- sub("(?i)[.,;]?\\s*(received api key|key hash).*$", "", msg, perl = TRUE)
  for (sec in secret) if (is.character(sec) && nzchar(sec)) msg <- gsub(sec, "<key>", msg, fixed = TRUE)
  msg <- gsub("(?i)bearer\\s+[A-Za-z0-9._~+/=-]{8,}", "Bearer <key>", msg, perl = TRUE)
  gsub("\\bsk-[A-Za-z0-9._-]{6,}", "<key>", msg, perl = TRUE)
}

#' Internal helper: Morie Llm Extract Text
#' @noRd
.morie_llm_extract_text <- function(data) {
  choices <- data$choices
  if (length(choices) == 0L) return("")
  msg <- choices[[1]]$message
  if (is.null(msg)) "" else (msg$content %||% "")
}

#' Internal helper: kept for the hint sites; the hosted tier no longer needs httr2
#' @noRd
.morie_httr2_note <- function() ""

#' Internal helper: Morie Llm Local Fallback
#' @noRd
.morie_llm_local_fallback <- function(prompt, tried = FALSE) {
  # the attribute lets a script tell this text from an answer: isTRUE(attr(x, "fallback"))
  base <- tryCatch(.morie_llm_api_base(), error = function(e) NULL)
  hosted <- !is.null(tryCatch(.morie_llm_hosted_key(), error = function(e) NULL))
  head <- if (isTRUE(tried) || hosted) {
    "No LLM provider answered this time (one is configured; the line below says why).\n\n"
  } else if (!is.null(base) && nzchar(base)) {
    sprintf("MORIE is running in local-only mode (your endpoint %s did not answer).\n\n", base)
  } else {
    "MORIE is running in local-only mode (no LLM provider detected).\n\n"
  }
  structure(paste0(
    head,
    "The analyses do not need a model: morie_run_pipeline(), ",
    "morie_run_morie_module() and every morie_* estimator work as they are. ",
    "To get answers from a model, enable one of these (tried in this order):\n",
    "  1. a local Ollama: curl -fsSL https://ollama.com/install.sh | sh\n",
    "  2. the hosted MORIE tier at https://llm.rmorie.com: ",
    "morie_llm_login() in R (GitHub; email = \"you@example.com\" for a code by email), ",
    "or `rmorie login` / `rmorie login --email you@example.com` from the shell after install_cli()\n",
    "  3. your own key: GEMINI_API_KEY, LLM_API_BASE_URL + LLM_API_KEY, ",
    "or OPENAI_API_KEY\n",
    "Addresses, keys, models and the route are saved with morie_llm_config() ",
    "(shell: `rmorie config setup`; `rmorie help llm` explains each route).\n",
    "morie_llm_detect_provider() reports what is reachable from here."
  ), fallback = TRUE)
}

#' Send a prompt to the best available LLM provider
#'
#' R port of `morie.llm.ask`. With the automatic route it tries each provider
#' in priority order; on HTTP/timeout failure falls through to the next, and
#' finally to a static local help string. A route saved with
#' \code{\link{morie_llm_config}} (or \code{MORIE_LLM_ROUTE}, or the
#' \code{route} argument) other than \code{"auto"} asks that route only.
#'
#' @param prompt User question or instruction.
#' @param context Optional named list injected as text into the system prompt.
#' @param model Optional model override.
#' @param provider Optional provider override (ollama/gemini/api/openai/hosted/local;
#'   \code{"own"} is the same as \code{"api"}). NULL = auto-detect.
#' @param system_prompt Optional full system-prompt override.
#' @param timeout HTTP timeout in seconds. Default 120.
#' @param route \code{NULL} (the saved setting), \code{"auto"}, \code{"own"},
#'   \code{"ollama"} or \code{"hosted"}: a route other than auto is the only
#'   one asked.
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
                          timeout = 120, route = NULL) {
  route <- .morie_llm_route(route)
  forced <- !identical(route, "auto")
  if (is.null(provider)) provider <- if (forced) morie_llm_detect_provider(route = route) else morie_llm_detect_provider()
  if (identical(provider, "own")) provider <- "api"
  if (identical(provider, "local")) {
    if (forced) stop(.morie_llm_route_missing(route), call. = FALSE)
    return(.morie_llm_local_fallback(prompt))
  }

  messages <- .morie_llm_messages(prompt, context = context,
                                  system_prompt = system_prompt)
  # a forced route asks that provider only; auto keeps the fall-through chain
  chain <- if (forced) provider else c(
    if (provider == "ollama") "ollama",
    if (provider %in% c("ollama", "gemini")) "gemini",
    if (provider != "hosted") c("api", "openai"),
    "hosted")
  attempts <- Filter(Negate(is.null), lapply(chain, .morie_llm_attempt, model = model, strict = forced))
  if (length(attempts) == 0L) return(.morie_llm_local_fallback(prompt))

  for (a in attempts) {
    out <- tryCatch(
      .morie_llm_extract_text(
        morie_llm_request_completion(a$base, a$model, messages,
                                     api_key = a$key, timeout = timeout)),
      error = function(e) if (forced) stop(e) else NULL)
    if (!is.null(out) && nzchar(out)) return(out)
  }
  # a model the hosted tier does not list is the user's to fix, said in one line
  if (!is.null(model) && identical(provider, "hosted") && !is.null(.morie_llm_hosted_key())) {
    listed <- tryCatch(morie_llm_hosted_models(), error = function(e) character())
    if (length(listed) && !model %in% listed) {
      stop(sprintf("the hosted tier has no model '%s' (rmorie models lists the %d it has)", model, length(listed)),
           call. = FALSE)
    }
  }
  .morie_llm_local_fallback(prompt, tried = TRUE)
}

#' Internal helper: one provider's request -- list(base, model, key) -- or NULL
#' when it is not set up. An Ollama server with no model to use is skipped on
#' the automatic route and named in an error on a forced one (\code{strict}).
#' @noRd
.morie_llm_attempt <- function(provider, model = NULL, strict = FALSE) {
  switch(provider,
    ollama = {
      if (.morie_llm_ollama_off()) return(NULL)
      mdl <- model %||% .morie_llm_ollama_default_model()
      if (is.null(mdl) || is.na(mdl) || !nzchar(mdl)) {
        if (strict) stop(.morie_llm_no_ollama_model(), call. = FALSE)
        return(NULL)
      }
      list(base = .morie_llm_ollama_base(), model = mdl, key = .morie_llm_ollama_key())
    },
    gemini = if (!is.null(.morie_llm_gemini_key()))
      list(base = GEMINI_BASE_URL, model = model %||% .morie_llm_gemini_model(), key = .morie_llm_gemini_key()),
    api = if (.morie_llm_api_usable())
      list(base = .morie_llm_api_base(), model = model %||% .morie_llm_api_model(), key = .morie_llm_api_key()),
    openai = if (!is.null(.morie_llm_openai_key()))
      list(base = OPENAI_BASE_URL, model = model %||% DEFAULT_OPENAI_MODEL, key = .morie_llm_openai_key()),
    hosted = if (!is.null(.morie_llm_hosted_base()) && !is.null(.morie_llm_hosted_key()))
      list(base = .morie_llm_hosted_base(), model = model %||% .morie_llm_hosted_model_available(),
           key = .morie_llm_hosted_key()),
    NULL)
}

#' Internal helper: why Ollama cannot answer, and what to do
#' @noRd
.morie_llm_no_ollama_model <- function() {
  sprintf(paste0("local Ollama at %s has no model to use: pull one (`ollama pull NAME`) or ",
                 "`rmorie config set ollama.model NAME`, or `rmorie config set route hosted` to use the hosted tier"),
          .morie_llm_ollama_base())
}

#' Internal helper: why a forced route cannot be used
#' @noRd
.morie_llm_route_missing <- function(route) {
  why <- switch(route,
    own = "no endpoint is set: `rmorie config set own.url URL` (and own.model, own.key)",
    ollama = if (.morie_llm_ollama_off()) "Ollama is switched off (ollama.url = off)"
             else sprintf("nothing answers at %s: start Ollama, or `rmorie config set ollama.url ADDRESS`", .morie_llm_ollama_base()),
    hosted = if (is.null(.morie_llm_hosted_base())) "the hosted tier is switched off (hosted.url = off, or the services document)"
             else if (is.null(.morie_llm_hosted_key())) "not logged in: `rmorie login` (GitHub, or --email ADDRESS), or `rmorie login --token KEY`"
             else sprintf("the gateway %s did not accept the stored key or did not answer (rmorie doctor says which)", .morie_llm_hosted_base()),
    "")
  sprintf("the %s route is selected (route = %s) but %s; `rmorie config set route auto` goes back to the automatic order",
          route, route, why)
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

  route <- .morie_llm_route()
  if (is.null(providers) && !identical(route, "auto")) {
    # a saved route (morie_llm_config / MORIE_LLM_ROUTE) is the only one asked
    providers <- c(.morie_llm_route_provider(route), "local")
  } else if (is.null(providers)) {
    detected <- morie_llm_detect_provider()
    providers <- unique(c(detected,
                          "ollama", "gemini",
                          "api", "openai", "hosted", "local"))
  }

  fallback_prompt <- function() {
    user_msgs <- Filter(function(m) identical(m$role %||% "", "user"), messages)
    if (length(user_msgs) == 0L) "" else user_msgs[[length(user_msgs)]]$content %||% ""
  }

  for (prov in providers) {
    if (identical(prov, "local")) {
      return(.morie_llm_local_fallback(fallback_prompt()))
    }
    # HTTP-OpenAI-compatible providers (an Ollama server with no model is skipped)
    cfg <- .morie_llm_attempt(if (identical(prov, "own")) "api" else prov, model = model)
    if (is.null(cfg)) next
    out <- tryCatch(
      .morie_llm_extract_text(
        morie_llm_request_completion(cfg$base, cfg$model, messages,
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

# The defaults below are what an rmoriebricklayer too old to read the signed
# services document falls back to. With a current bricklayer the document at
# https://rmorie.com/.well-known/morie-services.json decides (ML-DSA-44, key
# pinned in bricklayer), so the hosted addresses can change without a release.
DEFAULT_HOSTED_BASE_URL <- "https://llm.rmorie.com"
DEFAULT_HOSTED_AUTH_URL <- "https://llm.rmorie.com/auth"
DEFAULT_HOSTED_MODEL    <- "minimax-m3:cloud"
ACCESS_REQUEST_URL      <- "https://rmorie.com/access"

#' Internal helper: the signed services document's llm block (through
#' rmoriebricklayer's cached, verified copy), or NULL when that bricklayer
#' predates it
#' @noRd
.morie_llm_services <- function() {
  if (!requireNamespace("rmoriebricklayer", quietly = TRUE)) return(NULL)
  ns <- asNamespace("rmoriebricklayer")
  if (!exists("bricklayer_services", envir = ns, inherits = FALSE)) return(NULL)
  tryCatch(get("bricklayer_services", envir = ns)(offline = TRUE)$llm, error = function(e) NULL)
}

#' Internal helper: one line on how to get a hosted key
#' @noRd
.morie_llm_access_hint <- function() {
  svc <- .morie_llm_services()
  sprintf("keys are personal and issued on request at %s; store one with `rmorie login --token` (R: morie_llm_login(token = ))",
          svc$request_access %||% ACCESS_REQUEST_URL)
}

#' Internal helper: the hosted endpoint, or NULL when disabled by an override
#' of "" (POSIX) or "off" (any platform; Windows cannot hold an empty variable),
#' or by the services document
#' @noRd
.morie_llm_hosted_base <- function() {
  saved <- .morie_llm_saved("hosted.url")  # `rmorie config set hosted.url` (llm.json)
  if (nzchar(Sys.getenv("MORIE_HOSTED_BASE_URL", unset = "")) ||
      "MORIE_HOSTED_BASE_URL" %in% names(Sys.getenv()) || !is.null(saved)) {
    v <- sub("/+$", "", trimws(Sys.getenv("MORIE_HOSTED_BASE_URL", unset = saved %||% "")))
    return(if (nzchar(v) && !.morie_llm_off(v)) v else NULL)
  }
  svc <- .morie_llm_services()
  if (is.null(svc)) return(DEFAULT_HOSTED_BASE_URL)
  if (!identical(svc$mode, "key") || !nzchar(svc$base_url %||% "")) return(NULL)
  svc$base_url
}

#' Internal helper: the sign-in service of the hosted tier
#' @noRd
.morie_llm_hosted_auth <- function() {
  v <- sub("/+$", "", .morie_llm_env("MORIE_HOSTED_AUTH_URL", ""))
  if (nzchar(v)) return(v)
  a <- .morie_llm_services()$auth_url %||% ""
  if (nzchar(a)) a else DEFAULT_HOSTED_AUTH_URL
}

#' Internal helper: the hosted model name
#' @noRd
.morie_llm_hosted_model <- function() {
  v <- .morie_llm_setting("hosted.model")  # MORIE_HOSTED_MODEL, else `rmorie config set hosted.model`
  if (!is.null(v)) return(v)
  m <- .morie_llm_services()$default_model %||% ""
  if (nzchar(m)) m else DEFAULT_HOSTED_MODEL
}

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
.morie_llm_write_credentials <- function(data) .morie_llm_write_private_json(.morie_llm_credentials_path(), data)

#' Internal helper: write a JSON object to a file only its owner can read
#' (credentials.json, llm.json)
#' @noRd
.morie_llm_write_private_json <- function(p, data) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(p, ".tmp")
  # created empty and made private BEFORE a key is written into it, whatever the umask
  file.create(tmp)
  Sys.chmod(tmp, mode = "0600", use_umask = FALSE)
  # an empty list serialises as [] (an array): the shared file is always a JSON object
  writeLines(if (length(data)) .morie_to_json(data, auto_unbox = TRUE, pretty = TRUE) else "{}", tmp)
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

#' Internal helper: does the user's own OpenAI-compatible endpoint answer?
#' @noRd
.morie_llm_probe_api <- function(timeout = 2) {
  base <- .morie_llm_api_base()
  if (is.null(base) || .morie_llm_no_net()) return(FALSE)
  st <- .morie_llm_http(paste0(.morie_llm_v1(base), "/models"), headers = .morie_llm_bearer(.morie_llm_api_key()),
                        timeout = timeout)$status
  st > 0L && st < 500L
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
  if (is.null(base) || is.null(key) || .morie_llm_no_net()) {
    .morie_llm_cache$hosted_cached <- FALSE
    return(FALSE)
  }
  res <- .morie_llm_http(paste0(base, "/v1/models"), headers = .morie_llm_bearer(key), timeout = timeout)
  out <- res$status > 0L && res$status < 400L
  if (out) {
    ids <- tryCatch(vapply(.morie_llm_http_json(res)$data, function(m) as.character(m$id %||% ""), ""),
                    error = function(e) character())
    ids <- ids[nzchar(ids)]
    .morie_llm_cache$hosted_models <- if (length(ids)) ids else NULL
  }
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
  if (!is.null(email)) email <- trimws(email)
  if (!is.null(email) && !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email))
    stop(sprintf("'%s' is not an email address", email), call. = FALSE)
  auth <- .morie_llm_hosted_auth()
  if (!is.null(email)) return(.morie_llm_login_email(auth, email, code, to_email))
  start <- .morie_llm_http(paste0(auth, "/device/code"), body = "{}")
  if (start$status != 200L) stop(sprintf("the sign-in service answered %d", start$status), call. = FALSE)
  info <- .morie_llm_http_json(start)
  message(sprintf("Sign in at %s and enter the code: %s", info$verification_uri, info$user_code))
  if (isTRUE(open_browser) && .morie_llm_browsable(info$verification_uri)) {
    try(utils::browseURL(info$verification_uri), silent = TRUE)
  }
  interval <- as.numeric(info$interval %||% 5)
  deadline <- Sys.time() + poll_max_seconds
  waited <- 0
  while (Sys.time() < deadline) {
    Sys.sleep(interval)
    waited <- waited + interval
    if (waited %% 30 < interval) {
      message(sprintf("still waiting for the sign-in to be approved (%ds elapsed; Ctrl-C stops)", as.integer(waited)))
    }
    resp <- .morie_llm_http(paste0(auth, "/device/token"),
                            body = .morie_to_json(list(device_code = info$device_code), auto_unbox = TRUE))
    st <- resp$status
    if (st == 200L) {
      body <- .morie_llm_http_json(resp)
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
      interval <- as.numeric(.morie_llm_http_json(resp)$interval %||% interval)
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
  if (!grepl("@", email, fixed = TRUE)) stop(sprintf("'%s' is not an email address", email), call. = FALSE)
  perform <- function(path, body) .morie_llm_http(paste0(auth, path), body = .morie_to_json(body, auto_unbox = TRUE))
  fail <- function(resp) {
    err <- .morie_llm_http_json(resp)$error
    stop(if (is.null(err)) sprintf("the sign-in service answered %d", resp$status) else as.character(err), call. = FALSE)
  }
  if (is.null(code)) {
    resp <- perform("/email/code", list(email = email))
    if (resp$status != 200L) fail(resp)
    message(sprintf("A 6-digit code was sent to %s (valid for 10 minutes).", email))
    # the launcher runs Rscript, where readline() returns "" at once: .cli_readline reads stdin there
    code <- .cli_readline("Enter the code: ")
    if (is.na(code) || !nzchar(code)) {
      stop(sprintf("no code entered; finish with `rmorie login --email %s --code CODE`", email), call. = FALSE)
    }
  }
  payload <- list(email = email, code = trimws(code))
  if (isTRUE(to_email)) payload$deliver <- "email"
  resp <- perform("/email/verify", payload)
  if (resp$status != 200L) fail(resp)
  body <- .morie_llm_http_json(resp)
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
            "\nKey:      ", if (nzchar(k)) "set" else "(not set)")
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
  if (had) .morie_llm_write_credentials(d)  # nothing attached: nothing to write
  message(if (had) "Endpoint detached." else "No endpoint was attached.")
  invisible(had)
}


# Does the gateway accept this key? (asked before a key is stored)
.morie_llm_probe_token <- function(token) {
  base <- .morie_llm_hosted_base()
  if (is.null(base) || !nzchar(base)) return(FALSE)
  st <- .morie_llm_http(paste0(sub("/+$", "", base), "/v1/models"), headers = .morie_llm_bearer(token), timeout = 20)$status
  st >= 200L && st < 300L
}

# The stored key reaches the gateway but the gateway refuses it (401/403), as opposed to no gateway.
.morie_llm_hosted_rejected <- function() {
  key <- .morie_llm_hosted_key()
  base <- .morie_llm_hosted_base()
  if (is.null(key) || is.null(base) || !nzchar(base)) return(FALSE)
  .morie_llm_http(paste0(sub("/+$", "", base), "/v1/models"), headers = .morie_llm_bearer(key), timeout = 20)$status %in%
    c(401L, 403L)
}
