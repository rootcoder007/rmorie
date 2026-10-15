# SPDX-License-Identifier: AGPL-3.0-or-later
#
# llm.R -- language models in rmorie: the one you bring, through rmoriebricklayer.
#
# rmorie runs no model and hosts none. morie_llm_ask() asks the route
# rmoriebricklayer picks: an endpoint of your own (MORIE_LLM_BASE_URL), then
# each provider whose key is set (OpenAI, Anthropic, Gemini, OpenRouter, Groq,
# Mistral, DeepSeek, xAI, Hugging Face, OpenCode and more), then Ollama (on this
# machine, another one, or ollama.com with your Ollama key). The settings are
# rmoriebricklayer's, in the file it shares with rmorie (llm_config.R). The
# model gets the help of rmorie, rmoriebricklayer and rmoriedata with tools to
# read it. The Ollama helpers below are rmorie's own, for the modules that
# check a local server; the key in the shared credentials file is the data key
# for data.rmorie.com.

.morie_llm_cache <- new.env(parent = emptyenv())

DEFAULT_OLLAMA_BASE_URL <- "http://localhost:11434"

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
#'   morie_llm_probe_ollama()  # the answer is cached for the session
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
# host (an Ollama server, a company gateway) gets /v1. `provider set` and
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

#' Internal helper: kept for the hint sites; no language-model path needs httr2
#' @noRd
.morie_httr2_note <- function() ""

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

#' Internal helper: the data key for data.rmorie.com -- MORIE_HOSTED_KEY, else the stored credential
#' @noRd
.morie_llm_hosted_key <- function() {
  v <- .morie_llm_env("MORIE_HOSTED_KEY")
  if (nzchar(v)) return(v)
  k <- .morie_llm_read_credentials()$hosted_key
  if (is.character(k) && length(k) == 1L && nzchar(trimws(k))) trimws(k) else NULL
}

# ---- The route: rmoriebricklayer's -----------------------------------------

#' Internal helper: the MORIE packages installed here whose help the model gets
#' @noRd
.morie_llm_doc_packages <- function() {
  pk <- c("rmorie", "rmoriebricklayer", "rmoriedata")
  pk[vapply(pk, function(p) nzchar(system.file(package = p)), logical(1))]
}

#' Internal helper: why "hosted" is no route any more
#' @noRd
.morie_llm_hosted_retired <- function() {
  paste("the hosted MORIE language-model tier is retired: bring your own key (a provider's",
        "variable such as ANTHROPIC_API_KEY or OPENAI_API_KEY), run a model in Ollama, or",
        "set your own server; `rmorie config setup` walks through it")
}

#' Internal helper: a route as rmoriebricklayer names it. rmorie's older names map
#' onto it ("api" is your own endpoint; "local" and "auto" are the automatic order).
#' @noRd
.morie_llm_bl_route <- function(route = NULL, provider = NULL) {
  r <- route %||% provider
  if (is.null(r)) return(NULL)
  r <- tolower(trimws(as.character(r)[[1L]]))
  if (r %in% c("api", "own")) return("own")
  if (r %in% c("auto", "local", "")) return(NULL)
  if (identical(r, "hosted")) stop(.morie_llm_hosted_retired(), call. = FALSE)
  r
}

#' The language-model route that would answer
#'
#' Which route \code{\link{morie_llm_ask}} would take now, without sending
#' anything. The routes are rmoriebricklayer's: an endpoint of your own, then
#' each provider whose key is set (OpenAI, Anthropic, Google Gemini, OpenRouter,
#' Groq, Mistral and more: \code{rmoriebricklayer::bricklayer_llm_providers()}),
#' then Ollama (on this machine, another one, or ollama.com with your Ollama
#' key). \code{\link{morie_llm_config}} or \code{rmorie config setup} sets them up.
#' @param route \code{NULL} (the saved setting, else the automatic order),
#'   \code{"auto"}, \code{"own"}, \code{"ollama"} or a provider id such as
#'   \code{"anthropic"}.
#' @return The route id (\code{"own"}, \code{"ollama"}, a provider id), or
#'   \code{"local"} when no route is set up.
#' @examples
#' old <- Sys.getenv(c("OLLAMA_HOST", "XDG_CONFIG_HOME"), unset = NA)
#' Sys.setenv(OLLAMA_HOST = "off", XDG_CONFIG_HOME = tempfile())
#' morie_llm_detect_provider()
#' for (v in names(old)) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
#' @export
morie_llm_detect_provider <- function(route = NULL) {
  info <- tryCatch(rmoriebricklayer::bricklayer_llm_route_info(.morie_llm_bl_route(route)),
                   error = function(e) NULL)
  if (is.null(info)) "local" else info$route
}

#' Return TRUE when a language model is set up
#' @return Logical scalar.
#' @examples
#' old <- Sys.getenv(c("OLLAMA_HOST", "XDG_CONFIG_HOME"), unset = NA)
#' Sys.setenv(OLLAMA_HOST = "off", XDG_CONFIG_HOME = tempfile())
#' morie_llm_agent_available()
#' for (v in names(old)) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
#' @export
morie_llm_agent_available <- function() {
  morie_llm_detect_provider() != "local"
}

#' Internal helper: the text returned when no model answers
#' @noRd
.morie_llm_local_fallback <- function(prompt, tried = FALSE, why = NULL) {
  # the attribute lets a script tell this text from an answer: isTRUE(attr(x, "fallback"))
  head <- if (isTRUE(tried)) {
    sprintf("No language model answered this time%s.\n\n", if (length(why)) paste0(": ", why) else "")
  } else {
    "MORIE is running without a language model (none is set up on this machine).\n\n"
  }
  structure(paste0(
    head,
    "The analyses do not need a model: morie_run_pipeline(), morie_run_morie_module() and every ",
    "morie_* estimator work as they are. To get answers from a model, bring your own:\n",
    "  - a provider's key: ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, OPENROUTER_API_KEY, ",
    "GROQ_API_KEY, ... (rmoriebricklayer::bricklayer_llm_providers() lists them)\n",
    "  - Ollama: on this machine (https://ollama.com), another one (rmorie config set ollama.url ",
    "ADDRESS), or ollama.com itself (ollama.url https://ollama.com and your ollama.key)\n",
    "  - your own OpenAI-compatible server: rmorie config set own.url URL (LM Studio, vLLM, llama.cpp)\n",
    "`rmorie config setup` asks for each (R: morie_llm_config()); `rmorie doctor` says which ",
    "route answers."
  ), fallback = TRUE)
}

#' Send a prompt to a language model
#'
#' Asks the model you set up (see \code{\link{morie_llm_detect_provider}}) through
#' rmoriebricklayer. With \code{docs = TRUE} the model is given the help of
#' rmorie, rmoriebricklayer and rmoriedata (rmorie's own thousands of pages are
#' searched, the functions nearest the question listed) and tools to read it,
#' so it can name the right function and its arguments. When no model is set
#' up, or the automatic route fails, the answer is a short text saying how to
#' set one up (with \code{attr(, "fallback")} TRUE); a route you named is an
#' error instead.
#'
#' @param prompt User question or instruction.
#' @param context Optional named list (or text) given with the question.
#' @param model Optional model name; the route's model when NULL.
#' @param provider Older name for \code{route} (\code{"api"} is \code{"own"}).
#' @param system_prompt Optional instructions added for the model.
#' @param timeout Seconds to wait for each reply. Default 120.
#' @param route \code{NULL} (the saved setting, else the automatic order),
#'   \code{"auto"}, \code{"own"}, \code{"ollama"} or a provider id such as
#'   \code{"anthropic"}: a route other than auto is the only one asked.
#' @param docs \code{TRUE} (default) to give the model the packages' help.
#' @return Character scalar: the answer, or the fallback text.
#' @examples
#' old <- Sys.getenv(c("OLLAMA_HOST", "XDG_CONFIG_HOME"), unset = NA)
#' Sys.setenv(OLLAMA_HOST = "off", XDG_CONFIG_HOME = tempfile())
#' out <- morie_llm_ask("In one word, what is 2 + 2?")
#' isTRUE(attr(out, "fallback"))
#' for (v in names(old)) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
#' \dontrun{
#' Sys.setenv(ANTHROPIC_API_KEY = "...")
#' morie_llm_ask("Which rmorie function fits a staggered difference-in-differences design?")
#' }
#' @export
morie_llm_ask <- function(prompt, context = NULL, model = NULL,
                          provider = NULL, system_prompt = NULL,
                          timeout = 120, route = NULL, docs = TRUE) {
  if (!is.character(prompt) || length(prompt) != 1L || is.na(prompt) || !nzchar(trimws(prompt))) {
    stop("`prompt` must be a single non-empty string", call. = FALSE)
  }
  rte <- .morie_llm_bl_route(route, provider)
  forced <- !is.null(rte)
  if (!is.null(context)) {
    ctx <- if (is.list(context)) {
      paste(vapply(names(context), function(k) paste0(k, ": ", paste(as.character(context[[k]]), collapse = " ")), ""),
            collapse = "\n")
    } else {
      paste(as.character(context), collapse = "\n")
    }
    prompt <- paste0(prompt, "\n\nContext:\n", ctx)
  }
  extra <- c(.morie_llm_system_prompt(""), system_prompt)
  ask <- function() {
    rmoriebricklayer::bricklayer_llm_ask(prompt, model = model, timeout = timeout,
                                         system_prompt = paste(extra, collapse = "\n"), route = rte,
                                         docs = isTRUE(docs), packages = .morie_llm_doc_packages())
  }
  if (forced) return(ask())
  tryCatch(ask(), error = function(e) {
    msg <- conditionMessage(e)
    if (startsWith(msg, "No language-model route")) .morie_llm_local_fallback(prompt)
    else .morie_llm_local_fallback(prompt, tried = TRUE, why = msg)
  })
}

#' Ask a language model with a whole conversation
#'
#' Sends \code{messages} (each \code{list(role = , content = )}, role
#' \code{"system"}, \code{"user"} or \code{"assistant"}) as it is, through
#' \code{rmoriebricklayer::bricklayer_llm_chat()}, for chat loops. When no model
#' answers on the automatic route the result is the fallback text of
#' \code{\link{morie_llm_ask}}.
#'
#' @param messages List of role/content lists.
#' @param providers Optional route (a character vector: the first that is not
#'   \code{"local"} is used; \code{"api"} is \code{"own"}).
#' @param model Optional model name.
#' @param timeout Seconds to wait for the reply.
#' @return Character scalar response text.
#' @examples
#' old <- Sys.getenv(c("OLLAMA_HOST", "XDG_CONFIG_HOME"), unset = NA)
#' Sys.setenv(OLLAMA_HOST = "off", XDG_CONFIG_HOME = tempfile())
#' out <- morie_llm_ask_multi(list(list(role = "user", content = "hello")))
#' isTRUE(attr(out, "fallback"))
#' for (v in names(old)) if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
#' @export
morie_llm_ask_multi <- function(messages, providers = NULL, model = NULL, timeout = 120) {
  stopifnot(is.list(messages))
  pick <- setdiff(tolower(as.character(providers %||% character())), "local")
  rte <- if (length(pick)) .morie_llm_bl_route(pick[[1L]]) else NULL
  last <- Filter(function(m) identical(m$role %||% "", "user"), messages)
  q <- if (length(last)) last[[length(last)]]$content %||% "" else ""
  tryCatch(rmoriebricklayer::bricklayer_llm_chat(messages, model = model, route = rte, timeout = timeout),
           error = function(e) {
             msg <- conditionMessage(e)
             if (!is.null(rte) && !startsWith(msg, "No language-model route")) stop(e)
             if (startsWith(msg, "No language-model route")) .morie_llm_local_fallback(q)
             else .morie_llm_local_fallback(q, tried = TRUE, why = msg)
           })
}

# ---- The data key ------------------------------------------------------------

#' Sign in for the curated tables at data.rmorie.com
#'
#' Stores your MORIE data key, the key that opens the curated tables at
#' data.rmorie.com (\code{\link{morie_load_hosted_dataset}}). It is never a language
#' model: those you bring yourself (\code{\link{morie_llm_config}}). With
#' \code{token} the key you already hold is checked with the data service and
#' stored; with \code{email} a one-time code is sent there; otherwise the GitHub
#' device flow runs. The key is stored once, privately, for rmorie,
#' rmoriebricklayer, rmoriedata and morie. This is
#' \code{rmoriebricklayer::bricklayer_llm_login()}.
#'
#' @param open_browser Open the GitHub page in a browser.
#' @param poll_max_seconds How long to wait for the GitHub approval.
#' @param email An email address to send a sign-in code to.
#' @param code The code that was emailed (asked for when not given).
#' @param token A key you already hold.
#' @return The key, invisibly.
#' @examples
#' \dontrun{
#' morie_llm_login()                              # GitHub
#' morie_llm_login(email = "you@example.org")     # an emailed code
#' }
#' @export
morie_llm_login <- function(open_browser = interactive(), poll_max_seconds = 600,
                            email = NULL, code = NULL, token = NULL) {
  rmoriebricklayer::bricklayer_llm_login(token = token, email = email, code = code,
                                         open_browser = open_browser, poll_max_seconds = poll_max_seconds)
}

#' Forget the stored data key
#' @return TRUE (invisibly) when a key was removed.
#' @examples
#' old <- Sys.getenv("XDG_CONFIG_HOME", unset = NA)
#' Sys.setenv(XDG_CONFIG_HOME = tempfile())
#' morie_llm_logout()
#' if (is.na(old)) Sys.unsetenv("XDG_CONFIG_HOME") else Sys.setenv(XDG_CONFIG_HOME = old)
#' @export
morie_llm_logout <- function() rmoriebricklayer::bricklayer_llm_logout()

#' Internal helper: one line on how to get a data key
#' @noRd
.morie_llm_access_hint <- function() {
  paste("sign in with `rmorie login` (GitHub, or --email ADDRESS for a code), or ask for a key at",
        "https://rmorie.com/access; store one you hold with `rmorie login --token` (R: morie_llm_login(token = ))")
}

# ---- Your own endpoint, the older spelling --------------------------------------

#' Attach your own model endpoint
#'
#' Saves an OpenAI-compatible endpoint (chat completions at
#' \code{base_url/chat/completions}) as the \code{own} route: the same as
#' \code{morie_llm_config(own.url = , own.key = , own.model = )}.
#' @param base_url The endpoint, e.g. \code{"http://localhost:1234/v1"}.
#' @param key The API key for it (\code{NULL} for a server that needs none).
#' @param model Optional model name to ask by default.
#' @return \code{morie_llm_provider_set()} and \code{morie_llm_provider_unset()}:
#'   the settings table, invisibly; \code{morie_llm_provider_show()}: the
#'   \code{own.*} rows of it.
#' @examples
#' old <- Sys.getenv("XDG_CONFIG_HOME", unset = NA)
#' Sys.setenv(XDG_CONFIG_HOME = tempfile())
#' morie_llm_provider_set("http://localhost:1234/v1", key = NULL, model = "my-model")
#' morie_llm_provider_show()
#' morie_llm_provider_unset()
#' if (is.na(old)) Sys.unsetenv("XDG_CONFIG_HOME") else Sys.setenv(XDG_CONFIG_HOME = old)
#' @export
morie_llm_provider_set <- function(base_url, key = NULL, model = NULL) {
  invisible(morie_llm_config(own.url = base_url, own.key = key, own.model = model))
}

#' @rdname morie_llm_provider_set
#' @export
morie_llm_provider_show <- function() {
  tab <- morie_llm_config()
  tab[startsWith(tab$key, "own."), , drop = FALSE]
}

#' @rdname morie_llm_provider_set
#' @export
morie_llm_provider_unset <- function() {
  invisible(morie_llm_config(own.url = NULL, own.key = NULL, own.model = NULL))
}
