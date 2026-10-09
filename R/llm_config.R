# SPDX-License-Identifier: AGPL-3.0-or-later
#
# llm_config.R -- saved language-model settings: which route `ask` takes,
# and the address, key and model of each route.
#
# Every setting has an environment variable too; a variable that is set wins
# over the saved value, so a one-off `OLLAMA_MODEL=x rmorie ask ...` still
# works. The file is $XDG_CONFIG_HOME/morie/llm.json (default
# ~/.config/morie/llm.json; private, 0600), next to the credentials file the
# hosted key lives in. It is the same file, with the same keys, that
# rmoriebricklayer (`rmbl config`) and the Python package read, and it is
# only ever written by morie_llm_config() or `rmorie config`: an explicit
# user action, never an example or a test.

.morie_llm_config_spec <- function() {
  data.frame(
    key = c("route", "own.url", "own.key", "own.model",
            "ollama.url", "ollama.model", "ollama.key",
            "hosted.url", "hosted.model", "hosted.key"),
    env = c("MORIE_LLM_ROUTE", "MORIE_LLM_BASE_URL", "MORIE_LLM_API_KEY", "MORIE_LLM_MODEL",
            "OLLAMA_HOST", "OLLAMA_MODEL", "OLLAMA_API_KEY",
            "MORIE_HOSTED_BASE_URL", "MORIE_HOSTED_MODEL", "MORIE_HOSTED_KEY"),
    secret = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
    help = c(
      "which route `ask` uses: auto (Ollama with a model, your keys, your own server, then hosted), own, ollama or hosted",
      "your own OpenAI-compatible server, e.g. http://localhost:1234/v1 (LM Studio) or https://api.example.org/v1",
      "API key for your own server (sent as a Bearer token)",
      "model name on your own server",
      "your Ollama server, e.g. http://localhost:11434 or 192.168.1.20:11434; off to skip Ollama",
      "Ollama model to use (default: the first one pulled)",
      "API key for an Ollama server that wants one",
      "hosted MORIE tier address (default: from the signed services document); off to skip it",
      "hosted model (default: the tier's default; `rmorie models` lists them)",
      "your MORIE key (stored by `rmorie login`; checked with the gateway before it is saved)"
    ),
    stringsAsFactors = FALSE
  )
}

.morie_llm_routes <- c("auto", "own", "ollama", "hosted")
.morie_llm_off <- function(v) tolower(v) %in% c("off", "none", "disabled")

#' Internal helper: the language-model settings file, next to credentials.json
#' @noRd
.morie_llm_config_path <- function() file.path(dirname(.morie_llm_credentials_path()), "llm.json")

#' Internal helper: the saved settings (a named list, empty when absent or unreadable)
#' @noRd
.morie_llm_read_config <- function() {
  p <- .morie_llm_config_path()
  if (!file.exists(p)) return(list())
  out <- tryCatch(.morie_from_json(paste(readLines(p, warn = FALSE), collapse = "\n"),
                                   simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.list(out)) out else list()
}

#' Internal helper: write the settings file (removed when nothing is left in it)
#' @noRd
.morie_llm_write_config <- function(data) {
  p <- .morie_llm_config_path()
  if (!length(data)) {
    if (file.exists(p)) unlink(p)
    return(invisible(p))
  }
  .morie_llm_write_private_json(p, data)
}

#' Internal helper: the value saved under a setting, or NULL
#' @noRd
.morie_llm_saved <- function(key) {
  if (identical(key, "hosted.key")) return(NULL)  # the key lives in credentials.json
  v <- .morie_llm_read_config()[[key]]
  if (is.character(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(v))) trimws(v) else NULL
}

#' Internal helper: a setting's value right now -- the first of its environment
#' variables that is set (the contract's name, then rmorie's older spellings in
#' \code{also}), else the saved value, else NULL
#' @noRd
.morie_llm_setting <- function(key, also = character()) {
  spec <- .morie_llm_config_spec()
  for (e in c(spec$env[match(key, spec$key)], also)) {
    v <- trimws(Sys.getenv(e, unset = ""))
    if (nzchar(v)) return(v)
  }
  .morie_llm_saved(key)
}

#' Internal helper: the route `ask` takes -- an argument, else MORIE_LLM_ROUTE,
#' else the saved route, else "auto" (an unknown value counts as auto)
#' @noRd
.morie_llm_route <- function(route = NULL) {
  r <- tolower(trimws(route %||% .morie_llm_setting("route") %||% "auto"))
  if (r %in% .morie_llm_routes) r else "auto"
}

#' Internal helper: the provider name of a route (the own route is the "api" provider)
#' @noRd
.morie_llm_route_provider <- function(route) if (identical(route, "own")) "api" else route

# Where a setting comes from right now: "environment", "saved", or "default".
.morie_llm_config_source <- function(key, env) {
  envs <- c(env, switch(key, own.url = "LLM_API_BASE_URL", own.key = "LLM_API_KEY",
                        own.model = "MORIE_API_MODEL", ollama.url = "OLLAMA_BASE_URL", NULL))
  if (any(nzchar(trimws(Sys.getenv(envs, unset = ""))))) return("environment")
  if (identical(key, "hosted.key")) {
    return(if (is.null(.morie_llm_read_credentials()$hosted_key)) "default" else "saved")
  }
  if (!is.null(.morie_llm_saved(key))) return("saved")
  # an endpoint attached earlier with `rmorie provider set` (credentials.json)
  legacy <- switch(key, own.url = "api_base_url", own.key = "api_key", own.model = "api_model", NULL)
  if (!is.null(legacy) && nzchar(.morie_llm_stored_provider(legacy))) return("saved")
  "default"
}

# How a key is displayed: only whether one is set. No character of it is ever printed.
.morie_llm_mask <- function(v) {
  if (is.null(v) || !nzchar(v)) "(not set)" else "set"
}

#' Internal helper: the value a setting has right now (the default spelled out), for display
#' @noRd
.morie_llm_config_effective <- function(key) {
  switch(key,
    route = .morie_llm_route(),
    own.url = .morie_llm_api_base() %||% "(not set)",
    own.key = .morie_llm_mask(.morie_llm_api_key()),
    own.model = .morie_llm_api_model(),
    ollama.url = if (.morie_llm_ollama_off()) "off" else .morie_llm_ollama_base(),
    ollama.model = .morie_llm_setting("ollama.model") %||% "(the first one pulled)",
    ollama.key = .morie_llm_mask(.morie_llm_ollama_key()),
    hosted.url = tryCatch(.morie_llm_hosted_base() %||% "off", error = function(e) conditionMessage(e)),
    hosted.model = .morie_llm_hosted_model(),
    hosted.key = .morie_llm_mask(.morie_llm_hosted_key())
  )
}

.morie_llm_config_check <- function(key, value) {
  url_ok <- function(v) grepl("^https?://[^/[:space:]]+", v)
  switch(key,
    route = if (!value %in% .morie_llm_routes) {
      stop("route must be one of: auto, own, ollama, hosted", call. = FALSE)
    },
    own.url = if (!.morie_llm_off(value) && !url_ok(value)) {
      stop("own.url must be an http(s):// address, e.g. http://localhost:1234/v1", call. = FALSE)
    },
    hosted.url = if (!.morie_llm_off(value) && !grepl("^https://[^/[:space:]]+", value)) {
      # the bearer key is sent there: https only
      stop("hosted.url must be an https:// address (or off)", call. = FALSE)
    },
    ollama.url = if (grepl("[[:space:]]", value)) {
      stop("ollama.url must be an address such as http://localhost:11434", call. = FALSE)
    },
    NULL
  )
  invisible(TRUE)
}

.morie_llm_reset_probes <- function() {
  for (k in c("ollama_cached", "hosted_cached", "hosted_models")) .morie_llm_cache[[k]] <- NULL
}

#' Show or save the language-model settings
#'
#' Every setting that decides how \code{\link{morie_llm_ask}} (and
#' \code{rmorie ask}) reaches a model, in one place: which route to use, and
#' the address, key and model of your own OpenAI-compatible server, of a
#' local or LAN Ollama server, and of the hosted MORIE tier. Settings are
#' saved in \code{$XDG_CONFIG_HOME/morie/llm.json} (default
#' \code{~/.config/morie/}), a private file this function writes only when you
#' pass a setting; the hosted key goes to the shared credentials file through
#' \code{\link{morie_llm_login}}, after the gateway has accepted it. An
#' environment variable that is set (the \code{env} column) wins over a saved
#' value. The same file and keys are read by rmoriebricklayer and the Python
#' package.
#'
#' With \code{route = "auto"} (the default) \code{morie_llm_ask()} tries a
#' local Ollama server that has a model (a server with nothing pulled is
#' skipped), then your Gemini key, your own server, your OpenAI key, and the
#' hosted tier last. \code{"own"}, \code{"ollama"} or \code{"hosted"} use that
#' route and no other.
#'
#' The same is available from the shell: \code{rmorie config} lists the
#' settings, \code{rmorie config set KEY VALUE} and
#' \code{rmorie config unset KEY} change one, \code{rmorie config setup} walks
#' through all of them, and \code{rmorie doctor} says which route \code{ask}
#' will take.
#'
#' @param ... Settings to save, as \code{key = value}: \code{route}
#'   (\code{"auto"}, \code{"own"}, \code{"ollama"} or \code{"hosted"}),
#'   \code{own.url}, \code{own.key}, \code{own.model}, \code{ollama.url},
#'   \code{ollama.model}, \code{ollama.key}, \code{hosted.url},
#'   \code{hosted.model}, \code{hosted.key}. \code{NULL} or \code{""} removes
#'   a saved setting. With no arguments nothing is written.
#' @return A data frame with one row per setting: \code{key}, \code{value}
#'   (a key only as \code{"set"}), \code{source} (\code{"environment"},
#'   \code{"saved"} or \code{"default"}), \code{env} (the variable that
#'   overrides it) and \code{help}; visibly when called with no settings.
#' @examples
#' # reading the settings writes nothing
#' morie_llm_config()[, c("key", "value", "source")]
#' \dontrun{
#' # always use the hosted tier, with one model
#' morie_llm_config(route = "hosted", hosted.model = "gpt-oss-120b:cf")
#' # an Ollama server on another machine
#' morie_llm_config(ollama.url = "http://192.168.1.20:11434", ollama.model = "qwen3:8b")
#' # your own OpenAI-compatible server
#' morie_llm_config(own.url = "http://localhost:1234/v1", own.model = "my-model")
#' # back to the automatic order
#' morie_llm_config(route = NULL)
#' }
#' @export
morie_llm_config <- function(...) {
  args <- list(...)
  spec <- .morie_llm_config_spec()
  if (length(args)) {
    nm <- names(args)
    if (is.null(nm) || any(!nzchar(nm))) stop("settings are passed as key = value", call. = FALSE)
    bad <- setdiff(nm, spec$key)
    if (length(bad)) {
      stop(sprintf("unknown setting '%s' (one of: %s)", bad[[1L]], paste(spec$key, collapse = ", ")),
           call. = FALSE)
    }
    data <- .morie_llm_read_config()
    for (k in nm) {
      v <- args[[k]]
      v <- if (is.null(v) || (length(v) == 1L && is.na(v))) "" else trimws(as.character(v))
      if (length(v) != 1L) stop(sprintf("%s takes one value", k), call. = FALSE)
      if (identical(k, "hosted.key")) {
        if (nzchar(v)) {
          # the same rule as `rmorie login --token`: a key the gateway refuses is never stored
          if (!isTRUE(.morie_llm_probe_token(v))) {
            stop("the gateway did not accept that key; nothing stored (rmorie login mints one)", call. = FALSE)
          }
          morie_llm_login(token = v)
        } else {
          morie_llm_logout()
        }
        next
      }
      if (nzchar(v)) {
        if (identical(k, "route")) v <- tolower(v)
        .morie_llm_config_check(k, v)
        data[[k]] <- v
      } else {
        data[[k]] <- NULL
      }
    }
    .morie_llm_write_config(data)
    .morie_llm_reset_probes()
    env_set <- spec$env[match(setdiff(nm, "hosted.key"), spec$key)]
    env_set <- env_set[vapply(env_set, function(e) nzchar(Sys.getenv(e, unset = "")), TRUE)]
    if (length(env_set)) {
      warning(sprintf("%s is set in the environment and still wins over the saved value",
                      paste(env_set, collapse = ", ")), call. = FALSE)
    }
  }
  tab <- data.frame(
    key = spec$key,
    value = vapply(spec$key, .morie_llm_config_effective, "", USE.NAMES = FALSE),
    source = mapply(.morie_llm_config_source, spec$key, spec$env, USE.NAMES = FALSE),
    env = spec$env,
    help = spec$help,
    stringsAsFactors = FALSE
  )
  if (length(args)) invisible(tab) else tab
}

# `rmorie config ...`; returns the exit status.
.cli_config <- function(rest, out) {
  spec <- .morie_llm_config_spec()
  sub_verb <- if (length(rest)) rest[[1L]] else "show"
  args <- rest[-1L]
  usage <- function(msg) {
    out(paste0("rmorie config: ", msg, "\n"))
    2L
  }
  key_ok <- function() {
    length(args) && args[[1L]] %in% spec$key
  }
  key_error <- function() {
    if (!length(args)) {
      return(usage(sprintf("usage: rmorie config %s KEY%s   (keys: rmorie config help)", sub_verb,
                           if (identical(sub_verb, "set")) " VALUE" else "")))
    }
    usage(sprintf("unknown setting '%s' (one of: %s)", args[[1L]], paste(spec$key, collapse = ", ")))
  }
  show <- function(tab) {
    for (i in seq_len(nrow(tab))) {
      out(sprintf("  %-13s %-40s %s\n", tab$key[i], tab$value[i],
                  if (identical(tab$source[i], "default")) "" else paste0("(", tab$source[i], ")")))
    }
  }
  switch(sub_verb,
    show = ,
    list = {
      show(morie_llm_config())
      out(paste0(
        sprintf("\nSaved in %s. ", .morie_llm_config_path()),
        "An environment variable that is set wins over a saved value.\n",
        "Change one:                rmorie config set route hosted\n",
        "Walk through all of them:  rmorie config setup\n",
        "What each one means:       rmorie config help\n",
        "Which route ask will take: rmorie doctor\n"
      ))
    },
    help = {
      for (i in seq_len(nrow(spec))) {
        out(sprintf("  %-13s %s\n  %-13s (environment variable %s)\n", spec$key[i], spec$help[i], "", spec$env[i]))
      }
      out(paste0(
        "\nSettings live in ", .morie_llm_config_path(), " (shared with rmoriebricklayer and Python morie).\n",
        "\nExamples:\n",
        "  rmorie config set route hosted                         always use the hosted tier\n",
        "  rmorie config set hosted.model gpt-oss-120b:cf         its model\n",
        "  rmorie config set ollama.url http://192.168.1.20:11434  Ollama on another machine\n",
        "  rmorie config set ollama.model qwen3:8b\n",
        "  rmorie config set ollama.url off                       never try Ollama\n",
        "  rmorie config set own.url http://localhost:1234/v1     LM Studio, vLLM, llama.cpp ...\n",
        "  rmorie config set own.key                              (asks for the key, so it stays out of history)\n",
        "  rmorie config unset route                              back to the automatic order\n",
        "  rmorie config setup                                    answer a few questions instead\n"
      ))
    },
    get = {
      if (!key_ok()) return(key_error())
      out(paste0(.morie_llm_config_effective(args[[1L]]), "\n"))
    },
    set = {
      if (!key_ok()) return(key_error())
      k <- args[[1L]]
      v <- if (length(args) >= 2L) paste(args[-1L], collapse = " ") else ""
      if (!nzchar(v) && spec$secret[match(k, spec$key)]) {
        v <- .cli_readline(sprintf("%s: ", k))
        if (is.na(v)) v <- ""
      }
      if (!nzchar(v)) return(usage(sprintf("usage: rmorie config set %s VALUE", k)))
      withCallingHandlers(do.call(morie_llm_config, stats::setNames(list(v), k)),
        warning = function(w) {
          out(paste0("rmorie config: ", conditionMessage(w), "\n"))
          invokeRestart("muffleWarning")
        })
      out(sprintf("%s = %s\n", k, .morie_llm_config_effective(k)))
    },
    unset = {
      if (!key_ok()) return(key_error())
      k <- args[[1L]]
      do.call(morie_llm_config, stats::setNames(list(NULL), k))
      out(sprintf("%s = %s\n", k, .morie_llm_config_effective(k)))
    },
    path = out(paste0(.morie_llm_config_path(), "\n")),
    setup = return(.cli_config_setup(out)),
    return(usage("usage: rmorie config [show | help | get KEY | set KEY VALUE | unset KEY | setup | path]"))
  )
  0L
}

# `rmorie config setup`: one question per route, Enter keeps what is there.
.cli_config_setup <- function(out) {
  if (.cli_stdin_closed()) {
    out("rmorie config setup asks questions and needs a terminal; use `rmorie config set KEY VALUE` instead (keys: rmorie config help)\n")
    return(2L)
  }
  ask <- function(q, current = "") {
    a <- .cli_readline(sprintf("%s%s: ", q, if (nzchar(current)) sprintf(" [%s]", current) else ""))
    if (is.na(a) || !nzchar(a)) current else a
  }
  set <- function(...) morie_llm_config(...)
  out("Language-model setup. Press Enter to keep the value in [brackets].\n\n")
  route <- tolower(ask("Which should `ask` use: auto, hosted, ollama or own", .morie_llm_route()))
  if (!route %in% .morie_llm_routes) route <- "auto"
  set(route = if (identical(route, "auto")) NULL else route)
  if (route %in% c("auto", "hosted")) {
    if (is.null(.morie_llm_hosted_key())) {
      out("\nThe hosted tier needs a MORIE key: `rmorie login` (GitHub, or --email ADDRESS for a code),\n")
      out(sprintf("or paste one issued at %s here.\n", .morie_llm_services()$request_access %||% ACCESS_REQUEST_URL))
      k <- ask("MORIE key (Enter to skip)")
      if (nzchar(k)) {
        r <- tryCatch({ set(hosted.key = k); NULL }, error = function(e) conditionMessage(e))
        if (!is.null(r)) out(paste0(r, "\n"))
      }
    }
    if (!is.null(.morie_llm_hosted_key())) {
      have <- tryCatch(as.character(morie_llm_hosted_models()), error = function(e) character())
      if (length(have)) out(sprintf("Models: %s\n", paste(have, collapse = ", ")))
      m <- ask("Hosted model", .morie_llm_hosted_model())
      set(hosted.model = if (identical(m, .morie_llm_hosted_model()) && is.null(.morie_llm_saved("hosted.model"))) NULL else m)
    }
  }
  if (route %in% c("auto", "ollama")) {
    u <- ask("\nOllama address (off to skip Ollama)", .morie_llm_config_effective("ollama.url"))
    set(ollama.url = if (identical(u, DEFAULT_OLLAMA_BASE_URL)) NULL else u)
    set(ollama.model = ask("Ollama model (Enter for the first pulled)", .morie_llm_setting("ollama.model") %||% ""))
  }
  if (route %in% c("auto", "own")) {
    u <- ask("\nYour own OpenAI-compatible server (Enter to skip)", .morie_llm_setting("own.url") %||% "")
    set(own.url = u)
    if (nzchar(u)) {
      set(own.model = ask("Model on that server", .morie_llm_setting("own.model") %||% ""))
      k <- ask("API key for it (Enter for none or to keep)")
      if (nzchar(k)) set(own.key = k)
    }
  }
  out(sprintf("\nSaved in %s.\n\n", .morie_llm_config_path()))
  .cli_doctor(out)
  0L
}

#' Internal helper: the route `ask` would take now, for doctor:
#' list(provider, label, base, model), or NULL when nothing answers
#' @noRd
.morie_llm_route_plan <- function(route = NULL) {
  prov <- if (is.null(route)) morie_llm_detect_provider() else morie_llm_detect_provider(route = route)
  switch(prov,
    ollama = list(provider = prov, label = "local Ollama", base = .morie_llm_ollama_base(),
                  model = .morie_llm_ollama_default_model()),
    gemini = list(provider = prov, label = "Gemini (your key)", base = GEMINI_BASE_URL,
                  model = .morie_llm_gemini_model()),
    api = list(provider = prov, label = "your own endpoint", base = .morie_llm_api_base(),
               model = .morie_llm_api_model()),
    openai = list(provider = prov, label = "OpenAI (your key)", base = OPENAI_BASE_URL,
                  model = DEFAULT_OPENAI_MODEL),
    hosted = list(provider = prov, label = "hosted MORIE tier", base = .morie_llm_hosted_base(),
                  model = .morie_llm_hosted_model_available()),
    NULL)
}

# `rmorie doctor`: each route, then the one `ask` would take now.
.cli_doctor <- function(out) {
  ollama_up <- morie_llm_probe_ollama()
  ollama_models <- attr(.morie_llm_cache$ollama_cached, "models")
  hosted_key <- .morie_llm_hosted_key()
  rows <- list(
    c("Ollama (local)",
      if (.morie_llm_ollama_off()) "off (ollama.url = off)"
      else if (!ollama_up) "not reachable"
      else if (!is.null(ollama_models) && !length(ollama_models) && is.null(.morie_llm_setting("ollama.model")))
        "reachable, no model pulled (skipped)"
      else "reachable",
      .morie_llm_ollama_base()),
    c("Hosted LLM", if (is.null(hosted_key)) "not logged in -- rmorie login (GitHub) or rmorie login --email you@example.com"
                    else if (morie_llm_probe_hosted()) "logged in, gateway answering"
                    else if (.morie_llm_hosted_rejected()) "key rejected by the gateway -- rmorie login again"
                    else "logged in, gateway not reachable",
      if (!is.null(hosted_key) && morie_llm_probe_hosted()) {
        hm <- morie_llm_hosted_models()
        sprintf("%s  models: %s (default %s)", .morie_llm_hosted_base(),
                paste(hm, collapse = ", "), attr(hm, "default") %||% "")
      } else .morie_llm_hosted_base() %||% "disabled"),
    c("Gemini key", if (is.null(.morie_llm_gemini_key())) "absent" else "set", ""),
    c("Your own endpoint", if (is.null(.morie_llm_api_base())) "absent" else if (.morie_llm_api_usable()) "set" else "set, no key",
      .morie_llm_api_base() %||% ""),
    c("OpenAI key", if (is.null(.morie_llm_openai_key())) "absent" else "set", ""))
  for (r in rows) out(sprintf("  %-24s %-32s %s\n", r[[1L]], r[[2L]], r[[3L]]))
  route <- .morie_llm_route()
  plan <- tryCatch(.morie_llm_route_plan(), error = function(e) e)
  out(sprintf("  active provider: %s\n", if (is.list(plan) && !inherits(plan, "error")) plan$provider else "local"))
  if (inherits(plan, "error")) {
    out(sprintf("\n  ask would fail: %s\n", conditionMessage(plan)))
  } else if (is.null(plan)) {
    out(sprintf("\n  ask has no route%s: `rmorie login` for the hosted tier, or `rmorie config setup`\n",
                if (identical(route, "auto")) " yet" else sprintf(" (route = %s, and it is not set up)", route)))
  } else {
    m <- plan$model
    out(sprintf("\n  ask uses: %s (%s), model %s%s\n", plan$label, plan$base %||% "",
                if (is.null(m) || is.na(m) || !nzchar(m)) "(none)" else m,
                if (identical(route, "auto")) "" else sprintf("  (route = %s)", route)))
  }
  out("  change it: rmorie config set route hosted|ollama|own|auto  (all settings: rmorie config; help: rmorie help llm)\n")
  0L
}
