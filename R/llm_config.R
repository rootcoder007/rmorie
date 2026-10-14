# SPDX-License-Identifier: AGPL-3.0-or-later
#
# llm_config.R -- the language-model settings, which rmorie shares with
# rmoriebricklayer: one file, $XDG_CONFIG_HOME/morie/llm.json (default
# ~/.config/morie/llm.json, private, 0600), written only by morie_llm_config(),
# `rmorie config` or `rmbl config`. rmoriebricklayer owns the settings (a
# route, your own server, Ollama, and a key and model per provider) and checks
# every value; rmorie hands each change and the config and doctor verbs to it,
# and reads values here only for its own Ollama helpers.

.morie_llm_off <- function(v) tolower(v) %in% c("off", "none", "disabled")

# The environment variable of each setting rmorie reads itself.
.morie_llm_setting_env <- c(route = "MORIE_LLM_ROUTE", own.url = "MORIE_LLM_BASE_URL",
                            own.key = "MORIE_LLM_API_KEY", own.model = "MORIE_LLM_MODEL",
                            ollama.url = "OLLAMA_HOST", ollama.model = "OLLAMA_MODEL",
                            ollama.key = "OLLAMA_API_KEY")

#' Internal helper: the language-model settings file, next to credentials.json
#' @noRd
.morie_llm_config_path <- function() {
  # built as the credentials path is: dirname() would rewrite the separators on Windows
  base <- trimws(Sys.getenv("XDG_CONFIG_HOME", unset = ""))
  if (!nzchar(base)) base <- file.path(path.expand("~"), ".config")
  file.path(base, "morie", "llm.json")
}

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

#' Internal helper: a setting's value right now -- the first of its environment
#' variables that is set (then rmorie's older spellings in \code{also}), else
#' the saved value, else NULL
#' @noRd
.morie_llm_setting <- function(key, also = character()) {
  for (e in c(unname(.morie_llm_setting_env[key]), also)) {
    if (is.na(e)) next
    v <- trimws(Sys.getenv(e, unset = ""))
    if (nzchar(v)) return(v)
  }
  v <- .morie_llm_read_config()[[key]]
  if (is.character(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(v))) trimws(v) else NULL
}

#' Internal helper: forget what the probes learned, after a setting changed
#' @noRd
.morie_llm_reset_probes <- function() {
  .morie_llm_cache$ollama_cached <- NULL
}

#' Show or save the language-model settings
#'
#' Every setting that decides how \code{\link{morie_llm_ask}} (and
#' \code{rmorie ask}) reaches a model: the route, your own OpenAI-compatible
#' server (\code{own.url}, \code{own.model}, \code{own.key}), Ollama
#' (\code{ollama.url}, \code{ollama.model}, \code{ollama.key}; \code{ollama.url}
#' may be \code{https://ollama.com} with your Ollama key), and a key and model
#' per provider (\code{anthropic.key}, \code{openai.model}, ...: the providers
#' are \code{rmoriebricklayer::bricklayer_llm_providers()}). The settings are
#' rmoriebricklayer's, in the file it shares with rmorie, so
#' \code{morie_llm_config()} is \code{rmoriebricklayer::bricklayer_llm_config()}.
#' An environment variable that is set wins over the saved value; a key is never
#' shown, only "set".
#'
#' @param ... Settings as \code{key = value}; \code{NULL} or \code{""} removes
#'   one. With no arguments nothing is written.
#' @return A data frame with one row per setting (\code{key}, \code{value},
#'   \code{source}, \code{env}, \code{help}).
#' @examples
#' old <- Sys.getenv("XDG_CONFIG_HOME", unset = NA)
#' Sys.setenv(XDG_CONFIG_HOME = tempfile())
#' head(morie_llm_config())   # reading writes nothing
#' if (is.na(old)) Sys.unsetenv("XDG_CONFIG_HOME") else Sys.setenv(XDG_CONFIG_HOME = old)
#' \dontrun{
#' morie_llm_config(anthropic.key = "sk-ant-...", route = "anthropic")
#' morie_llm_config(ollama.url = "http://192.168.1.20:11434", ollama.model = "qwen3:8b")
#' morie_llm_config(route = NULL)   # back to the automatic order
#' }
#' @export
morie_llm_config <- function(...) {
  out <- rmoriebricklayer::bricklayer_llm_config(...)
  if (...length()) .morie_llm_reset_probes()
  out
}

# `rmorie config ...` and `rmorie doctor`: rmoriebricklayer's verbs, under the rmorie name.
.cli_config <- function(rest, out) {
  .morie_llm_reset_probes()
  .cli_bricklayer(c("config", rest), out)
}

.cli_doctor <- function(out) .cli_bricklayer("doctor", out)

# Run one of rmoriebricklayer's shell verbs with its messages naming `rmorie`.
.cli_bricklayer <- function(args, out) {
  old <- Sys.getenv("RMBL_PROG", unset = NA)
  Sys.setenv(RMBL_PROG = "rmorie")
  on.exit(if (is.na(old)) Sys.unsetenv("RMBL_PROG") else Sys.setenv(RMBL_PROG = old), add = TRUE)
  rmoriebricklayer::bricklayer_cli(args, out = out)
}
