# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ask the rmorie terminal agent
#'
#' Thin wrapper that shells out to the \code{rmorie} command-line agent (a
#' separate, optional binary from \pkg{rmorie-cli}). The agent can run R,
#' read/write files, remember notes, and use any configured model backend
#' (Anthropic, OpenAI-compatible, Google Gemini, or a local Ollama model).
#' The CLI is the single implementation; this function only forwards to it,
#' so every MORIE package can offer agentic help without duplicating logic.
#'
#' @param task Character scalar. The task/prompt for the agent.
#' @param model Optional model id, e.g. \code{"claude-sonnet-5"},
#'   \code{"gpt-4o"}, \code{"gemini-2.5-flash"}, or any tag your Ollama
#'   server serves. Defaults to the CLI's default model.
#' @param backend Optional backend: \code{"auto"} (route on the model id),
#'   \code{"anthropic"}, \code{"openai"}, \code{"gemini"}, or \code{"ollama"}.
#' @param dry_run Logical; if \code{TRUE} the agent only plans (no network).
#' @return Character scalar: the agent's combined stdout/stderr.
#' @details Requires the \code{rmorie} binary on \code{PATH}. Cloud backends
#'   read \code{RMORIE_AGENT_API_KEY} from the environment (the key is never
#'   passed through R). If the binary is absent a clear message is returned
#'   instead of an error.
#' @examples
#' \dontrun{
#' agent("use R to compute the mean of 1:10")
#' agent("summarize mtcars", model = "gemini-2.5-flash", backend = "gemini")
#' }
#' @export
agent <- function(task, model = NULL, backend = "auto", dry_run = FALSE) {
  stopifnot(is.character(task), length(task) == 1L, nzchar(task))
  bin <- Sys.which("rmorie")
  if (!nzchar(bin)) {
    return("rmorie CLI not found on PATH. Install rmorie-cli to use agent().")
  }
  args <- c("agent", "--backend", backend)
  if (!is.null(model)) args <- c(args, "-m", model)
  if (isTRUE(dry_run)) args <- c(args, "--dry-run")
  args <- c(args, task)
  .morie_ensure_exec_allowed("rmorie agent execution")
  out <- suppressWarnings(
    system2(bin, args = args, stdout = TRUE, stderr = TRUE))
  paste(out, collapse = "\n")
}

#' Is the rmorie CLI agent available?
#'
#' @return Logical scalar; \code{TRUE} if the \code{rmorie} binary is on PATH.
#' @examples
#' agent_available()
#' @export
agent_available <- function() nzchar(Sys.which("rmorie"))
