# ---- The rmorie command line, shipped inside the package -------------------
#
# `inst/bin/rmorie` is a two-line launcher that runs morie_cli() through
# Rscript; install_cli() links it into a directory on PATH (an R package
# cannot install executables itself). Everything the launcher does is an
# exported R function, so the same verbs work from an R session.

#' Run the rmorie command line
#'
#' Dispatches the verbs of the \code{rmorie} launcher. Verbs:
#' \describe{
#'   \item{\code{login [--email ADDRESS] [--code CODE] [--to-email] [--no-browser]}}{sign
#'     in to the hosted LLM tier with GitHub (device flow) or an emailed code;
#'     \code{--to-email} has the key emailed instead of stored}
#'   \item{\code{login --token [KEY]}}{store a key you already have (prompts
#'     for it when KEY is omitted)}
#'   \item{\code{logout}}{forget the hosted key}
#'   \item{\code{doctor}}{report the LLM providers reachable from this machine}
#'   \item{\code{models}}{list the models you can ask: the hosted tier's for your
#'     key (default marked), then the local Ollama server's}
#'   \item{\code{ask [--model NAME] PROMPT...}}{send a prompt to the active provider
#'     (or the named model) and print the reply}
#'   \item{\code{analyze SUBJECT [JSON]}}{run an analysis subject through \code{cli_main()}}
#'   \item{\code{version}}{print the package version}
#'   \item{\code{help}}{this list}
#' }
#' @param args Character vector of arguments; defaults to the command line.
#' @param out Connection or function for output (default: the console).
#' @return The exit status, invisibly (0 on success).
#' @examples
#' morie_cli("version")
#' morie_cli(c("ask", "--help"))
#' @export
morie_cli <- function(args = commandArgs(trailingOnly = TRUE), out = cat) {
  args <- as.character(args)
  verb <- if (length(args)) args[[1L]] else "help"
  rest <- args[-1L]
  pkg <- utils::packageName()
  flag <- function(name) {
    i <- match(name, rest)
    if (is.na(i)) return(NULL)
    if (i == length(rest)) stop(sprintf("%s needs a value", name), call. = FALSE)
    rest[[i + 1L]]
  }
  has <- function(name) name %in% rest
  status <- 0L
  tryCatch({
    switch(verb,
      login = {
        if (has("--token")) {
          tok <- if (match("--token", rest) < length(rest)) rest[[match("--token", rest) + 1L]] else ""
          if (!nzchar(tok)) tok <- trimws(readline("Paste your MORIE key: "))
          morie_llm_login(token = tok)
        } else {
          key <- morie_llm_login(open_browser = !has("--no-browser") && interactive(),
                                 email = flag("--email"), code = flag("--code"),
                                 to_email = has("--to-email"))
          if (nzchar(key)) out(sprintf("Logged in to %s\n", .morie_llm_hosted_base()))
        }
      },
      logout = { morie_llm_logout() },
      doctor = {
        rows <- list(
          c("Ollama (local)", if (morie_llm_probe_ollama()) "reachable" else "not reachable",
            .morie_llm_ollama_base()),
          c("Hosted LLM", if (is.null(.morie_llm_hosted_key())) "not logged in -- rmorie login"
                          else if (morie_llm_probe_hosted()) "logged in, gateway answering"
                          else "logged in, gateway not reachable",
            if (!is.null(.morie_llm_hosted_key()) && morie_llm_probe_hosted()) {
              hm <- morie_llm_hosted_models()
              sprintf("%s  models: %s (default %s)", .morie_llm_hosted_base(),
                      paste(hm, collapse = ", "), attr(hm, "default") %||% "")
            } else .morie_llm_hosted_base() %||% "disabled"),
          c("Gemini key", if (is.null(.morie_llm_gemini_key())) "absent" else "set", ""),
          c("OpenAI-compatible API", if (is.null(.morie_llm_api_base())) "absent" else "set", .morie_llm_api_base() %||% ""),
          c("OpenAI key", if (is.null(.morie_llm_openai_key())) "absent" else "set", ""))
        for (r in rows) out(sprintf("  %-24s %-32s %s\n", r[[1L]], r[[2L]], r[[3L]]))
        out(sprintf("  active provider: %s\n", morie_llm_detect_provider()))
      },
      models = {
        if (is.null(.morie_llm_hosted_key())) {
          out("Hosted LLM: not logged in -- rmorie login\n")
        } else {
          hm <- morie_llm_hosted_models()
          if (!length(hm)) {
            out(sprintf("Hosted LLM (%s): logged in, gateway not reachable (or the key was replaced by a newer sign-in: run rmorie login again)\n", .morie_llm_hosted_base() %||% "disabled"))
          } else {
            out(sprintf("Hosted LLM (%s); default marked *:\n", .morie_llm_hosted_base()))
            for (m in hm) out(sprintf("  %s %s\n", if (identical(m, attr(hm, "default"))) "*" else " ", m))
          }
        }
        if (morie_llm_probe_ollama()) {
          lm <- morie_llm_ollama_models()$name
          out(sprintf("Local Ollama (%s): %s\n", .morie_llm_ollama_base(),
                      if (length(lm)) paste(lm, collapse = ", ") else "running, no models pulled"))
        } else {
          out(sprintf("Local Ollama: not reachable at %s\n", .morie_llm_ollama_base()))
        }
        out("Pick one per call with `rmorie ask --model NAME ...`, or set MORIE_HOSTED_MODEL / MORIE_OLLAMA_MODEL.\n")
      },
      ask = {
        mdl <- flag("--model")
        if (!is.null(mdl)) rest <- rest[-(match("--model", rest) + 0:1)]
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie ask [--model NAME] PROMPT...\n")
        } else {
          out(paste0(morie_llm_ask(paste(rest, collapse = " "), model = mdl), "\n"))
        }
      },
      analyze = {
        if (!length(rest)) stop("usage: rmorie analyze SUBJECT [JSON]", call. = FALSE)
        res <- cli_main(rest[[1L]], if (length(rest) > 1L) rest[[2L]] else "{}")
        if (is.character(res)) out(paste0(res, "\n"))
      },
      version = out(sprintf("%s %s\n", pkg, as.character(utils::packageVersion(pkg)))),
      help = ,
      `--help` = ,
      `-h` = out(paste0(
        "usage: rmorie <verb> [options]\n\n",
        "  login [--email ADDRESS] [--code CODE] [--no-browser]   sign in to the hosted LLM tier\n",
        "        [--to-email]                                     ... and have the key emailed instead\n",
        "  login --token [KEY]                                    store a key you already have\n",
        "  logout                                                 forget the hosted key\n",
        "  doctor                                                 LLM providers reachable from here\n",
        "  models                                                 models you can ask (hosted + local)\n",
        "  ask [--model NAME] PROMPT...                           ask the active provider\n",
        "  analyze SUBJECT [JSON]                                 run an analysis subject\n",
        "  version                                                package version\n")),
      stop(sprintf("unknown verb '%s' (try: rmorie help)", verb), call. = FALSE))
  }, error = function(e) {
    out(paste0("rmorie: ", conditionMessage(e), "\n"))
    status <<- 1L
  })
  invisible(status)
}

#' Install the rmorie command-line launcher
#'
#' Links the launcher shipped in the package (\code{inst/bin/rmorie}) into a
#' directory on your PATH so that \code{rmorie login}, \code{rmorie ask ...}
#' and the other verbs of \code{\link{morie_cli}} work from any shell. On
#' Windows a \code{rmorie.cmd} wrapper is written instead of a symlink.
#' @param dir Target directory (default \code{~/.local/bin}; created if absent).
#' @param name Command name (default \code{rmorie}).
#' @return The path of the installed launcher, invisibly.
#' @examples
#' \dontrun{
#' install_cli()
#' }
#' @export
install_cli <- function(dir = file.path(path.expand("~"), ".local", "bin"), name = "rmorie") {
  src <- system.file("bin", "rmorie", package = utils::packageName())
  if (!nzchar(src)) stop("the launcher is missing from this installation")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (.Platform$OS.type == "windows") {
    target <- file.path(dir, paste0(name, ".cmd"))
    writeLines(sprintf("@echo off\r\nRscript --vanilla -e \"%s::morie_cli()\" --args %%*",
                       utils::packageName()), target)
  } else {
    target <- file.path(dir, name)
    if (file.exists(target) || !is.na(Sys.readlink(target))) unlink(target)
    ok <- file.symlink(src, target)
    if (!isTRUE(ok)) {
      file.copy(src, target, overwrite = TRUE)
      Sys.chmod(target, "0755")
    }
    Sys.chmod(src, "0755")
  }
  on_path <- dir %in% strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1L]]
  message(sprintf("Installed %s%s", target,
                  if (on_path) "" else sprintf("; add %s to your PATH", dir)))
  invisible(target)
}
