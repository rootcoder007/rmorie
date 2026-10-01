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
#'   \item{\code{explain FILENAME}, \code{inspect PATH}, \code{verify PATH}}{read,
#'     browse and validate module output tables}
#'   \item{\code{profile-dataset PATH}, \code{sample PATH --n N}}{profile a
#'     CSV, draw a sample from it}
#'   \item{\code{run-modules}, \code{pipeline}}{run several modules; the
#'     pipeline tracks compute emissions and seals them in a capsule}
#'   \item{\code{emissions}, \code{verify-pollution}}{measure this machine's
#'     compute emissions; run the pollution-to-health pipeline}
#'   \item{\code{percy}, \code{agent}, \code{chat}}{talk to Perseus through the
#'     provider chain}
#'   \item{\code{selftest}, \code{tutorial}, \code{generate-template},
#'     \code{update}}{smoke test, walkthrough, first-paper scaffold, update check}
#'   \item{\code{crypto}, \code{ingest}, \code{download-bootstrap},
#'     \code{exec}, \code{edit}, \code{percysuits}}{file encryption, open-data
#'     feeds, bootstrap weights, evaluate R code, edit a file, pull models}
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
            out(sprintf("Hosted LLM (%s): logged in, but the gateway did not accept the key or did not answer; run rmorie login again, or rmorie doctor\n", .morie_llm_hosted_base() %||% "disabled"))
          } else {
            out(sprintf("Hosted LLM (%s); default marked *:\n", .morie_llm_hosted_base()))
            for (m in hm) out(sprintf("  %s %s\n", if (identical(m, attr(hm, "default"))) "*" else " ", m))
          }
        }
        if (!is.null(.morie_llm_api_base()) && !is.null(.morie_llm_api_key())) {
          out(sprintf("Your endpoint (%s): model %s\n", .morie_llm_api_base(), .morie_llm_api_model()))
        }
        if (morie_llm_probe_ollama()) {
          lm <- morie_llm_ollama_models()$name
          out(sprintf("Local Ollama (%s): %s\n", .morie_llm_ollama_base(),
                      if (length(lm)) paste(lm, collapse = ", ") else "running, no models pulled"))
        } else {
          out(sprintf("Local Ollama: not reachable at %s\n", .morie_llm_ollama_base()))
        }
        out("Pick one per call with `rmorie ask --model NAME ...`, or set MORIE_HOSTED_MODEL / MORIE_OLLAMA_MODEL; attach your own endpoint with `rmorie provider set`.\n")
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
      `list-modules` = {
        for (m in morie_module_names()) out(paste0("  ", m, "\n"))
      },
      `run-module` = {
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie run-module NAME [--output-dir DIR] [--cpads FILE | --dataset KEY]   (names: rmorie list-modules; keys: rmorie list-datasets)\n")
        } else {
          od <- flag("--output-dir")
          cp <- flag("--cpads")
          if (is.null(cp) && !is.null(flag("--dataset"))) cp <- .cpads_dataset_csv(flag("--dataset"))
          res <- if (is.null(cp)) morie_run_morie_module(rest[[1L]], output_dir = od)
                 else morie_run_morie_module(rest[[1L]], cpads_csv = cp, output_dir = od)
          out(sprintf("Completed module: %s\n", rest[[1L]]))
          if (is.list(res) && length(names(res))) out(sprintf("Generated tables: %s\n", paste(names(res), collapse = ", ")))
        }
      },
      `list-datasets` = {
        d <- morie_list_datasets()
        out(paste0(utils::capture.output(print(d, row.names = FALSE)), collapse = "\n"))
        out("\n")
      },
      pull = {
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie pull KEY [--out FILE.csv] | pull --all [--out DIR]   (keys: rmorie list-datasets)\n")
        } else if (identical(rest[[1L]], "--all")) {
          d <- morie_list_datasets()
          od <- flag("--out") %||% "datasets"
          dir.create(od, recursive = TRUE, showWarnings = FALSE)
          for (k in d$key) {
            r <- tryCatch(morie_load_dataset(k), error = function(e) e)
            if (inherits(r, "error")) {
              out(sprintf("  %-12s FAILED: %s\n", k, conditionMessage(r)))
            } else {
              utils::write.csv(r, file.path(od, paste0(k, ".csv")), row.names = FALSE)
              out(sprintf("  %-12s %s rows -> %s\n", k, format(nrow(r), big.mark = ","), file.path(od, paste0(k, ".csv"))))
            }
          }
        } else {
          dest <- flag("--out") %||% paste0(gsub("[^A-Za-z0-9_.-]", "_", rest[[1L]]), ".csv")
          df <- morie_load_dataset(rest[[1L]])
          utils::write.csv(df, dest, row.names = FALSE)
          out(sprintf("wrote %s  (%d rows, %d cols)\n", dest, nrow(df), ncol(df)))
        }
      },
      provider = {
        sub <- if (length(rest)) rest[[1L]] else "show"
        if (identical(sub, "set")) {
          morie_llm_provider_set(flag("--base-url") %||% stop("provider set needs --base-url", call. = FALSE),
                                 flag("--key") %||% stop("provider set needs --key", call. = FALSE),
                                 model = flag("--model"))
        } else if (identical(sub, "unset")) {
          morie_llm_provider_unset()
        } else if (identical(sub, "show")) {
          morie_llm_provider_show()
        } else {
          out("usage: rmorie provider set --base-url URL --key KEY [--model NAME] | show | unset\n")
        }
      },
      explain = {
        if (!length(rest) || identical(rest[[1L]], "--help")) out("usage: rmorie explain FILENAME\n")
        else out(paste0(explain_file(rest[[1L]]), "\n"))
      },
      inspect = {
        if (!length(rest) || identical(rest[[1L]], "--help")) out("usage: rmorie inspect PATH [--module NAME]\n")
        else status <- .cli_inspect(rest, flag, out)
      },
      verify = {
        if (!length(rest) || identical(rest[[1L]], "--help")) out("usage: rmorie verify PATH [--module NAME]\n")
        else status <- .cli_verify(rest, flag, out)
      },
      `profile-dataset` = status <- .cli_profile_dataset(rest, flag, has, out),
      sample = status <- .cli_sample(rest, flag, has, out),
      `run-modules` = status <- .cli_run_modules(rest, flag, has, out),
      pipeline = status <- .cli_run_modules(rest, flag, has, out, pipeline = TRUE),
      percy = ,
      perseus = ,
      agent = status <- .cli_percy(rest, flag, out, verb),
      chat = status <- .cli_chat(rest, flag, out),
      selftest = status <- .cli_selftest(out),
      tutorial = status <- .cli_tutorial(has, out),
      `generate-template` = status <- .cli_generate_template(flag, out),
      update = status <- .cli_update(has, out),
      crypto = status <- .cli_crypto(rest, flag, out),
      ingest = status <- .cli_ingest(rest, flag, has, out),
      `download-bootstrap` = status <- .cli_download_bootstrap(flag, out),
      exec = status <- .cli_exec(rest, flag, out),
      edit = status <- .cli_edit(rest, out),
      percysuits = status <- .cli_percysuits(flag, has, out),
      `verify-pollution` = status <- .cli_verify_pollution(rest, flag, has, out),
      emissions = status <- .cli_emissions(flag, has, out),
      `verify-earth-engine` = {
        out("verify-earth-engine needs the Google Earth Engine Python client; run it from the Python package:  pip install morie && morie verify-earth-engine\n")
        status <- 2L
      },
      cheatsheet = out(paste0(
        "rmorie cheat sheet\n==================\n\n",
        "INSTALL\n",
        "  install.packages(\"rmorie\", repos = c(\"https://rootcoder007.r-universe.dev\", \"https://cloud.r-project.org\"))\n",
        "  Rscript -e 'rmorie::install_cli()'          # the rmorie launcher on PATH\n\n",
        "RUN AN ANALYSIS\n",
        "  rmorie list-modules\n",
        "  rmorie run-module power-design --output-dir out/\n\n",
        "PULL DATA\n",
        "  rmorie list-datasets\n",
        "  rmorie pull ocp21 --out cpads.csv            # the real CPADS PUMF; cached, then the modules use it\n",
        "  rmorie pull --all --out datasets/            # every dataset the catalog knows\n",
        "  rmorie pull chicago_crime/incidents          # curated tables at data.rmorie.com (after rmorie login)\n\n",
        "ASK A MODEL\n",
        "  rmorie login                                 # hosted tier, free: GitHub or email sign-in\n",
        "  rmorie models                                # what you can ask, default marked *\n",
        "  rmorie ask \"which module fits a treatment-control design?\"\n",
        "  rmorie ask --model NAME \"...\"\n",
        "  rmorie doctor                                # which routes answer from this machine\n",
        "  rmorie provider set --base-url URL --key KEY [--model NAME]\n",
        "                                               # attach your own OpenAI-compatible endpoint\n\n",
        "OUTPUTS AND DATA\n",
        "  rmorie explain power_two_proportion_gender.csv\n",
        "  rmorie inspect out/   |   rmorie verify out/\n",
        "  rmorie profile-dataset data.csv --suggest   |   rmorie sample data.csv --n 100\n\n",
        "COMPUTE EMISSIONS AND POLLUTION\n",
        "  rmorie pipeline --all --output-dir out/     # CO2 of the run + signed capsule in out/emissions/\n",
        "  rmorie emissions --seconds 5                # measure this machine; morie_emissions_verify() checks the capsule\n",
        "  rmorie verify-pollution --pollutant no2 --demo\n\n",
        "PYTHON SIDE\n",
        "  pip install morie   then   morie cheatsheet\n")),
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
        "  list-modules                                           the CPADS analysis modules\n",
        "  run-module NAME [--output-dir DIR] [--cpads FILE | --dataset KEY]   run one module (--dataset ocp21 = the real PUMF)\n",
        "  list-datasets                                          built-in dataset keys and cache state\n",
        "  pull KEY [--out FILE.csv] | pull --all [--out DIR]     download a dataset (or every one) as CSV; cached for the modules\n",
        "  cheatsheet                                             one-page reference\n",
        "  provider set --base-url URL --key KEY [--model NAME]   attach your own model endpoint\n",
        "  provider show | unset                                  ... see it, or detach it\n",
        "  explain FILENAME                                       what an output table contains, how to read it\n",
        "  inspect PATH [--module NAME]                           schema, rows and preview of output CSVs\n",
        "  verify PATH [--module NAME]                            validate statistical outputs (exit 1 on failure)\n",
        "  profile-dataset PATH [--treatment C] [--outcome C] [--suggest]   variable types, roles, analysis plan\n",
        "  sample PATH --n N [--method srs|stratified|cluster|pps] [--output F]   draw a sample\n",
        "  run-modules [--modules a,b] [--cpads FILE] [--output-dir DIR]   run several modules\n",
        "  pipeline (--all | --modules a,b) [--output-dir DIR] [--no-carbon]   run modules, track CO2, seal a capsule\n",
        "  emissions [--seconds N] [--output-dir DIR] [--no-capsule]   measure this machine's compute emissions\n",
        "  verify-pollution --pollutant no2|pm25 (--demo | --exposure-csv F | --exposure-mean X ...)   pollution -> health pipeline\n",
        "  percy | agent [--model NAME] [--context TEXT] QUESTION   talk to Perseus (same fallback chain as ask)\n",
        "  chat [--model NAME]                                    interactive conversation\n",
        "  selftest                                               smoke test of the subsystems\n",
        "  tutorial [--dry-run]                                   first-time walkthrough\n",
        "  generate-template [--module NAME] [--out FILE]         methods + results scaffold for a first paper\n",
        "  update [--yes]                                         check for a newer release, optionally install\n",
        "  crypto keygen|encrypt|decrypt ...                      post-quantum file encryption (ML-KEM-768 + ChaCha20)\n",
        "  ingest ckan|tps|siu|a2aj ...                           pull open-data feeds\n",
        "  download-bootstrap [--survey all|csads_2021|...]       cache the survey bootstrap-weight files\n",
        "  exec 'R CODE' | --file F                               evaluate R code\n",
        "  edit FILE                                              open a file in your editor\n",
        "  percysuits [--dry-run]                                 pull the Perseus model set into Ollama\n",
        "  verify-earth-engine                                    (Python side only)\n",
        "  version                                                package version\n",
        "  help | -h | --help                                     this list; VERB --help for one verb\n\n",
        "Install or update:  install.packages(\"rmorie\", repos = c(\"https://rootcoder007.r-universe.dev\", ",
        "\"https://cloud.r-project.org\"))\n",
        "Launcher on PATH:   Rscript -e 'rmorie::install_cli()'\n",
        "Python side:        pip install morie   (then: morie r-install)\n")),
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
