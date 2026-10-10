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
#'   \item{\code{doctor}}{report the LLM providers reachable from this machine, and
#'     the route \code{ask} will take}
#'   \item{\code{config [show | help | get KEY | set KEY VALUE | unset KEY | setup | path]}}{show
#'     or change the language-model settings (\code{\link{morie_llm_config}}): the
#'     route \code{ask} uses and the address, key and model of each route;
#'     \code{setup} walks through them}
#'   \item{\code{models}}{list the models you can ask: the hosted tier's for your
#'     key (default marked), then the local Ollama server's}
#'   \item{\code{ask [--model NAME] [--route ROUTE] PROMPT...}}{send a prompt to the
#'     active provider (or the named model, on the named route) and print the reply}
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
#'   \item{\code{help [start | llm | config | r]}}{this list, or a guide: getting
#'     started, the language-model routes, every setting, the same from R}
#' }
#' @param args Character vector of arguments; defaults to the command line.
#' @param out Connection or function for output (default: the console).
#' @return The exit status, invisibly (0 on success).
#' @examples
#' morie_cli("version")
#' morie_cli(c("ask", "--help"))
#' @export
morie_cli <- function(args = commandArgs(trailingOnly = TRUE), out = cat) {
  old_progress <- options(morie.progress = TRUE)  # a person is watching: downloads draw their bar
  on.exit(options(old_progress), add = TRUE)
  args <- as.character(args)
  if (length(args) && identical(args[[1L]], "--args")) args <- args[-1L]  # R >= 4.6 keeps the separator
  if (!nzchar(Sys.getenv("MORIE_CACHE_DB", ""))) {
    # The command line is an explicit user action: what `rmorie pull` fetches is kept
    # under the user cache directory so later runs (modules, pipeline, selftest) use it.
    Sys.setenv(MORIE_CACHE_DB = .morie_cli_cache_db())
    on.exit(Sys.unsetenv("MORIE_CACHE_DB"), add = TRUE)
  }
  # launchers of older versions passed an explicit --args separator, which R >= 4.6 keeps
  if (length(args) && identical(args[[1L]], "--args")) args <- args[-1L]
  verb <- if (length(args)) args[[1L]] else "help"
  rest <- args[-1L]
  pkg <- utils::packageName()
  if (!verb %in% c("help", "--help", "-h") && any(rest %in% c("--help", "-h"))) {
    return(.cli_verb_help(verb, out))  # `VERB --help` describes the verb; it never runs it
  }
  bad <- .cli_unknown_options(verb, rest)
  if (length(bad)) {
    # an option the verb does not take is refused, not run as data (exec evaluated it, ask sent it)
    out(sprintf("rmorie %s: unknown option %s (rmorie %s --help)\n", verb, bad[[1L]], verb))
    return(invisible(2L))
  }
  flag <- function(name) {
    i <- match(name, rest)
    if (is.na(i)) return(NULL)
    if (i == length(rest) || startsWith(rest[[i + 1L]], "--"))
      stop(structure(class = c("cli_usage", "error", "condition"),
                     list(message = sprintf("%s needs a value", name), call = NULL)))
    rest[[i + 1L]]
  }
  has <- function(name) name %in% rest
  status <- 0L
  opened <- NULL
  tryCatch(withCallingHandlers({
    switch(verb,
      login = {
        if (has("--token")) {
          tok <- if (match("--token", rest) < length(rest)) rest[[match("--token", rest) + 1L]] else ""
          # the launcher runs Rscript: .cli_readline() reads a typed or piped line (echo KEY | rmorie login --token)
          if (!nzchar(tok)) tok <- .cli_readline("Paste your MORIE key: ")
          if (is.na(tok) || !nzchar(tok)) {
            out("--token needs a value: rmorie login --token KEY, or paste the key when asked\n")
            status <- 2L
          } else if (!isTRUE(.morie_llm_probe_token(tok))) {
            out("the gateway did not accept that key; nothing stored (rmorie login mints one)\n")
            status <- 1L
          } else {
            morie_llm_login(token = tok)
          }
        } else {
          if (has("--to-email") && is.null(flag("--email"))) {
            out("--to-email needs --email ADDRESS (the key is emailed to that address)\n")
            status <- 2L
          } else {
          key <- morie_llm_login(open_browser = !has("--no-browser") && interactive(),
                                 email = flag("--email"), code = flag("--code"),
                                 to_email = has("--to-email"))
          if (nzchar(key)) out(sprintf("Logged in to %s\n", .morie_llm_hosted_base()))
          }
        }
      },
      logout = { morie_llm_logout() },
      doctor = status <- .cli_doctor(out),
      config = status <- .cli_config(rest, out),
      models = {
        if (is.null(.morie_llm_hosted_key())) {
          out(paste0("Hosted LLM: not logged in -- rmorie login (GitHub) or rmorie login --email you@example.com", .morie_httr2_note(), "\n"))
        } else {
          hm <- morie_llm_hosted_models()
          if (!length(hm)) {
            out(if (.morie_llm_hosted_rejected()) {
              sprintf("Hosted LLM (%s): the gateway rejected your key -- run rmorie login again\n", .morie_llm_hosted_base())
            } else {
              sprintf("Hosted LLM (%s): logged in, but the gateway is not reachable (offline?) -- rmorie doctor says more\n", .morie_llm_hosted_base() %||% "disabled")
            })
            status <- 1L
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
        out(paste0("Pick one per call with `rmorie ask --model NAME ...`; make one the default with ",
                   "`rmorie config set hosted.model NAME` (or ollama.model, own.model); ",
                   "point ask at your own server with `rmorie config set own.url URL` (rmorie help llm).\n"))
      },
      ask = {
        mdl <- flag("--model")
        if (!is.null(mdl)) rest <- rest[-(match("--model", rest) + 0:1)]
        rte <- flag("--route")
        if (!is.null(rte)) {
          rest <- rest[-(match("--route", rest) + 0:1)]
          rte <- tolower(rte)
          if (!rte %in% .morie_llm_routes) {
            stop(structure(class = c("cli_usage", "error", "condition"),
                           list(message = "--route takes auto, own, ollama or hosted", call = NULL)))
          }
        }
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie ask [--model NAME] [--route auto|own|ollama|hosted] PROMPT...\n")
          status <- 2L
        } else if (!identical(.morie_llm_route(rte), "auto") &&
                   identical(if (is.null(rte)) morie_llm_detect_provider() else morie_llm_detect_provider(route = rte),
                             "local")) {
          # a chosen route that is not set up: say what is missing, not the generic fallback text
          out(paste0("rmorie ask: ", .morie_llm_route_missing(.morie_llm_route(rte)), "\n"))
          status <- 1L
        } else {
          provider <- if (is.null(rte)) morie_llm_detect_provider() else morie_llm_detect_provider(route = rte)
          if (identical(provider, "local")) {
            out(paste0(.morie_llm_local_fallback(paste(rest, collapse = " ")), "\n"))
            out(.cli_llm_fallback_cause(mdl))
            status <- 1L
          } else {
            ans <- morie_llm_ask(paste(rest, collapse = " "), model = mdl, provider = provider, route = rte)
            out(paste0(trimws(ans), "\n"))  # some models open with blank lines
            if (isTRUE(attr(ans, "fallback"))) {
              out(.cli_llm_fallback_cause(mdl))
              status <- 1L
            }
          }
        }
      },
      `list-modules` = {
        d <- .morie_module_descriptions()
        w <- max(nchar(names(d)))
        for (m in morie_module_names()) {
          out(sprintf("  %-*s  %s\n", w, m, d[[m]] %||% ""))
          if (has("--outputs")) out(paste0(strrep(" ", w + 4L), paste(.morie_module_outputs[[m]], collapse = ", "), "\n"))
        }
        out("\nrmorie list-modules --outputs also lists the files each module writes.\n")
      },
      `run-module` = {
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie run-module NAME [--output-dir DIR] [--cpads FILE | --dataset KEY]   (names: rmorie list-modules; keys: rmorie list-datasets)\n")
          if (length(rest)) status <- 0L else status <- 2L
        } else if (!rest[[1L]] %in% morie_module_names()) {
          out(sprintf("Unknown module: %s (rmorie list-modules names them)\n", rest[[1L]]))
          status <- 1L
        } else {
          od <- flag("--output-dir") %||% file.path("morie-output", rest[[1L]])
          cp <- flag("--cpads")
          if (rest[[1L]] %in% c("otis-analysis", "mapq-psychometrics") && (!is.null(cp) || !is.null(flag("--dataset")))) {
            # these two bring their own frame; the option would otherwise be dropped without a word
            out(sprintf("note: %s runs on its own bundled frame; --cpads / --dataset are ignored\n", rest[[1L]]))
            cp <- NULL
          } else if (is.null(cp) && !is.null(flag("--dataset"))) cp <- .cpads_dataset_csv(flag("--dataset"))
          before <- .cli_file_stamps(od)
          res <- if (is.null(cp)) morie_run_morie_module(rest[[1L]], output_dir = od)
                 else morie_run_morie_module(rest[[1L]], cpads_csv = cp, output_dir = od)
          n_files <- .cli_files_written(od, before)
          if (n_files > 0L) {
            out(sprintf("Completed module: %s\n", rest[[1L]]))
            if (is.list(res) && length(names(res))) out(sprintf("Generated tables: %s\n", paste(names(res), collapse = ", ")))
            out(sprintf("Written to %s (%d files)\n", od, n_files))
          }
          if (n_files == 0L) {
            out(sprintf("%s wrote nothing%s\n", rest[[1L]],
                        if (rest[[1L]] %in% c("figures", "tables", "meta-synthesis", "final-report")) ": it collects the figures and tables a project checkout wrote (data/manifest/outputs); run the analysis modules into that tree first" else ""))
            status <- 1L
          }
        }
      },
      `list-datasets` = status <- .cli_list_datasets(out),
      pull = {
        if (!length(rest) || !nzchar(trimws(rest[[1L]]))) {  # `pull ""` is a usage error too
          out("usage: rmorie pull KEY [--out FILE.csv] | pull --all [--out DIR]   (keys: rmorie list-datasets)\n")
          status <- 2L
        } else if (identical(rest[[1L]], "--help")) {
          out("usage: rmorie pull KEY [--out FILE.csv] | pull --all [--out DIR]   (keys: rmorie list-datasets)\n")
        } else if (identical(rest[[1L]], "--all") && !has("-y") && !has("--yes") &&
                   (.cli_stdin_closed() || !tolower(trimws(.cli_readline(sprintf(
                     "pull --all writes every catalog dataset to %s (several GB). Continue? [y/N] ",
                     normalizePath(flag("--out") %||% "datasets", mustWork = FALSE))) %||% "")) %in% c("y", "yes"))) {
          # several GB: ask on a terminal, and need -y where nobody can answer
          out(if (.cli_stdin_closed()) "pull --all downloads several GB; no terminal to confirm on: pass -y\n" else "pull --all: nothing downloaded\n")
          status <- if (.cli_stdin_closed()) 2L else 1L
        } else if (identical(rest[[1L]], "--all")) {
          d <- morie_list_datasets()
          od <- flag("--out") %||% "datasets"
          dir.create(od, recursive = TRUE, showWarnings = FALSE)
          for (k in d$key) {
            miss <- .morie_own_file_missing(k)
            if (!is.null(miss)) {
              out(sprintf("  %-12s skipped: your own research file is not at %s\n", k, miss))
              next
            }
            r <- tryCatch(morie_load_dataset(k), error = function(e) e)
            if (inherits(r, "error")) {
              out(sprintf("  %-12s FAILED: %s\n", k, conditionMessage(r)))
            } else {
              .morie_write_csv_minimal(r, file.path(od, paste0(k, ".csv")))
              out(sprintf("  %-12s %s rows -> %s\n", k, format(nrow(r), big.mark = ","), file.path(od, paste0(k, ".csv"))))
            }
          }
        } else if (!is.null(miss <- .morie_own_file_missing(rest[[1L]]))) {
          # pull writes real data only: the synthetic panel the analyses fall back on is not the file
          out(sprintf("%s is your own research file and it is not at %s: put it there (MORIE_DATA_DIR moves the data directory). The analyses use a synthetic toy panel until then.\n",
                      rest[[1L]], miss))
          status <- 1L
        } else {
          dest <- flag("--out") %||% paste0(gsub("[^A-Za-z0-9_.-]", "_", rest[[1L]]), ".csv")
          df <- withCallingHandlers(morie_load_dataset(rest[[1L]]), warning = function(w) {
            # connection-layer warnings (url(), download.file) precede an error that already names the cause
            if (grepl("cannot open|URL|InternetOpenUrl|download|connection|proxy", conditionMessage(w), ignore.case = TRUE)) {
              invokeRestart("muffleWarning")
            }
          })
          dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
          if (is.environment(df)) {
            # an R environment (research objects), not a table: hand over the file itself
            src <- attr(df, "morie_path")
            dest <- sub("\\.csv$", "", dest, ignore.case = TRUE)
            dest <- paste0(dest, ".", tools::file_ext(src))
            if (!isTRUE(file.copy(src, dest, overwrite = TRUE))) stop(sprintf("cannot write %s", dest), call. = FALSE)
            out(sprintf("%s is an R environment (%d objects), not a table: copied it to %s; open it with load(\"%s\") or morie_load_dataset(\"%s\")\n",
                        rest[[1L]], length(ls(df)), dest, dest, rest[[1L]]))
          } else {
          wrote <- if (dir.exists(dest)) {
            out(sprintf("cannot write %s: it is a directory\n", dest))
            FALSE
          } else tryCatch({
            # the connection warning (not a regular file, permission denied) precedes the error
            # that names the cause; report that once, not both
            withCallingHandlers(.morie_write_csv_minimal(df, dest), warning = function(w) {
              if (grepl("cannot open|not a regular file", conditionMessage(w))) invokeRestart("muffleWarning")
            })
            TRUE
          }, error = function(e) {
            out(sprintf("cannot write %s: %s\n", dest, conditionMessage(e)))
            FALSE
          })
          if (wrote) out(sprintf("wrote %s  (%d rows, %d cols)\n", dest, nrow(df), ncol(df))) else status <- 1L
          }
        }
      },
      provider = {
        sub <- if (length(rest)) rest[[1L]] else "show"
        if (identical(sub, "set")) {
          morie_llm_provider_set(flag("--base-url") %||% stop(structure(class = c("cli_usage", "error", "condition"),
                                 list(message = "provider set needs --base-url URL", call = NULL))),
                                 flag("--key") %||% stop(structure(class = c("cli_usage", "error", "condition"),
                                                                   list(message = "provider set needs --key KEY", call = NULL))),
                                 model = flag("--model"))
        } else if (identical(sub, "unset")) {
          morie_llm_provider_unset()
        } else if (identical(sub, "show")) {
          morie_llm_provider_show()
        } else {
          out("usage: rmorie provider set --base-url URL --key KEY [--model NAME] | show | unset\n")
          status <- 2L
        }
      },
      explain = {
        if (!length(rest) || identical(rest[[1L]], "--help")) {
          out("usage: rmorie explain FILENAME\n")
          if (!length(rest)) status <- 2L
        } else {
          txt <- explain_file(rest[[1L]])
          out(paste0(txt, "\n"))
          if (grepl("^No registered explanation", txt)) status <- 1L  # an unknown table is not a success
        }
      },
      inspect = {
        if (identical(rest[1L], "--help")) out("usage: rmorie inspect PATH [--module NAME]\n")
        else status <- .cli_inspect(rest, flag, out)
      },
      verify = {
        if (identical(rest[1L], "--help")) out("usage: rmorie verify PATH [--module NAME]\n")
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
      `generate-template` = status <- .cli_generate_template(rest, flag, has, out),
      update = status <- .cli_update(has, out),
      crypto = status <- .cli_crypto(rest, flag, out),
      ingest = status <- .cli_ingest(rest, flag, has, out),
      `download-bootstrap` = status <- .cli_download_bootstrap(flag, out, has),
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
        "  rmorie pull chicago_crime/incidents          # curated tables at data.rmorie.com (after rmorie login, GitHub or --email)\n\n",
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
        if (!length(rest)) {
          out("usage: rmorie analyze SUBJECT [JSON]   (subjects: otis, siu, tps, nypd, cpd; JSON keys: otis {\"data\":FILE,\"year\":2023,\"sex\":\"Male\"}, siu {\"data\":FILE}, tps {\"datasets\":[\"Assault\"],\"nrows\":5000} or {\"data\":FILE})\n")
          status <- 2L
        } else if (length(rest) > 1L && !.cli_json_object(rest[[2L]])) {
          out(sprintf("rmorie analyze: the second argument must be a JSON object, e.g. '{\"data\":\"FILE.csv\"}'; got: %s\n", rest[[2L]]))
          status <- 2L
        } else {
          res <- cli_main(rest[[1L]], if (length(rest) > 1L) rest[[2L]] else "{}")
          if (is.character(res)) out(paste0(res, "\n"))
          if ((is.list(res) && identical(res$status, "error")) ||
              (is.character(res) && grepl("\"status\"\\s*:\\s*\"error\"", res))) status <- 1L
          if (status == 0L && .cli_analyze_all_failed(res)) {
            out("every analysis in this subject failed (each entry's warnings field says why)\n")
            status <- 1L
          }
        }
      },
      `--version` = ,
      `-v` = ,
      version = out(sprintf("%s %s\n", pkg, as.character(utils::packageVersion(pkg)))),
      help = ,
      `--help` = ,
      `-h` = status <- .cli_help(rest, out),
      stop(structure(class = c("cli_usage", "error", "condition"),
                     list(message = sprintf("unknown verb '%s' (try: rmorie help)", verb), call = NULL))))
  }, warning = function(w) {
    # R reports a refused file as a warning naming the path, then a bare "cannot open the connection"
    # error; keep the path and the reason, and do not let the raw warning leak after the message
    if (!grepl("cannot open file '", conditionMessage(w), fixed = TRUE)) return(invisible())
    opened <<- sub("^.*cannot open file '(.*)': (.*)$", "\\1: \\2", conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    msg <- conditionMessage(e)
    if (!is.null(opened) && grepl("cannot open (the connection|file)", msg)) {
      msg <- paste0("cannot open ", opened)
    } else if (grepl("cannot open (the connection|file)|[Pp]ermission denied", msg) &&
               !grepl("could not be reached|cannot open the connection to '", msg)) {  # a URL: the network, not a path
      msg <- paste0(msg, " (permission denied, or the path does not exist)")
    }
    out(paste0("rmorie ", verb, ": ", msg, "\n"))
    status <<- if (inherits(e, "cli_usage")) 2L else 1L
  })
  invisible(status)
}

# `rmorie help [TOPIC]`: the verbs, a getting-started guide, and one page per topic.
.cli_help <- function(rest, out) {
  topic <- if (length(rest) && !startsWith(rest[[1L]], "-")) tolower(rest[[1L]]) else ""
  verbs <- paste0(
        "usage: rmorie <verb> [options]\n\n",
        "  login [--email ADDRESS] [--code CODE] [--no-browser]   sign in to the hosted LLM tier\n",
        "        [--to-email]                                     ... and have the key emailed instead\n",
        "  login --token [KEY]                                    store a key you already have\n",
        "  logout                                                 forget the hosted key\n",
        "  doctor                                                 LLM routes reachable from here, and the one ask uses\n",
        "  models                                                 models you can ask (hosted + local)\n",
        "  ask [--model NAME] [--route auto|own|ollama|hosted] PROMPT...   ask a model (this route only, with --route)\n",
        "  config [show | help | get KEY | set KEY VALUE | unset KEY | setup | path]   language-model settings\n",
        "        (route, addresses, keys, models; saved in ~/.config/morie/llm.json; setup asks for each)\n",
        "  analyze SUBJECT [JSON]                                 run an analysis subject\n",
        "  list-modules [--outputs]                               the analysis modules (--outputs: the files each writes)\n",
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
        "  sample PATH --n N [--method srs|stratified|cluster|pps] [--output F]   draw N rows (stratified: N in total, --per-stratum for N each; .weight = design weight)\n",
        "  run-modules [--modules a,b] [--cpads FILE] [--output-dir DIR]   run several modules\n",
        "  pipeline (--all | --modules a,b) [--output-dir DIR] [--no-carbon]   run modules, track CO2, seal a capsule\n",
        "  emissions [--seconds N] [--output-dir DIR] [--country ISO3] [--no-capsule]   measure this machine's compute emissions (--country CAN; detected when omitted)\n",
        "  verify-pollution --pollutant no2|pm25 (--demo | --exposure-csv F | --exposure-mean X ...)   pollution -> health pipeline\n",
        "  percy | agent [--model NAME] [--context TEXT] QUESTION   talk to Perseus (same fallback chain as ask)\n",
        "  chat [--model NAME]                                    interactive conversation\n",
        "  selftest                                               smoke test of the subsystems\n",
        "  tutorial [--dry-run]                                   first-time walkthrough\n",
        "  generate-template [MODULE | --module NAME] [--out FILE] [--force]   methods + results scaffold for a first paper\n",
        "  update [--yes]                                         check for a newer release, optionally install\n",
        "  crypto keygen|encrypt|decrypt ...                      post-quantum file encryption (ML-KEM-768 + ChaCha20)\n",
        "  ingest ckan|tps|siu|a2aj ...                           pull open-data feeds\n",
        "  download-bootstrap --survey KEY|all                    cache the survey bootstrap-weight files (hundreds of MB each)\n",
        "  exec 'R CODE' | --file F                               evaluate R code\n",
        "  edit FILE                                              open a file in your editor\n",
        "  percysuits [--dry-run] [--host URL]                    pull the Perseus model set into Ollama\n",
        "  verify-earth-engine                                    (Python side only)\n",
        "  version                                                package version\n",
        "  help [start | llm | config | r]                        this list, or a guide; VERB --help for one verb\n\n",
        "Guides:  rmorie help start   getting started, step by step\n",
        "         rmorie help llm     every way to point ask at a model (hosted, Ollama, your own server)\n",
        "         rmorie help config  every language-model setting, its environment variable, examples\n",
        "         rmorie help r       the same from R and Rscript\n\n",
        "Install or update:  install.packages(\"rmorie\", repos = c(\"https://rootcoder007.r-universe.dev\", ",
        "\"https://cloud.r-project.org\"))\n",
        "Launcher on PATH:   Rscript -e 'rmorie::install_cli()'\n",
        "Python side:        pip install morie   (then: morie r-install)\n"
  )
  start <- paste0(
    "Getting started with rmorie\n\n",
    "1. Check what is set up (and which route `ask` will take):\n",
    "     rmorie doctor\n",
    "2. Pick a model source (any one is enough):\n",
    "     hosted MORIE tier   rmorie login                  (GitHub, or --email ADDRESS for a code)\n",
    "                         rmorie login --token KEY      (a key issued at https://rmorie.com/access)\n",
    "     local Ollama        ollama pull qwen3:8b          (https://ollama.com)\n",
    "     your own server     rmorie config set own.url http://localhost:1234/v1\n",
    "   or answer a few questions instead:  rmorie config setup\n",
    "3. Ask:\n",
    "     rmorie ask \"which module fits a treatment-control design?\"\n",
    "     rmorie ask --model gpt-oss-120b:cf \"...\"      (one model, this time)\n",
    "     rmorie ask --route hosted \"...\"               (one route, this time)\n",
    "4. Make a choice stick:\n",
    "     rmorie config set route hosted\n",
    "     rmorie config set hosted.model gpt-oss-120b:cf\n",
    "5. Run an analysis (no model needed):\n",
    "     rmorie list-modules\n",
    "     rmorie run-module power-design --output-dir out/\n",
    "     rmorie tutorial                                 (a guided walk through one analysis)\n",
    "     rmorie cheatsheet                               (one-page reference)\n"
  )
  llm <- paste0(
    "Where ask sends a prompt\n\n",
    "With route = auto (the default) ask tries, in order: a local Ollama server (only if it has a\n",
    "model), your GEMINI_API_KEY, your own server (own.url), your OPENAI_API_KEY, then the hosted\n",
    "MORIE tier (if you are logged in). A route other than auto is the only one asked.\n",
    "`rmorie doctor` shows each one and the route ask will take.\n\n",
    "Hosted MORIE tier (the :cloud and :cf models)\n",
    "  rmorie login                                 sign in (GitHub, or --email ADDRESS for a code)\n",
    "  rmorie login --token KEY                     paste a key; it is checked before it is saved\n",
    "  rmorie models                                the models your key can use\n",
    "  rmorie config set route hosted               always use it, even with Ollama running\n",
    "  rmorie config set hosted.model NAME          its default model\n\n",
    "Ollama, on this machine or another\n",
    "  ollama pull qwen3:8b                         a model to use\n",
    "  rmorie config set ollama.url http://192.168.1.20:11434\n",
    "  rmorie config set ollama.model qwen3:8b\n",
    "  rmorie config set ollama.url off             never try Ollama\n\n",
    "Your own OpenAI-compatible server (LM Studio, vLLM, llama.cpp, a provider's API)\n",
    "  rmorie config set own.url http://localhost:1234/v1\n",
    "  rmorie config set own.model NAME\n",
    "  rmorie config set own.key                    (asks for the key, so it stays out of history)\n\n",
    "One call only\n",
    "  rmorie ask --route ollama --model qwen3:8b \"...\"\n",
    "  MORIE_LLM_ROUTE=hosted rmorie ask \"...\"      (environment variables win over saved settings)\n"
  )
  r_help <- paste0(
    "From R (or Rscript -e '...': single quotes outside, double quotes inside)\n\n",
    "  library(rmorie)\n",
    "  morie_llm_config()                                        # every setting, where it comes from\n",
    "  morie_llm_config(route = \"hosted\", hosted.model = \"gpt-oss-120b:cf\")\n",
    "  morie_llm_config(ollama.url = \"http://192.168.1.20:11434\", ollama.model = \"qwen3:8b\")\n",
    "  morie_llm_config(own.url = \"http://localhost:1234/v1\", own.model = \"m\")\n",
    "  morie_llm_config(route = NULL)                            # back to auto\n",
    "  morie_llm_login()                                         # or email = \"you@example.org\", token = \"KEY\"\n",
    "  morie_llm_detect_provider()                               # the provider ask will use\n",
    "  morie_llm_hosted_models()\n",
    "  morie_llm_ask(\"Which module fits a treatment-control design?\", route = \"hosted\")\n",
    "  morie_cli(c(\"config\", \"set\", \"route\", \"hosted\"))         # any shell verb, from R\n\n",
    "From the shell:\n",
    "  Rscript -e 'rmorie::morie_llm_config(route = \"hosted\")'\n",
    "  Rscript -e 'cat(rmorie::morie_llm_ask(\"What is a difference-in-differences design?\"), \"\\n\")'\n",
    "  Rscript -e 'rmorie::install_cli()'                       # puts the rmorie command on PATH\n"
  )
  if (!nzchar(topic)) {
    out(verbs)
  } else if (topic %in% c("start", "getting-started", "quickstart")) {
    out(start)
  } else if (topic %in% c("llm", "routes", "hosted", "ollama", "models")) {
    out(llm)
  } else if (identical(topic, "config")) {
    return(.cli_config("help", out))
  } else if (topic %in% c("r", "rscript")) {
    out(r_help)
  } else {
    return(.cli_verb_help(topic, out))
  }
  0L
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
  # the launcher pins the library this copy of the package lives in, so the shell runs the same
  # rmorie as the R session that installed it, whatever R_LIBS the shell or ~/.Renviron sets: the
  # pin is applied inside R with .libPaths(), after Rscript has read every environment file
  lib <- normalizePath(dirname(system.file(package = utils::packageName())), winslash = "/")
  pkg <- utils::packageName()
  if (.Platform$OS.type == "windows") {
    target <- file.path(dir, paste0(name, ".cmd"))
    writeLines(sprintf(paste0("@echo off\r\n",
                              "Rscript --no-save --no-restore -e \".libPaths(c('%s', .libPaths())); q <- %s::morie_cli(); quit(status = as.integer(q))\" %%*"),
                       lib, pkg), target)
  } else {
    target <- file.path(dir, name)
    if (file.exists(target) || !is.na(Sys.readlink(target))) unlink(target)
    writeLines(c("#!/bin/sh",
                 sprintf("# rmorie command-line launcher (written by %s::install_cli()); runs the package installed in", pkg),
                 sprintf("# %s (pinned with .libPaths() inside R, so R_LIBS in the shell or ~/.Renviron cannot replace it)", lib),
                 sprintf("exec Rscript --no-save --no-restore -e 'suppressPackageStartupMessages({ .libPaths(c(\"%s\", .libPaths())); q <- %s::morie_cli(); quit(status = as.integer(q)) })' \"$@\"", lib, pkg)),
               target)
    Sys.chmod(target, "0755")
  }
  on_path <- dir %in% strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1L]]
  message(sprintf("Installed %s%s", target,
                  if (on_path) "" else sprintf("; add %s to your PATH", dir)))
  invisible(target)
}

# The CSV `pull` writes, as morie's Python writes it (pandas' to_csv): a field is quoted only
# when it holds a comma, a quote or a line break, a missing value is an empty field, numbers in
# R's 15 significant digits, UTF-8. write.csv quoted every text field and wrote NA, so the two
# arms' files for one dataset did not compare equal.
.morie_write_csv_minimal <- function(df, path) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  q <- function(v) {
    v <- enc2utf8(as.character(v))
    need <- !is.na(v) & grepl("[,\"\r\n]", v)
    v[need] <- paste0("\"", gsub("\"", "\"\"", v[need], fixed = TRUE), "\"")
    v[is.na(v)] <- ""
    v
  }
  cols <- lapply(df, function(v) {
    if (is.factor(v)) v <- as.character(v)
    if (inherits(v, c("Date", "POSIXt"))) v <- format(v)
    q(v)
  })
  lines <- c(paste(q(names(df)), collapse = ","),
             if (nrow(df)) do.call(paste, c(unname(cols), sep = ",")))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
  invisible(path)
}
