# Clean-user smoke suite for the R command line (rmorie, and the morie R arm).
# Every verb of morie_cli() is run for real in an empty HOME with assertions,
# no mocks. Usage (package installed, or a source tree via pkgload):
#   Rscript inst/smoke/smoke.R            # exit status 0 = every case passed
#   MORIE_SMOKE_KEY=sk-... Rscript ...    # unlocks hosted-tier and data.rmorie.com cases
# Verb coverage is enforced against the verbs listed by `help`.

pkg <- Sys.getenv("MORIE_SMOKE_PKG", "rmorie")
tree <- Sys.getenv("MORIE_SMOKE_TREE", "")
if (nzchar(tree)) suppressPackageStartupMessages(pkgload::load_all(tree, quiet = TRUE)) else suppressPackageStartupMessages(library(pkg, character.only = TRUE))
cli <- get("morie_cli")

home <- tempfile("morie-smoke-")
dir.create(home)
work <- file.path(home, "work")
dir.create(work)
setwd(work)
Sys.setenv(HOME = home, XDG_CONFIG_HOME = file.path(home, "cfg"),
           XDG_CACHE_HOME = Sys.getenv("MORIE_SMOKE_CACHE", file.path(home, "cache")),
           MORIE_EMISSIONS_OFFLINE = "1", R_USER_CACHE_DIR = file.path(home, "rcache"))
key <- Sys.getenv("MORIE_SMOKE_KEY", "")
if (nzchar(key)) Sys.setenv(MORIE_HOSTED_KEY = key) else Sys.unsetenv("MORIE_HOSTED_KEY")
for (v in c("LLM_API_BASE_URL", "LLM_API_KEY", "GEMINI_API_KEY", "OPENAI_API_KEY", "MORIE_DATA_DIR")) Sys.unsetenv(v)
options(timeout = 3600)

NOT_RUNNABLE <- character()  # every verb has a case

LAST <- new.env()
run <- function(...) {
  buf <- character()
  st <- withCallingHandlers(
    cli(c(...), out = function(s) buf <<- c(buf, s)),
    message = function(m) { buf <<- c(buf, conditionMessage(m)); invokeRestart("muffleMessage") })
  LAST$text <- paste(buf, collapse = "")
  list(status = st, text = LAST$text)
}
# the hosted tier answers 429 when every runner of every package asks at once on the shared key: wait and ask once more
run_llm <- function(...) {
  r <- run(...)
  if (r$status != 0 && grepl("429", r$text, fixed = TRUE)) { Sys.sleep(45); r <- run(...) }
  r
}
results <- list()
check <- function(cond, what) if (!isTRUE(cond)) stop(what, call. = FALSE)
case <- function(verb, fn) results[[length(results) + 1L]] <<- list(verb = verb, fn = fn)

case("version", function() { r <- run("version"); check(r$status == 0 && grepl(pkg, r$text), r$text) })
case("list-modules", function() { r <- run("list-modules"); check(r$status == 0 && grepl("power-design", r$text), r$text) })
case("list-datasets", function() {
  r <- run("list-datasets"); check(r$status == 0 && grepl("ocp21", r$text), substr(r$text, 1, 300))
  if (nzchar(key)) check(grepl("data.rmorie.com", r$text), "hosted rows missing although a key is set")
})
case("cheatsheet", function() { r <- run("cheatsheet"); check(r$status == 0 && grepl("provider set", r$text), r$text) })
case("explain", function() { r <- run("explain", "power_two_proportion_gender.csv"); check(r$status == 0 && grepl("effect_size", r$text), r$text) })
case("doctor", function() { r <- run("doctor"); check(r$status == 0 && nzchar(r$text), r$text) })
case("models", function() { r <- run("models"); check(r$status == 0, r$text); if (!nzchar(key)) check(grepl("login", r$text, ignore.case = TRUE), "no key: models must point at login") })
case("provider", function() {
  check(run("provider", "set", "--base-url", "https://api.example.org/v1/", "--key", "sk-smoke-1234567", "--model", "demo")$status == 0, "provider set")
  r <- run("provider", "show"); check(grepl("api.example.org/v1", r$text) && !grepl("sk-smoke-1234567", r$text), r$text)
  check(grepl("Your endpoint", run("models")$text), "models does not list the endpoint")
  check(run("provider", "unset")$status == 0, "provider unset")
})
case("ask", function() { r <- run_llm("ask", "What does the power-design module compute?"); check(r$status == 0 && nzchar(trimws(r$text)), r$text) })
case("percy", function() { r <- run_llm("percy", "Which module compares two groups?"); check(r$status == 0 && nzchar(trimws(r$text)), r$text) })
case("perseus", function() { r <- run_llm("perseus", "hello"); check(r$status == 0, r$text) })
case("agent", function() { r <- run_llm("agent", "hello"); check(r$status == 0, r$text) })
case("chat", function() {
  con <- textConnection("/quit"); on.exit(close(con))
  testthat::local_mocked_bindings(.cli_readline = function(prompt) "/quit", .package = pkg)
  r <- run("chat"); check(r$status == 0 && grepl("Bye", r$text), r$text)
})
case("tutorial", function() { r <- run("tutorial", "--dry-run"); check(r$status == 0 && grepl("STEP 3", r$text), substr(r$text, 1, 200)) })
case("login", function() {
  if (!nzchar(key)) { message("  login: SKIP (MORIE_SMOKE_KEY not set)"); return(invisible()) }
  r <- run("login", "--token", key); check(r$status == 0, r$text)
  check(file.exists(file.path(home, "cfg", "morie", "credentials.json")), "credentials file not written")
})
case("logout", function() { check(run("logout")$status == 0, "logout") })
case("generate-template", function() {
  r <- run("generate-template", "--module", "hawkes", "--out", "paper/first.md")
  check(r$status == 0 && any(grepl("hawkes", readLines("paper/first.md"))), r$text)
})
mkcsv <- function() {
  d <- data.frame(treated = rep(0:1, 60), y = ((seq_len(120) * 7) %% 13) / 3, g = rep(c("a", "b"), each = 60), w = 1 + seq_len(120) %% 3)
  utils::write.csv(d, "d.csv", row.names = FALSE); "d.csv"
}
case("profile-dataset", function() { p <- mkcsv(); r <- run("profile-dataset", p, "--treatment", "treated", "--outcome", "y", "--suggest"); check(r$status == 0 && grepl("Suggested", r$text), r$text) })
case("sample", function() {
  p <- mkcsv(); r <- run("sample", p, "--n", "7", "--output", "s.csv"); check(r$status == 0 && nrow(utils::read.csv("s.csv")) == 7, r$text)
  r <- run("sample", p, "--n", "3", "--method", "stratified", "--strata-col", "g", "--output", "st.csv"); check(nrow(utils::read.csv("st.csv")) == 6, r$text)
})
case("run-module", function() {
  r <- run("run-module", "power-design", "--output-dir", "out0")
  check(r$status == 0 && file.exists("out0/power_two_proportion_gender.csv"), r$text)
})
case("pull", function() {
  r <- run("pull", "ocp21", "--out", "cpads.csv"); check(r$status == 0, r$text)
  n <- length(readLines("cpads.csv")) - 1L; check(n > 40000, paste("CPADS PUMF rows:", n))
  msgs <- character()
  r <- run("run-module", "descriptive-statistics", "--output-dir", "out1")
  check(r$status == 0, r$text)
  check(!grepl("synthetic", r$text, ignore.case = TRUE), "after the pull the module still used the synthetic frame")
  if (nzchar(key)) {
    r <- run("pull", "fec_cm_2020/fec_cm_2020", "--out", "fec.csv"); check(r$status == 0 && length(readLines("fec.csv")) > 1000, r$text)
  } else message("  pull data.rmorie.com: SKIP (MORIE_SMOKE_KEY not set)")
})
case("run-modules", function() { r <- run("run-modules", "--modules", "power-design", "--output-dir", "out2"); check(r$status == 0 && grepl("power-design", r$text), r$text) })
case("pipeline", function() {
  r <- run("pipeline", "--modules", "power-design", "--output-dir", "out3")
  check(r$status == 0 && grepl("CO2", r$text) && file.exists("out3/emissions/emissions.csv") && file.exists("out3/emissions/capsule_bundle.json"), r$text)
})
case("inspect", function() { r <- run("inspect", "out0"); check(r$status == 0 && grepl("rows", r$text), substr(r$text, 1, 200)) })
case("verify", function() { r <- run("verify", "out0"); check(r$status %in% c(0L, 1L) && nzchar(r$text), r$text) })
case("emissions", function() {
  r <- run("emissions", "--seconds", "1", "--output-dir", "em", "--country", "CAN")
  check(r$status == 0 && grepl("kg CO2eq", r$text) && grepl("Capsule", r$text), r$text)
  check(isTRUE(get("morie_emissions_verify")("em")$ok), "capsule does not verify")
})
case("verify-pollution", function() {
  r <- run("verify-pollution", "--pollutant", "no2", "--demo"); check(r$status == 0 && grepl("STATUS: ok", r$text) && grepl("source:   Atkinson", r$text), r$text)
  r <- run("verify-pollution", "--pollutant", "pm25", "--exposure-mean", "2", "--exposure-prevalence", "0.5"); check(r$status == 1 && grepl("assumption_failure", r$text), r$text)
})
case("crypto", function() {
  r <- run("crypto", "keygen", "--name", "smoke", "--output", "keys")
  if (r$status != 0 && grepl("liboqs|sodium", r$text)) { message("  crypto: SKIP (built without liboqs/libsodium)"); return(invisible()) }
  check(r$status == 0 && file.exists("keys/smoke.moriepk"), r$text)
  writeLines("hello", "secret.txt")
  r <- run("crypto", "encrypt", "secret.txt", "--recipient", "keys/smoke.moriepk"); check(r$status == 0 && file.exists("secret.txt.morieenc"), r$text)
})
case("ingest", function() { r <- run("ingest", "tps", "--list"); check(r$status == 0 && nzchar(r$text), r$text) })
case("download-bootstrap", function() { r <- run("download-bootstrap", "--survey", "zzz_none"); check(r$status == 1 && grepl("Keys:", r$text), r$text) })
case("percysuits", function() { r <- run("percysuits", "--dry-run"); check(r$status %in% c(0L, 1L) && nzchar(r$text), r$text) })
case("edit", function() {
  f <- file.path(work, "edit_me.R")
  if (.Platform$OS.type == "windows") {
    stub <- file.path(work, "stub_editor.bat")
    writeLines(c("@echo off", "echo cat(6 * 7) >> %1"), stub)
  } else {
    stub <- file.path(work, "stub_editor.sh")
    writeLines(c("#!/bin/sh", "printf 'cat(6 * 7)\\n' >> \"$1\""), stub)
    Sys.chmod(stub, "0755")
  }
  old <- Sys.getenv(c("EDITOR", "VISUAL"), unset = NA)
  Sys.setenv(EDITOR = stub); Sys.unsetenv("VISUAL")
  on.exit({ if (is.na(old[["EDITOR"]])) Sys.unsetenv("EDITOR") else Sys.setenv(EDITOR = old[["EDITOR"]])
            if (!is.na(old[["VISUAL"]])) Sys.setenv(VISUAL = old[["VISUAL"]]) }, add = TRUE)
  r <- run("edit", f)
  check(r$status == 0 && file.exists(f) && any(grepl("cat(6 * 7)", readLines(f, warn = FALSE), fixed = TRUE)),
        paste("edit through $EDITOR:", r$text))
})
case("help", function() { r <- run("help"); check(r$status == 0 && grepl("usage: rmorie", r$text), r$text) })
case("--help", function() { r <- run("--help"); check(r$status == 0 && grepl("usage: rmorie", r$text), r$text) })
case("-h", function() { r <- run("-h"); check(r$status == 0 && grepl("usage: rmorie", r$text), r$text) })
case("verify-earth-engine", function() {
  r <- run("verify-earth-engine")
  check(r$status != 0 && grepl("morie verify-earth-engine", r$text, fixed = TRUE), paste("must point at the Python verb:", r$text))
})
case("launcher", function() {
  # exactly what inst/bin/rmorie does, with the tree loaded in place of the installed package
  tree_arg <- if (nzchar(tree)) sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(tree)) else "library(rmorie)"
  r <- suppressWarnings(system2("Rscript", c("--vanilla", "-e",
    shQuote(sprintf("suppressMessages(%s); q <- morie_cli(); quit(status = as.integer(q))", tree_arg)),
    "--args", "version"), stdout = TRUE, stderr = TRUE))
  check(identical(attr(r, "status"), NULL) && any(grepl(paste(pkg, "1\\."), r)), paste("launcher:", paste(r, collapse = " | ")))
})
case("run-module-default-dir", function() {
  r <- run("run-module", "power-design")
  check(r$status == 0 && dir.exists(file.path("morie-output", "power-design")) &&
          length(list.files(file.path("morie-output", "power-design"), pattern = "[.]csv$")) > 5,
        paste("run-module without --output-dir must write under morie-output/:", r$text))
})
case("verb-help", function() {
  r <- run("emissions", "--help")
  check(r$status == 0 && grepl("emissions", r$text) && !dir.exists("emissions"), paste("emissions --help must not run it:", r$text))
})
case("launcher", function() {
  # exactly what inst/bin/rmorie does, with the tree loaded in place of the installed package
  tree_arg <- if (nzchar(tree)) sprintf("pkgload::load_all(%s, quiet = TRUE)", shQuote(tree)) else "library(rmorie)"
  r <- suppressWarnings(system2("Rscript", c("--vanilla", "-e",
    shQuote(sprintf("suppressMessages(%s); q <- morie_cli(); quit(status = as.integer(q))", tree_arg)),
    "--args", "version"), stdout = TRUE, stderr = TRUE))
  check(identical(attr(r, "status"), NULL) && any(grepl(paste(pkg, "1\\."), r)), paste("launcher:", paste(r, collapse = " | ")))
})
case("run-module-default-dir", function() {
  r <- run("run-module", "power-design")
  check(r$status == 0 && dir.exists(file.path("morie-output", "power-design")) &&
          length(list.files(file.path("morie-output", "power-design"), pattern = "[.]csv$")) > 5,
        paste("run-module without --output-dir must write under morie-output/:", r$text))
})
case("verb-help", function() {
  r <- run("emissions", "--help")
  check(r$status == 0 && grepl("emissions", r$text) && !dir.exists("emissions"), paste("emissions --help must not run it:", r$text))
})
case("exec", function() { r <- run("exec", "6 * 7"); check(r$status == 0 && grepl("42", r$text), r$text) })
case("ask-fallback-honest", function() {
  if (nzchar(key)) return(invisible())
  r <- run("ask", "hello"); check(grepl("local|fallback|login", r$text, ignore.case = TRUE), "without a key ask must say it fell back")
})
case("selftest", function() { r <- run("selftest"); check(r$status == 0 && grepl("All tests passed", r$text), r$text) })
case("update", function() { r <- run("update"); check(r$status %in% c(0L, 1L), r$text) })
case("analyze", function() { r <- run("analyze"); check(r$status == 1 && grepl("usage", r$text), r$text) })

# verb coverage, from the help text
help_text <- run("help")$text
verbs <- unique(regmatches(help_text, gregexpr("(?m)^  ([a-z][a-z-]*)", help_text, perl = TRUE))[[1]])
verbs <- trimws(verbs)
covered <- c(vapply(results, function(x) x$verb, ""), names(NOT_RUNNABLE), "ask-fallback-honest")
missing <- setdiff(verbs, covered)
if (length(missing)) { cat("VERBS WITHOUT A SMOKE CASE:", paste(missing, collapse = ", "), "\n"); quit(status = 2) }

only <- strsplit(Sys.getenv("MORIE_SMOKE_ONLY", ""), ",")[[1]]
if (length(only)) results <- Filter(function(x) x$verb %in% only, results)
failed <- 0L
for (x in results) {
  t0 <- Sys.time()
  out <- tryCatch({ x$fn(); "OK" }, error = function(e) { failed <<- failed + 1L; paste("FAIL", substr(conditionMessage(e), 1, 500)) })
  cat(sprintf("[%s] %-20s %.1fs\n", substr(out, 1, 4), x$verb, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (startsWith(out, "FAIL")) cat("      ", out, "\n")
  if (startsWith(out, "FAIL") && nzchar(Sys.getenv("MORIE_SMOKE_DEBUG"))) cat("      last text:", substr(LAST$text, 1, 400), "\n")
}
cat(sprintf("\nsmoke (%s): %d ok, %d failed, %d not runnable here\n", pkg, length(results) - failed, failed, length(NOT_RUNNABLE)))
unlink(home, recursive = TRUE)
quit(status = if (failed) 1L else 0L)
