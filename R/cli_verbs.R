# Helpers for the verbs of morie_cli() that mirror the Python command line:
# explain, inspect, verify, profile-dataset, sample, run-modules, pipeline,
# percy/agent, chat, selftest, tutorial, generate-template, update, crypto,
# ingest, download-bootstrap, exec, edit, percysuits, verify-pollution and
# emissions. Each takes the parsed `rest`, the `flag`/`has` accessors and the
# `out` sink of morie_cli() and returns an exit status.

.cli_readline <- function(prompt) {
  if (interactive()) return(trimws(readline(prompt)))
  cat(prompt)
  con <- file("stdin")
  on.exit(close(con))
  l <- tryCatch(readLines(con, n = 1L, warn = FALSE), error = function(e) character())
  if (!length(l)) return("q")
  trimws(l[[1L]])
}

.cli_strip_flag <- function(rest, name, with_value = TRUE) {
  i <- match(name, rest)
  if (is.na(i)) return(rest)
  rest[-(i + if (with_value) 0:1 else 0)]
}

.cli_csv_files <- function(target, module = NULL) {
  files <- list.files(target, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!is.null(module)) files <- files[grepl(module, files, fixed = TRUE)]
  sort(files)
}

.cli_inspect_text <- function(r) {
  lines <- c(sprintf("%s", r$path), sprintf("  format: %s  size: %s bytes", r$format, format(r$size_bytes)))
  if (!isTRUE(r$exists)) return(paste0(paste(c(lines, "  status: missing"), collapse = "\n"), "\n"))
  for (k in setdiff(names(r), c("path", "format", "exists", "size_bytes", "contents_preview"))) {
    v <- r[[k]]
    if (is.atomic(v) && length(v) <= 12L) lines <- c(lines, sprintf("  %s: %s", k, paste(format(v), collapse = ", ")))
  }
  pv <- r$contents_preview
  if (is.data.frame(pv)) {
    lines <- c(lines, sprintf("  rows: %d  columns: %d", nrow(pv), ncol(pv)),
               paste0("  ", utils::capture.output(print(utils::head(pv, 10L), row.names = FALSE))))
  } else if (!is.null(pv)) {
    lines <- c(lines, paste0("  ", utils::head(utils::capture.output(utils::str(pv)), 12L)))
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}

.cli_verify_text <- function(rep) {
  lines <- c(sprintf("%s: %s", rep$path, if (isTRUE(rep$passed)) "PASS" else "FAIL"))
  for (k in names(rep$checks)) {
    v <- rep$checks[[k]]
    lines <- c(lines, sprintf("  [%s] %s", if (isTRUE(v)) "ok" else "x", k))
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}

.cli_inspect <- function(rest, flag, out) {
  target <- rest[[1L]]
  if (dir.exists(target)) {
    files <- .cli_csv_files(target, flag("--module"))
    if (!length(files)) {
      out(sprintf("No CSV files found in %s\n", target))
      return(1L)
    }
    for (f in files) out(paste0(.cli_inspect_text(morie_inspect_output(f)), "\n"))
    return(0L)
  }
  if (!file.exists(target)) {
    out(sprintf("Path not found: %s\n", target))
    return(1L)
  }
  out(.cli_inspect_text(morie_inspect_output(target)))
  0L
}

.cli_verify <- function(rest, flag, out) {
  target <- rest[[1L]]
  files <- if (dir.exists(target)) .cli_csv_files(target, flag("--module")) else if (file.exists(target)) target else character()
  if (!length(files)) {
    out(sprintf("%s\n", if (dir.exists(target)) paste("No CSV files found in", target) else paste("Path not found:", target)))
    return(1L)
  }
  ok <- TRUE
  for (f in files) {
    rep <- morie_verify_statistical_output(f)
    out(.cli_verify_text(rep))
    if (!isTRUE(rep$passed)) ok <- FALSE
  }
  if (ok) 0L else 1L
}

.cli_profile_dataset <- function(rest, flag, has, out) {
  path <- flag("--csv") %||% (if (length(rest) && !startsWith(rest[[1L]], "--")) rest[[1L]] else NULL)
  if (is.null(path)) {
    out("usage: rmorie profile-dataset PATH [--treatment COL] [--outcome COL] [--weights COL] [--suggest]\n")
    return(2L)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  profile <- morie_dataset_profile(df, hint_treatment = flag("--treatment"),
                                   hint_outcome = flag("--outcome"), hint_weights = flag("--weights"))
  out(paste0(paste(morie_dataset_profile_summary_table(profile), collapse = "\n"), "\n"))
  if (!is.null(profile$suggested_treatment)) out(sprintf("\nSuggested treatment: %s\n", profile$suggested_treatment))
  if (!is.null(profile$suggested_outcome)) out(sprintf("Suggested outcome:   %s\n", profile$suggested_outcome))
  if (!is.null(profile$suggested_weights)) out(sprintf("Suggested weights:   %s\n", profile$suggested_weights))
  if (has("--suggest")) {
    plan <- morie_dataset_suggest_plan(profile)
    out("\n--- Suggested Analysis Plan ---\n")
    for (i in seq_along(plan)) out(sprintf("  %d. [%s] %s\n", i, plan[[i]]$analysis, plan[[i]]$rationale))
  }
  0L
}

.cli_sample <- function(rest, flag, has, out) {
  path <- flag("--csv") %||% (if (length(rest) && !startsWith(rest[[1L]], "--")) rest[[1L]] else NULL)
  n <- flag("--n")
  if (is.null(path) || is.null(n)) {
    out("usage: rmorie sample PATH --n N [--method srs|stratified|cluster|pps] [--strata-col COL] [--cluster-col COL] [--size-col COL] [--proportional] [--seed 42] [--output FILE]\n")
    return(2L)
  }
  n <- as.integer(n)
  seed <- as.integer(flag("--seed") %||% "42")
  method <- flag("--method") %||% "srs"
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  s <- switch(method,
    srs = morie_simple_random_sample(df, n, seed = seed),
    stratified = {
      col <- flag("--strata-col")
      if (is.null(col)) {
        out("Error: --strata-col is required for stratified sampling\n")
        return(1L)
      }
      morie_stratified_sample(df, col, n, proportional = has("--proportional"), seed = seed)
    },
    cluster = {
      col <- flag("--cluster-col")
      if (is.null(col)) {
        out("Error: --cluster-col is required for cluster sampling\n")
        return(1L)
      }
      morie_cluster_sample(df, col, n, seed = seed)
    },
    pps = {
      col <- flag("--size-col")
      if (is.null(col)) {
        out("Error: --size-col is required for PPS sampling\n")
        return(1L)
      }
      morie_pps_sample(df, col, n, seed = seed)
    },
    {
      out(sprintf("Unknown method: %s\n", method))
      return(1L)
    })
  out(sprintf("Sampled %d rows using %s\n", nrow(s), method))
  dest <- flag("--output")
  if (!is.null(dest)) {
    utils::write.csv(s, dest, row.names = FALSE)
    out(sprintf("Wrote sample to %s\n", dest))
  } else {
    out(paste0(paste(utils::capture.output(print(utils::head(s, 20L), row.names = FALSE)), collapse = "\n"), "\n"))
  }
  0L
}

.cli_run_modules <- function(rest, flag, has, out, pipeline = FALSE) {
  mods <- flag("--modules")
  if (pipeline && !has("--all") && is.null(mods)) {
    out("usage: rmorie pipeline (--all | --modules a,b,...) [--cpads FILE] [--output-dir DIR] [--no-carbon]\n")
    return(0L)
  }
  selected <- if (is.null(mods)) morie_module_names() else trimws(strsplit(mods, ",")[[1L]])
  od <- flag("--output-dir")
  cp <- flag("--cpads") %||% flag("--cpads-csv")
  run <- function() {
    if (is.null(cp)) morie_run_morie_modules(selected, output_dir = od)
    else morie_run_morie_modules(selected, cpads_csv = cp, output_dir = od)
  }
  if (pipeline && !has("--no-carbon")) {
    edir <- file.path(od %||% file.path("data", "manifest", "outputs"), "emissions")
    r <- morie_emissions_track(run(), project_name = "morie-pipeline", output_dir = edir)
    res <- r$value
    out(sprintf("Pipeline CO2 emissions: %.6f kg CO2eq  (%s)\n", r$emissions_kg, file.path(edir, "emissions.csv")))
    if (!is.null(r$capsule)) out(sprintf("Capsule: %s%s\n", r$capsule$manifest, if (isTRUE(r$capsule$signed)) " (signed)" else ""))
  } else {
    res <- run()
  }
  out(sprintf("Completed modules: %s\n", paste(names(res), collapse = ", ")))
  0L
}

.cli_percy <- function(rest, flag, out, verb) {
  mdl <- flag("--model")
  ctx <- flag("--context")
  rest <- .cli_strip_flag(.cli_strip_flag(rest, "--model"), "--context")
  rest <- rest[!rest %in% c("--no-stream", "--remote", "--local", "--pi", "--cloud")]
  if (!length(rest) || identical(rest[[1L]], "--help")) {
    out(sprintf("usage: rmorie %s [--model NAME] [--context TEXT] QUESTION...\n", verb))
    return(2L)
  }
  out(paste0(morie_llm_ask(morie_build_prompt(paste(rest, collapse = " "), context = ctx), model = mdl), "\n"))
  0L
}

.cli_chat <- function(rest, flag, out) {
  mdl <- flag("--model")
  out(sprintf("rmorie chat (%s). Type a question; /quit to leave.\n", morie_llm_detect_provider()))
  history <- character()
  repeat {
    q <- .cli_readline("you> ")
    if (!nzchar(q) || q %in% c("/quit", "/exit", "/q", "q", "quit", "exit")) break
    ctx <- if (length(history)) paste(utils::tail(history, 6L), collapse = "\n") else NULL
    a <- morie_llm_ask(q, context = ctx, model = mdl)
    history <- c(history, paste0("User: ", q), paste0("Assistant: ", a))
    out(paste0("percy> ", a, "\n"))
  }
  out("Bye!\n")
  0L
}

.cli_selftest <- function(out) {
  results <- list()
  check <- function(name, fn) {
    t0 <- proc.time()[["elapsed"]]
    r <- tryCatch({
      v <- fn()
      if (identical(v, "skip")) list(ok = NA, detail = "skipped: not available in this build")
      else list(ok = isTRUE(v), detail = "")
    }, error = function(e) list(ok = FALSE, detail = conditionMessage(e)))
    results[[length(results) + 1L]] <<- list(name = name, ok = r$ok, detail = r$detail,
                                             secs = proc.time()[["elapsed"]] - t0)
  }
  check("package loads", function() is.character(as.character(utils::packageVersion(utils::packageName()))))
  check("module registry (23 modules)", function() length(morie_module_names()) >= 20L)
  check("dataset catalog", function() nrow(morie_list_datasets()) > 0L)
  check("explain registry", function() nzchar(explain_file("power_two_proportion_gender.csv")))
  check("llm provider detection", function() morie_llm_detect_provider() %in% c("ollama", "hosted", "gemini", "api", "openai", "local"))
  check("statistics: two-sample t", function() {
    r <- morie_two_sample_t_test(c(1, 2, 3, 4, 5), c(2, 3, 4, 5, 6))
    is.list(r) && is.numeric(unlist(r)[1L])
  })
  check("sampling: SRS", function() nrow(morie_simple_random_sample(data.frame(x = 1:50), 5L)) == 5L)
  check("crypto: hybrid round trip", function() {
    if (!isTRUE(tryCatch(morie_crypto_liboqs_available(), error = function(e) FALSE)) ||
        !isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE))) return("skip")
    k <- morie_crypto_hybrid_keygen()
    ct <- morie_crypto_hybrid_encrypt(charToRaw("selftest"), k$pk)
    identical(rawToChar(morie_crypto_hybrid_decrypt(ct, k$sk)), "selftest")
  })
  check("envhealth: PM2.5 pipeline", function() {
    r <- morie_verify_pollution("pm25", exposure_mean = 12, exposure_prevalence = 0.9)
    identical(r$status, "ok") && r$pipeline$paf > 0
  })
  check("emissions tracker", function() {
    d <- tempfile()
    r <- morie_emissions_track(sum(sqrt(1:1e4)), output_dir = d, country_iso_code = "CAN")
    file.exists(file.path(d, "emissions.csv")) && r$emissions_kg > 0
  })
  check("module run: power-design", function() {
    r <- morie_run_morie_module("power-design", output_dir = tempfile())
    is.list(r) && length(r) > 0L
  })
  check("inspector + verify on a module table", function() {
    d <- tempfile()
    morie_run_morie_module("power-design", output_dir = d)
    f <- list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)[1L]
    isTRUE(morie_inspect_output(f)$exists)
  })
  out("MORIE Self-Test (R)\n")
  out(paste0(strrep("=", 60), "\n"))
  for (r in results) {
    tag <- if (is.na(r$ok)) "SKIP" else if (r$ok) " OK " else "FAIL"
    out(sprintf("  [%s] %-40s %5.1fs %s\n", tag, r$name, r$secs, substr(r$detail, 1L, 60L)))
  }
  oks <- vapply(results, function(r) r$ok, logical(1))
  failed <- sum(!oks, na.rm = TRUE)
  out(sprintf("\n%s: %d OK, %d failed, %d skipped\n", if (failed) "FAILED" else "All tests passed",
              sum(oks, na.rm = TRUE), failed, sum(is.na(oks))))
  if (failed) 1L else 0L
}

.cli_tutorial <- function(has, out) {
  dry <- has("--dry-run")
  out_root <- file.path(path.expand("~"), sprintf("rmorie-tutorial-%s", format(Sys.Date())))
  out(sprintf(paste0(
    "\nWelcome to the rmorie tutorial.\n\nThis walks you through one full analysis from a clean install to\n",
    "interpretable output. Every step is a real command, and we run each one\nfor you. Output lands in:\n\n    %s\n\n",
    "Press Enter to continue, s to skip a step, q to quit. If you have used\nmorie before, `rmorie cheatsheet` is the one-page reference.\n"), out_root))
  steps <- list(
    list("What does morie know how to do?",
         "morie ships 23 analysis modules. Each has a short description and a list of output files.",
         c("list-modules")),
    list("Is everything healthy?",
         "`rmorie doctor` reports which language-model routes answer from this machine; `rmorie selftest` exercises the subsystems.",
         c("doctor")),
    list("Run a real analysis on the bundled synthetic dataset",
         sprintf("The `power-design` module computes how many participants you need to detect a given effect. Output lands in %s. You will see a synthetic-data note: the bundled CPADS frame is a toy file; add `--cpads /path/to/real.csv` when you have the PUMF.", file.path(out_root, "power-design")),
         c("run-module", "power-design", "--output-dir", file.path(out_root, "power-design"))),
    list("What did we just produce?",
         "The table that answers \"how many participants do I need?\" is power_two_proportion_gender.csv. `rmorie explain` says how to read any output file.",
         c("explain", "power_two_proportion_gender.csv")),
    list("Pull real data in one line",
         "`rmorie list-datasets` shows the keys; `rmorie pull KEY` writes one as CSV.",
         c("list-datasets")),
    list("What to do next",
         paste0("  Run any module:        rmorie run-module NAME\n  Pull a dataset:        rmorie pull KEY\n",
                "  Ask for help:          rmorie ask \"...\"   (rmorie login first for the hosted tier)\n",
                "  One-page reference:    rmorie cheatsheet\n  Issues:                https://github.com/rootcoder007/rmorie/issues\n\nWelcome aboard."),
         NULL))
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    out(sprintf("\n%s\nSTEP %d  --  %s\n%s\n%s\n\n", strrep("=", 70), i, s[[1L]], strrep("=", 70), s[[2L]]))
    if (is.null(s[[3L]])) next
    cmd <- paste("rmorie", paste(s[[3L]], collapse = " "))
    if (dry) {
      out(sprintf("  $ %s\n", cmd))
      next
    }
    ans <- tolower(.cli_readline("[Enter] continue / [s] skip / [q] quit: "))
    if (ans == "q") return(0L)
    if (ans == "s") next
    out(sprintf("  $ %s\n\n", cmd))
    morie_cli(s[[3L]], out = out)
  }
  0L
}

.cli_generate_template <- function(flag, out) {
  module <- flag("--module") %||% "power-design"
  dest <- flag("--out") %||% "first-paper.md"
  src <- system.file("templates", "first-paper.md", package = utils::packageName())
  if (!nzchar(src)) stop("the first-paper template is missing from this installation", call. = FALSE)
  txt <- gsub("[MODULE_NAME]", module, paste(readLines(src, warn = FALSE), collapse = "\n"), fixed = TRUE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, dest)
  out(sprintf("wrote %s  (%s chars)\n", dest, format(nchar(txt), big.mark = ",")))
  0L
}

.cli_latest_version <- function(pkg) {
  if (identical(pkg, "rmorie")) {
    url <- "https://rootcoder007.r-universe.dev/api/packages/rmorie"
    txt <- paste(readLines(url, warn = FALSE), collapse = "")
    v <- regmatches(txt, regexpr("\"Version\"\\s*:\\s*\"[^\"]+\"", txt))
    return(list(version = sub(".*\"([^\"]+)\"$", "\\1", v), source = "r-universe"))
  }
  url <- "https://raw.githubusercontent.com/rootcoder007/morie/main/r-package/morie/DESCRIPTION"
  l <- grep("^Version:", readLines(url, warn = FALSE), value = TRUE)
  list(version = trimws(sub("^Version:", "", l[1L])), source = "GitHub main")
}

.cli_update <- function(has, out) {
  pkg <- utils::packageName()
  cur <- utils::packageVersion(pkg)
  latest <- tryCatch(.cli_latest_version(pkg), error = function(e) NULL)
  if (is.null(latest)) {
    out("Could not reach the package index (offline?).\n")
    return(1L)
  }
  newer <- utils::compareVersion(latest$version, as.character(cur)) > 0
  out(sprintf("Installed: %s %s\nLatest:    %s (%s)\n", pkg, cur, latest$version, latest$source))
  if (!newer) {
    out("You are up to date.\n")
    return(0L)
  }
  cmd <- if (identical(pkg, "rmorie")) {
    "install.packages(\"rmorie\", repos = c(\"https://rootcoder007.r-universe.dev\", \"https://cloud.r-project.org\"))"
  } else {
    "remotes::install_github(\"rootcoder007/morie\", subdir = \"r-package/morie\")"
  }
  if (!has("-y") && !has("--yes")) {
    out(sprintf("Update with:  Rscript -e '%s'   (or: rmorie update --yes)\n", cmd))
    return(0L)
  }
  out("Installing...\n")
  eval(parse(text = cmd))
  0L
}

.cli_keystore_path <- function() {
  file.path(path.expand("~"), ".morie", "keys", "keystore.json")
}

.cli_keystore_password <- function() {
  pw <- Sys.getenv("MORIE_KEYSTORE_PASSWORD", "")
  if (nzchar(pw)) return(pw)
  .cli_readline("Keystore password: ")
}

.cli_crypto <- function(rest, flag, out) {
  sub <- if (length(rest)) rest[[1L]] else ""
  usage <- paste0("usage: rmorie crypto keygen [--name NAME] [--output DIR]\n",
                  "       rmorie crypto encrypt FILE --recipient PKFILE|KEYNAME\n",
                  "       rmorie crypto decrypt FILE --key KEYNAME\n",
                  "Keys live in ~/.morie/keys/keystore.json (password: prompt or MORIE_KEYSTORE_PASSWORD).\n")
  if (identical(sub, "keygen")) {
    name <- flag("--name") %||% "default"
    k <- morie_crypto_hybrid_keygen()
    od <- flag("--output")
    if (!is.null(od)) {
      dir.create(od, recursive = TRUE, showWarnings = FALSE)
      writeBin(k$pk, file.path(od, paste0(name, ".moriepk")))
      writeBin(k$sk, file.path(od, paste0(name, ".moriesk")))
      out(sprintf("Public key:  %s\nSecret key:  %s\n", file.path(od, paste0(name, ".moriepk")),
                  file.path(od, paste0(name, ".moriesk"))))
      return(0L)
    }
    ks <- .cli_keystore_path()
    pw <- .cli_keystore_password()
    if (!file.exists(ks)) {
      dir.create(dirname(ks), recursive = TRUE, showWarnings = FALSE)
      morie_crypto_keystore_create(pw, path = ks)
      out(sprintf("Created keystore at %s\n", ks))
    }
    morie_crypto_keystore_store(name, k$pk, k$sk, pw, path = ks)
    out(sprintf("Key pair '%s' stored in keystore\n", name))
    return(0L)
  }
  if (identical(sub, "encrypt")) {
    f <- if (length(rest) > 1L) rest[[2L]] else NULL
    rcpt <- flag("--recipient")
    if (is.null(f) || is.null(rcpt)) {
      out(usage)
      return(2L)
    }
    if (!file.exists(f)) {
      out(sprintf("File not found: %s\n", f))
      return(1L)
    }
    pk <- if (file.exists(rcpt)) readBin(rcpt, "raw", file.info(rcpt)$size) else {
      morie_crypto_keystore_load(rcpt, .cli_keystore_password(), path = .cli_keystore_path())$pk
    }
    ct <- morie_crypto_hybrid_encrypt(readBin(f, "raw", file.info(f)$size), pk)
    dest <- paste0(f, ".morieenc")
    writeBin(ct, dest)
    out(sprintf("Encrypted: %s\n", dest))
    return(0L)
  }
  if (identical(sub, "decrypt")) {
    f <- if (length(rest) > 1L) rest[[2L]] else NULL
    kn <- flag("--key")
    if (is.null(f) || is.null(kn)) {
      out(usage)
      return(2L)
    }
    if (!file.exists(f)) {
      out(sprintf("File not found: %s\n", f))
      return(1L)
    }
    sk <- morie_crypto_keystore_load(kn, .cli_keystore_password(), path = .cli_keystore_path())$sk
    pt <- morie_crypto_hybrid_decrypt(readBin(f, "raw", file.info(f)$size), sk)
    dest <- if (endsWith(f, ".morieenc")) sub("\\.morieenc$", "", f) else paste0(f, ".dec")
    writeBin(pt, dest)
    out(sprintf("Decrypted: %s\n", dest))
    return(0L)
  }
  out(usage)
  2L
}

.cli_ingest <- function(rest, flag, has, out) {
  portal <- if (length(rest)) rest[[1L]] else ""
  usage <- paste0(
    "usage: rmorie ingest ckan --portal URL (--search TEXT [--rows N] | --package ID [--out DIR])\n",
    "       rmorie ingest tps (--list | --layer NAME [--year Y] [--where CLAUSE] [--max N] [--geometry] [--out FILE])\n",
    "       rmorie ingest siu (--list | --report-id ID --out DIR)\n",
    "       rmorie ingest a2aj (coverage [--doc-type cases|laws] | search QUERY [--size N] [--doc-type T] | fetch CITATION [--doc-type T])\n")
  print_df <- function(d) out(paste0(paste(utils::capture.output(print(d, row.names = FALSE)), collapse = "\n"), "\n"))
  if (identical(portal, "ckan")) {
    p <- flag("--portal")
    if (is.null(p)) {
      out(usage)
      return(2L)
    }
    if (!is.null(flag("--search"))) {
      print_df(morie_ingest_ckan_search_packages(p, flag("--search"), rows = as.integer(flag("--rows") %||% "20")))
      return(0L)
    }
    if (!is.null(flag("--package"))) {
      res <- morie_ingest_ckan_fetch_package_csvs(p, flag("--package"))
      od <- flag("--out") %||% "ckan-out"
      dir.create(od, recursive = TRUE, showWarnings = FALSE)
      for (nm in names(res)) {
        if (is.data.frame(res[[nm]])) {
          f <- file.path(od, paste0(gsub("[^A-Za-z0-9_.-]", "_", nm), ".csv"))
          utils::write.csv(res[[nm]], f, row.names = FALSE)
          out(sprintf("wrote %s (%d rows)\n", f, nrow(res[[nm]])))
        }
      }
      return(0L)
    }
    out(usage)
    return(2L)
  }
  if (identical(portal, "tps")) {
    if (has("--list") || is.null(flag("--layer"))) {
      print_df(morie_ingest_tps_layers())
      return(0L)
    }
    yr <- flag("--year")
    mx <- flag("--max")
    df <- morie_ingest_tps_fetch(flag("--layer"), year = if (is.null(yr)) NULL else as.integer(yr),
                                 where = flag("--where"), return_geometry = has("--geometry"),
                                 max_features = if (is.null(mx)) NULL else as.integer(mx))
    dest <- flag("--out")
    if (is.null(dest)) print_df(utils::head(df, 50L)) else {
      utils::write.csv(df, dest, row.names = FALSE)
      out(sprintf("wrote %s (%d rows)\n", dest, nrow(df)))
    }
    return(0L)
  }
  if (identical(portal, "siu")) {
    if (has("--list") || is.null(flag("--report-id"))) {
      print_df(utils::head(morie_siu_index(), 50L))
      return(0L)
    }
    od <- flag("--out") %||% "siu-out"
    dir.create(od, recursive = TRUE, showWarnings = FALSE)
    html <- morie_siu_fetch_report(flag("--report-id"))
    f <- file.path(od, paste0(flag("--report-id"), ".html"))
    writeLines(as.character(html), f)
    out(sprintf("wrote %s\n", f))
    return(0L)
  }
  if (identical(portal, "a2aj")) {
    action <- if (length(rest) > 1L) rest[[2L]] else "coverage"
    dt <- flag("--doc-type") %||% "cases"
    if (identical(action, "coverage")) {
      print_df(morie_ingest_a2aj_coverage(doc_type = dt))
      return(0L)
    }
    if (identical(action, "search") && length(rest) > 2L) {
      print_df(morie_ingest_a2aj_search(rest[[3L]], doc_type = dt, size = as.integer(flag("--size") %||% "10")))
      return(0L)
    }
    if (identical(action, "fetch") && length(rest) > 2L) {
      r <- morie_ingest_a2aj_fetch(rest[[3L]], doc_type = dt)
      out(paste0(paste(utils::capture.output(utils::str(r, max.level = 1L)), collapse = "\n"), "\n"))
      return(0L)
    }
    out(usage)
    return(2L)
  }
  out(sprintf("unknown portal '%s'; valid: ckan, tps, siu, a2aj\n", portal))
  2L
}

.cli_download_bootstrap <- function(flag, out) {
  survey <- flag("--survey") %||% "all"
  cat_ <- morie_dataset_catalog()
  boot <- cat_[cat_$type == "bootstrap", , drop = FALSE]
  if (!identical(survey, "all")) {
    boot <- boot[grepl(sub("_.*$", "", survey), boot$survey, fixed = TRUE) &
                   grepl(sub("^[a-z]+_", "", survey), boot$year, fixed = TRUE), , drop = FALSE]
  }
  if (!nrow(boot)) {
    out(sprintf("No bootstrap files match '%s'. Keys: %s\n", survey,
                paste(cat_$key[cat_$type == "bootstrap"], collapse = ", ")))
    return(1L)
  }
  for (i in seq_len(nrow(boot))) {
    out(sprintf("  Downloading %s (%s)...\n", boot$key[i], boot$name[i]))
    r <- tryCatch(morie_load_dataset(boot$key[i]), error = function(e) e)
    if (inherits(r, "error")) out(sprintf("    ERROR: %s\n", conditionMessage(r)))
    else out(sprintf("    OK: %s rows cached\n", format(nrow(r), big.mark = ",")))
  }
  0L
}

.cli_exec <- function(rest, flag, out) {
  f <- flag("--file")
  code <- if (!is.null(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else paste(rest[!rest %in% c("--file", f)], collapse = " ")
  if (!nzchar(trimws(code))) {
    out("usage: rmorie exec 'R CODE' | rmorie exec --file script.R\n")
    return(2L)
  }
  env <- new.env(parent = globalenv())
  res <- withVisible(eval(parse(text = code), envir = env))
  if (res$visible) out(paste0(paste(utils::capture.output(print(res$value)), collapse = "\n"), "\n"))
  0L
}

.cli_edit <- function(rest, out) {
  if (!length(rest)) {
    out("usage: rmorie edit FILE\n")
    return(2L)
  }
  f <- rest[[1L]]
  if (!file.exists(f)) file.create(f)
  if (interactive()) {
    utils::file.edit(f)
    return(0L)
  }
  ed <- Sys.getenv("VISUAL", Sys.getenv("EDITOR", ""))
  if (!nzchar(ed)) {
    out(sprintf("No editor: set EDITOR, or open %s in RStudio / VS Code.\n", f))
    return(1L)
  }
  system2(ed, shQuote(f))
  0L
}

.cli_percy_models <- function() {
  c("functiongemma:270m", "gemma4:e2b", "gemma3:4b", "qwen3.5:4b", "deepseek-r1:8b",
    "mistral-nemo", "phi4-mini", "llama3.2:3b", "nomic-embed-text")
}

.cli_percysuits <- function(flag, has, out) {
  host <- flag("--host") %||% .morie_llm_ollama_base()
  if (!morie_llm_probe_ollama()) {
    out(sprintf("Ollama is not reachable at %s. Install it from https://ollama.com and run `ollama serve`.\n", host))
    return(1L)
  }
  have <- morie_llm_ollama_models()$name
  want <- .cli_percy_models()
  missing <- want[!vapply(want, function(m) any(startsWith(have, sub(":.*$", "", m))), logical(1))]
  out(sprintf("Installed: %d/%d | Missing: %d\n", length(want) - length(missing), length(want), length(missing)))
  if (!length(missing)) return(0L)
  if (has("--dry-run")) {
    out(paste0("Would pull: ", paste(missing, collapse = ", "), "\n"))
    return(0L)
  }
  for (m in missing) {
    out(sprintf("  pulling %s ...\n", m))
    rc <- system2("ollama", c("pull", m))
    if (!identical(rc, 0L)) out(sprintf("  failed: %s\n", m))
  }
  0L
}

.cli_verify_pollution <- function(rest, flag, has, out) {
  pol <- flag("--pollutant")
  if (is.null(pol)) {
    out("usage: rmorie verify-pollution --pollutant no2|pm25 [--demo | --exposure-csv FILE | --exposure-mean X --exposure-prevalence P] [--outcome NAME] [--reference 5.8] [--baseline-rate 500] [--population 1000000] [--region R] [--years Y] [--json]\n")
    return(2L)
  }
  r <- morie_verify_pollution(
    pol, outcome = flag("--outcome") %||% "all_cause_mortality", region = flag("--region"),
    years = flag("--years"), demo = has("--demo"), exposure_csv = flag("--exposure-csv"),
    exposure_mean = as.numeric(flag("--exposure-mean") %||% "0"),
    exposure_prevalence = as.numeric(flag("--exposure-prevalence") %||% "0"),
    reference = as.numeric(flag("--reference") %||% "5.8"),
    baseline_rate = as.numeric(flag("--baseline-rate") %||% "500"),
    population = as.numeric(flag("--population") %||% "1000000"))
  if (has("--json")) {
    out(paste0(.morie_to_json(unclass(r), pretty = TRUE, auto_unbox = TRUE, null = "null"), "\n"))
  } else {
    out(.envhealth_report_text(r))
  }
  attr(r, "exit_status")
}

.cli_emissions <- function(flag, has, out) {
  secs <- as.numeric(flag("--seconds") %||% "3")
  od <- flag("--output-dir") %||% "emissions"
  t <- morie_emissions_start(project_name = "morie-emissions-check", output_dir = od,
                             capsule = !has("--no-capsule"),
                             country_iso_code = flag("--country") %||% "")
  t0 <- Sys.time()
  x <- 0
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) x <- x + sum(sqrt(seq_len(20000)))
  e <- morie_emissions_stop(t)
  out(.emissions_text(e))
  0L
}
