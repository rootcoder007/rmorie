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
  if (!length(l)) return(NA_character_)  # stdin closed: the caller says so instead of guessing
  trimws(l[[1L]])
}

.cli_strip_flag <- function(rest, name, with_value = TRUE) {
  i <- match(name, rest)
  if (is.na(i)) return(rest)
  rest[-(i + if (with_value) 0:1 else 0)]
}

.cli_csv_files <- function(target, module = NULL, out = NULL) {
  files <- sort(list.files(target, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE))
  if (!is.null(module) && length(files)) {
    # module names are hyphenated ("power-design"); the tables they write are not ("power_summary.csv"):
    # match on letters and digits only, the whole name or its first word, in the path or the file name
    norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    key <- norm(module)
    stem <- norm(sub("[-_].*$", "", module))
    hit <- grepl(key, norm(files), fixed = TRUE) | (nzchar(stem) & grepl(stem, norm(basename(files)), fixed = TRUE))
    if (any(hit)) {
      files <- files[hit]
    } else if (!is.null(out)) {
      out(sprintf("no table in %s names the module %s; using all %d tables\n", target, module, length(files)))
    }
  }
  files
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

# where a catalog entry comes from, in the words `morie list-datasets` uses
.cli_dataset_route <- function(e) {
  has <- function(col) col %in% names(e) && !is.na(e[[col]]) && nzchar(e[[col]])
  if (has("rmoriedata")) return("rmoriedata (CRAN)")
  if (has("arcgis_url")) return("Toronto Police ArcGIS")
  if (identical(e$source, "statcan")) return("Statistics Canada")
  if (has("fetcher")) return(if (identical(e$source, "naps")) "ECCC NAPS" else e$fetcher)
  if (has("download_url")) {
    host <- sub("^https?://([^/]+).*$", "\\1", e$download_url)
    return(paste0(host, if (has("hosted_key")) " (or data.rmorie.com)" else ""))
  }
  if (has("hosted_key")) return("data.rmorie.com (your MORIE key)")
  if (has("hosted_file")) return("data.rmorie.com file (an R object: rmorie loads it, morie saves it)")
  if (has("ckan_resource_id")) return(if (identical(e$source, "otis")) "data.ontario.ca" else "open.canada.ca")
  paste0("own file: ", e$local_path)
}

.cli_list_datasets <- function(out) {
  d <- morie_list_datasets()
  cat_df <- morie_dataset_catalog()
  route <- vapply(seq_len(nrow(d)), function(i) {
    if (identical(d$type[i], "hosted")) return("data.rmorie.com (your MORIE key)")
    .cli_dataset_route(cat_df[cat_df$key == d$key[i], , drop = FALSE][1L, ])
  }, character(1L))
  shown <- ifelse(d$cached & !is.na(d$rows), format(d$rows, big.mark = ",", trim = TRUE), "not cached")
  out(paste0(sprintf("%-20s %-12s %10s  %s", d$key, d$type, shown, route), collapse = "\n"))
  out("\n")
  n_hub <- sum(d$type == "hosted")
  n_cat <- nrow(d) - n_hub
  n_own <- sum(startsWith(route, "own file"))
  out(strrep("-", 96L))
  out("\n")
  out(sprintf("%d keys: %d download from their portal, rmoriedata or data.rmorie.com on first use; %d %s your own research file%s, placed under $MORIE_DATA_DIR/datasets/ with the path%s shown.\n",
              n_cat, n_cat - n_own, n_own, if (n_own == 1L) "is" else "are", if (n_own == 1L) "" else "s", if (n_own == 1L) "" else "s"))
  if (n_hub) {
    out(sprintf("%d curated tables at data.rmorie.com (db/table keys), opened by your MORIE key: rmorie pull KEY\n", n_hub))
  } else {
    out("Curated tables at data.rmorie.com appear here after `rmorie login` (GitHub) or `rmorie login --email you@example.com` (they need the MORIE key).\n")
  }
  0L
}

.cli_inspect <- function(rest, flag, out) {
  if (!length(rest)) {
    out("usage: rmorie inspect PATH [--module NAME]   (a CSV, or a directory of module outputs)\n")
    return(2L)
  }
  target <- rest[[1L]]
  if (dir.exists(target)) {
    files <- .cli_csv_files(target, flag("--module"), out)
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
  if (!length(rest)) {
    out("usage: rmorie verify PATH [--module NAME]   (a CSV, or a directory of module outputs)\n")
    return(2L)
  }
  target <- rest[[1L]]
  files <- if (dir.exists(target)) .cli_csv_files(target, flag("--module"), out) else if (file.exists(target)) target else character()
  if (!length(files)) {
    out(sprintf("%s\n", if (dir.exists(target)) paste("No CSV files found in", target) else paste("Path not found:", target)))
    return(1L)
  }
  ok <- TRUE
  for (f in files) {
    rep <- if (grepl("\\.json$", f, ignore.case = TRUE)) morie_verify_statistical_output(f) else .cli_verify_csv(f)
    out(.cli_verify_text(rep))
    if (!isTRUE(rep$passed)) ok <- FALSE
  }
  if (ok) 0L else 1L
}

# A module table verified as a CSV: parses, has rows and columns, no column that is entirely NA,
# no infinite numbers, the header is unique. Shaped like morie_verify_statistical_output()'s report.
.cli_verify_csv <- function(path) {
  checks <- list()
  df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  checks$csv_parses <- !is.null(df)
  if (is.null(df)) return(list(path = path, passed = FALSE, checks = checks))
  checks$has_rows <- nrow(df) > 0L
  checks$has_columns <- ncol(df) > 0L
  checks$unique_header <- !anyDuplicated(names(df))
  num <- vapply(df, is.numeric, TRUE)
  checks$no_infinite <- !any(vapply(df[num], function(col) any(is.infinite(col)), TRUE))
  # a column of blank cells (a text field with nothing to say, e.g. missing_formula_inputs) is not empty;
  # only a column whose every cell is the NA token is
  raw <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character"),
                  error = function(e) NULL)
  checks$no_empty_column <- nrow(df) == 0L || is.null(raw) ||
    !any(vapply(raw, function(col) all(is.na(col) | col == "NA"), TRUE))
  list(path = path, passed = all(unlist(checks)), checks = checks, rows = nrow(df), cols = ncol(df))
}

.cli_profile_dataset <- function(rest, flag, has, out) {
  path <- flag("--csv") %||% (if (length(rest) && !startsWith(rest[[1L]], "--")) rest[[1L]] else NULL)
  if (is.null(path)) {
    out("usage: rmorie profile-dataset PATH [--treatment COL] [--outcome COL] [--weights COL] [--suggest]\n")
    return(2L)
  }
  if (!file.exists(path)) {
    out(sprintf("File not found: %s\n", path))
    return(1L)
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
    out("usage: rmorie sample PATH --n N [--method srs|stratified|cluster|pps] [--strata-col COL [--per-stratum]] [--cluster-col COL] [--size-col COL] [--seed 42] [--output FILE] [--no-weight]\n")
    return(2L)
  }
  if (!grepl("^[0-9]+$", n) || as.integer(n) < 1L) {
    out(sprintf("--n must be a whole number of rows, not '%s'\n", n))
    return(2L)
  }
  n <- as.integer(n)
  seed <- as.integer(flag("--seed") %||% "42")
  method <- flag("--method") %||% "srs"
  if (!file.exists(path)) {
    out(sprintf("File not found: %s\n", path))
    return(1L)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (n > nrow(df) && method %in% c("srs", "stratified")) {
    out(sprintf("--n %d exceeds the %d rows in %s\n", n, nrow(df), path))
    return(1L)
  }
  s <- switch(method,
    srs = morie_simple_random_sample(df, n, seed = seed),
    stratified = {
      col <- flag("--strata-col")
      if (is.null(col)) {
        out("Error: --strata-col is required for stratified sampling\n")
        return(1L)
      }
      if (!col %in% names(df)) {
        out(sprintf("column '%s' is not in %s (columns: %s)\n", col, path, paste(head(names(df), 12), collapse = ", ")))
        return(1L)
      }
      # --n is the total sample size, allocated across the strata in proportion to their sizes;
      # --per-stratum draws --n rows from every stratum instead
      if (has("--per-stratum")) morie_stratified_sample(df, col, n, proportional = FALSE, seed = seed)
      else morie_stratified_sample(df, col, n, proportional = TRUE, seed = seed)
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
  if (has("--no-weight")) s$.weight <- NULL
  out(sprintf("Sampled %d rows using %s%s\n", nrow(s), method,
              if (".weight" %in% names(s)) " (the .weight column holds each row's design weight; --no-weight drops it)" else ""))
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
    out("usage: rmorie pipeline (--all | --modules a,b,...) [--cpads FILE | --dataset KEY] [--output-dir DIR] [--no-carbon]\n")
    return(2L)
  }
  selected <- if (is.null(mods)) morie_module_names() else trimws(strsplit(mods, ",")[[1L]])
  od <- flag("--output-dir")
  cp <- flag("--cpads") %||% flag("--cpads-csv")
  if (is.null(cp) && !is.null(flag("--dataset"))) cp <- .cpads_dataset_csv(flag("--dataset"))
  run <- function() {
    if (is.null(cp)) morie_run_morie_modules(selected, output_dir = od)
    else morie_run_morie_modules(selected, cpads_csv = cp, output_dir = od)
  }
  if (pipeline && !has("--no-carbon")) {
    edir <- file.path(od %||% file.path("data", "manifest", "outputs"), "emissions")
    r <- morie_emissions_track(run(), project_name = "morie-pipeline", output_dir = edir)
    res <- r$value
    out(sprintf("Pipeline CO2 emissions: %s kg CO2eq  (%s)\n", format(signif(r$emissions_kg, 3)), file.path(edir, "emissions.csv")))
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
  ans <- morie_llm_ask(morie_build_prompt(paste(rest, collapse = " "), context = ctx), model = mdl)
  out(paste0(ans, "\n"))
  if (isTRUE(attr(ans, "fallback"))) {
    out(.cli_llm_fallback_cause())
    return(1L)
  }
  0L
}

# One line naming why no model answered, for `ask`, `percy` and friends.
.cli_llm_fallback_cause <- function(model = NULL) {
  base <- .morie_llm_api_base()
  if (!is.null(base) && !isTRUE(tryCatch(.morie_llm_probe_api(), error = function(e) FALSE))) {
    return(sprintf("could not connect to your endpoint %s (rmorie provider show / unset)\n", base))
  }
  if (!is.null(.morie_llm_hosted_key()) && isTRUE(tryCatch(.morie_llm_hosted_rejected(), error = function(e) FALSE))) {
    return(sprintf("your hosted key was rejected by %s -- run `rmorie login` again\n", .morie_llm_hosted_base()))
  }
  if (!is.null(model)) return(sprintf("no provider answered for model '%s' (rmorie models lists the names)\n", model))
  "no LLM backend answered; this is the local fallback text (rmorie login with GitHub or --email, or start Ollama)\n"
}

.cli_chat <- function(rest, flag, out) {
  mdl <- flag("--model")
  out(sprintf("rmorie chat (%s). Type a question; /quit to leave.\n", morie_llm_detect_provider()))
  history <- character()
  repeat {
    q <- .cli_readline("you> ")
    if (is.na(q) || !nzchar(q) || q %in% c("/quit", "/exit", "/q", "q", "quit", "exit")) break
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
      if (identical(v, "skip")) list(ok = NA, detail = "skipped: not available here")
      else if (is.character(v) && length(v) == 2L && identical(v[[1L]], "skip")) list(ok = NA, detail = paste0("skipped: ", v[[2L]]))
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
        !isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE))) return(c("skip", "rmorie was built without liboqs/libsodium"))
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
  has_cpads <- file.exists(tryCatch(.cpads_default_csv(), error = function(e) ""))
  check("module run: power-design", function() {
    if (!has_cpads) return(c("skip", "no CPADS CSV (rmorie pull ocp21)"))
    r <- morie_run_morie_module("power-design", output_dir = tempfile())
    is.list(r) && length(r) > 0L
  })
  check("inspector + verify on a module table", function() {
    if (!has_cpads) return("skip")
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
         sprintf(paste0("The `power-design` module computes how many participants you need to detect a given effect. ",
                        "Output lands in %s. You will see a synthetic-data note: the bundled CPADS frame is a toy file; ",
                        "add `--cpads /path/to/real.csv` when you have the PUMF."), file.path(out_root, "power-design")),
         c("run-module", "power-design", "--output-dir", file.path(out_root, "power-design"))),
    list("What did we just produce?",
         "The table that answers \"how many participants do I need?\" is power_two_proportion_gender.csv. `rmorie explain` says how to read any output file.",
         c("explain", "power_two_proportion_gender.csv")),
    list("Pull real data in one line",
         "`rmorie list-datasets` shows the keys; `rmorie pull KEY` writes one as CSV.",
         c("list-datasets")),
    list("What to do next",
         paste0("  Run any module:        rmorie run-module NAME\n  Pull a dataset:        rmorie pull KEY\n",
                "  Ask for help:          rmorie ask \"...\"   (rmorie login, GitHub or --email, first)\n",
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
    if (is.na(ans)) {
      out("\nthe tutorial needs an interactive terminal (stdin is closed); `rmorie tutorial --dry-run` prints every command instead\n")
      return(2L)
    }
    if (ans == "q") return(0L)
    if (ans == "s") next
    out(sprintf("  $ %s\n\n", cmd))
    morie_cli(s[[3L]], out = out)
  }
  0L
}

.cli_generate_template <- function(rest, flag, has, out) {
  positional <- rest[!startsWith(rest, "--") & !rest %in% c(flag("--module"), flag("--out"))]
  module <- flag("--module") %||% (if (length(positional)) positional[[1L]] else "power-design")
  dest <- flag("--out") %||% "first-paper.md"
  key <- gsub("-", "_", module, fixed = TRUE)
  ns_exports <- getNamespaceExports(asNamespace(utils::packageName()))
  known_module <- module %in% morie_module_names()
  known_family <- any(startsWith(ns_exports, paste0("morie_", key)))
  if (!known_module && !known_family) {
    out(sprintf("unknown module: %s (a pipeline module from `rmorie list-modules`, or a method family such as hawkes or dml)\n", module))
    return(1L)
  }
  if (file.exists(dest) && !has("--force")) {
    out(sprintf("%s already exists; pass --out NAME to write elsewhere or --force to replace it\n", dest))
    return(1L)
  }
  src <- system.file("templates", "first-paper.md", package = utils::packageName())
  if (!nzchar(src)) stop("the first-paper template is missing from this installation", call. = FALSE)
  txt <- gsub("[MODULE_NAME]", module, paste(readLines(src, warn = FALSE), collapse = "\n"), fixed = TRUE)
  if (known_module) {
    mods <- morie_list_morie_modules()
    desc <- mods$description[match(module, mods$name)]
    if (length(desc) == 1L && !is.na(desc) && nzchar(desc)) {
      txt <- gsub("[REPLACE_WITH_MODULE_DESCRIPTION]", sub("[.]$", "", desc), txt, fixed = TRUE)
    }
  }
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
                  "       rmorie crypto encrypt FILE --to PKFILE|KEYNAME\n",
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
    rcpt <- flag("--to") %||% flag("--recipient")
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
  cat_ <- morie_dataset_catalog()
  boot <- cat_[cat_$type == "bootstrap", , drop = FALSE]
  survey <- flag("--survey")
  if (is.null(survey)) {
    # hundreds of MB per file: never start without being told which one
    out(paste0("usage: rmorie download-bootstrap --survey KEY|all\n",
               "  The bootstrap-weight files are large (hundreds of MB each) and are cached under the morie cache directory.\n",
               "  Keys: ", paste(boot$key, collapse = ", "), "\n"))
    return(2L)
  }
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
  env <- new.env(parent = asNamespace(utils::packageName()))  # the package's functions are in scope, as in rmorie exec
  printed <- utils::capture.output(res <- withVisible(eval(parse(text = code), envir = env)))
  if (length(printed)) out(paste0(paste(printed, collapse = "\n"), "\n"))
  if (res$visible) out(paste0(paste(utils::capture.output(print(res$value)), collapse = "\n"), "\n"))
  0L
}

.cli_edit <- function(rest, out) {
  if (!length(rest)) {
    out("usage: rmorie edit FILE\n")
    return(2L)
  }
  f <- rest[[1L]]
  if (interactive()) {
    if (!file.exists(f)) file.create(f)
    utils::file.edit(f)
    return(0L)
  }
  ed <- Sys.getenv("VISUAL", "")
  if (!nzchar(ed)) ed <- Sys.getenv("EDITOR", "")
  if (!nzchar(ed)) {
    out(sprintf("No editor: set EDITOR, or open %s in RStudio / VS Code.\n", f))
    return(1L)
  }
  ed_bin <- basename(strsplit(trimws(ed), "[[:space:]]+")[[1L]][1L])
  if (!isatty(stdin()) && ed_bin %in% c("nano", "vi", "vim", "nvim", "emacs", "pico", "ed", "micro", "joe", "ne")) {
    out(sprintf("%s is a terminal editor and stdin is not a terminal here; run `rmorie edit %s` from a shell, or set EDITOR to a graphical editor\n", ed_bin, f))
    return(1L)
  }
  if (!file.exists(f)) file.create(f)
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
    out(paste0("usage: rmorie verify-pollution --pollutant no2|pm25 [--demo | --exposure-csv FILE | ",
               "--exposure-mean X --exposure-prevalence P] [--outcome NAME] [--reference 5.8] [--baseline-rate 500] ",
               "[--population 1000000] [--region R] [--years Y] [--json]\n"))
    return(2L)
  }
  if (!has("--demo") && is.null(flag("--exposure-csv")) &&
      xor(is.null(flag("--exposure-mean")), is.null(flag("--exposure-prevalence")))) {
    out("--exposure-mean and --exposure-prevalence go together (the share of the population at that mean exposure)\n")
    return(2L)
  }
  for (nm in c("--exposure-mean", "--exposure-prevalence", "--reference", "--baseline-rate", "--population")) {
    v <- flag(nm)
    if (!is.null(v) && (is.na(suppressWarnings(as.numeric(v))) || !is.finite(as.numeric(v)))) {
      out(sprintf("%s must be a number, not '%s'\n", nm, v))
      return(2L)
    }
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
  secs <- suppressWarnings(as.numeric(flag("--seconds") %||% "3"))
  if (length(secs) != 1L || is.na(secs) || !is.finite(secs) || secs <= 0) {
    out(sprintf("--seconds must be a positive number, not '%s'\n", flag("--seconds")))
    return(2L)
  }
  od <- flag("--output-dir") %||% "emissions"
  t <- morie_emissions_start(project_name = "morie-emissions-check", output_dir = od,
                             capsule = !has("--no-capsule"),
                             country_iso_code = flag("--country") %||% "")
  on.exit(if (!is.null(t$sampler) && isTRUE(t$sampler)) tryCatch(.emissions_sampler_stop(), error = function(e) NULL), add = TRUE)
  t0 <- Sys.time()
  x <- 0
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) x <- x + sum(sqrt(seq_len(20000)))
  e <- morie_emissions_stop(t)
  t$sampler <- FALSE
  out(.emissions_text(e))
  0L
}


# `rmorie VERB --help`: the lines of the help text that describe the verb, never the verb itself.
.cli_verb_help <- function(verb, out) {
  txt <- character()
  morie_cli("help", out = function(s) txt <<- c(txt, s))
  lines <- strsplit(paste(txt, collapse = ""), "\n", fixed = TRUE)[[1L]]
  aliases <- c(agent = "percy", perseus = "percy")
  if (verb %in% names(aliases)) verb <- aliases[[verb]]
  hit <- grepl(paste0("^  ", verb, "( |$)"), lines)
  keep <- hit
  for (i in which(hit)) {  # continuation lines are indented deeper and follow the verb's line
    j <- i + 1L
    while (j <= length(lines) && grepl("^        ", lines[j])) {
      keep[j] <- TRUE
      j <- j + 1L
    }
  }
  if (!any(keep)) {
    out(sprintf("rmorie %s: no help entry (rmorie help lists every verb)\n", verb))
    return(2L)
  }
  # "usage: rmorie VERB ARGS   what it does", one line per form of the verb
  shown <- vapply(lines[keep], function(l) {
    l <- trimws(l)
    if (startsWith(l, verb)) paste0("usage: rmorie ", gsub("\\s{2,}", "   ", l)) else paste0("       ", gsub("\\s{2,}", "   ", l))
  }, "")
  out(paste0(paste(shown, collapse = "\n"), "\n"))
  0L
}

# Where the command line keeps what it pulls: the user cache directory (an existing store is reused).
.morie_cli_cache_db <- function() {
  root <- morie_cache_dir()
  duck <- file.path(root, "morie.duckdb")
  lite <- file.path(root, "morie.db")
  if (file.exists(duck)) return(duck)
  if (file.exists(lite)) return(lite)
  if (requireNamespace("duckdb", quietly = TRUE)) duck else lite
}
