# Helpers for the verbs of morie_cli() that mirror the Python command line:
# explain, inspect, verify, profile-dataset, sample, run-modules, pipeline,
# percy/agent, chat, selftest, tutorial, generate-template, update, crypto,
# ingest, download-bootstrap, exec, edit, percysuits, verify-pollution and
# emissions. Each takes the parsed `rest`, the `flag`/`has` accessors and the
# `out` sink of morie_cli() and returns an exit status.

# no terminal to type into (Rscript < /dev/null, a pipe); a function so tests can mock it
.cli_stdin_closed <- function() !interactive() && !isatty(stdin())

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
    stem <- tolower(sub("[-_].*$", "", module))
    # a table belongs to the module when it sits in the module's own output directory or its name
    # starts with the module's name (ebac_core_*, descriptive_statistics_*); only when no table does
    # is the module's first word tried as a prefix (power-design writes power_*), never a substring
    known <- .morie_module_outputs[[module]]
    if (!is.null(known) && !any(grepl("\\.csv$", known, ignore.case = TRUE))) {
      # figures, tables, final-report: their outputs are figures / HTML, not tables
      kinds <- unique(tolower(tools::file_ext(known)))
      return(structure(character(), note = sprintf("%s writes no tables (its outputs are %s files); nothing to check",
                                                   module, paste(kinds[nzchar(kinds)], collapse = "/")), rc = 0L))
    }
    hit <- if (!is.null(known)) {
      # a module morie knows: exactly the tables it writes
      basename(files) %in% basename(known) | grepl(paste0("/", module, "/"), files, fixed = TRUE) |
        startsWith(norm(basename(files)), key)
    } else {
      grepl(paste0("/", module, "/"), files, fixed = TRUE) | startsWith(norm(basename(files)), key)
    }
    if (is.null(known) && !any(hit) && nzchar(stem)) hit <- startsWith(tolower(basename(files)), paste0(stem, "_"))
    if (any(hit)) {
      files <- files[hit]
    } else {
      # never every table in the tree under another module's name
      return(structure(character(), note = sprintf("no table of %s in %s (run it: rmorie run-module %s)",
                                                   module, target, module), rc = 1L))
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
  lines <- c(sprintf("%s: %s%s", rep$path, if (isTRUE(rep$passed)) "PASS" else "FAIL",
                     if (!is.null(rep$note)) paste0("  (", rep$note, ")") else ""))
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
  paste0("own file: ", .morie_own_file_target(e$local_path))
}

.cli_list_datasets <- function(out) {
  d <- morie_list_datasets()
  cat_df <- morie_dataset_catalog()
  route <- vapply(seq_len(nrow(d)), function(i) {
    if (identical(d$type[i], "hosted")) return("data.rmorie.com (your MORIE key)")
    .cli_dataset_route(cat_df[cat_df$key == d$key[i], , drop = FALSE][1L, ])
  }, character(1L))
  shown <- ifelse(d$cached & !is.na(d$rows), format(d$rows, big.mark = ",", trim = TRUE), "not cached")
  own <- startsWith(route, "own file")
  for (i in which(own & !d$cached)) {  # read in place, never cached: say whether the file is there
    lp <- cat_df$local_path[cat_df$key == d$key[i]][1L]
    shown[i] <- if (!is.null(.morie_own_file_path(lp))) "present" else if (identical(d$key[i], "mapq")) "synthetic" else "not found"
  }
  kw <- max(20L, max(nchar(d$key)))  # long db/table keys ran into the type column
  out(sprintf(paste0("%-", kw, "s %-12s %10s  %s\n"), "Key", "Type", "Rows", "Route"))
  out(paste0(sprintf(paste0("%-", kw, "s %-12s %10s  %s"), d$key, d$type, shown, route), collapse = "\n"))
  out("\n")
  n_hub <- sum(d$type == "hosted")
  n_cat <- nrow(d) - n_hub
  n_own <- sum(startsWith(route, "own file"))
  out(strrep("-", 96L))
  out("\n")
  out(sprintf("%d keys: %d download from their portal, rmoriedata or data.rmorie.com on first use; %d %s your own research file%s, placed at the path%s shown (MORIE_DATA_DIR moves the data directory).\n",
              n_cat, n_cat - n_own, n_own, if (n_own == 1L) "is" else "are", if (n_own == 1L) "" else "s", if (n_own == 1L) "" else "s"))
  if (n_hub) {
    out(sprintf("%d curated tables at data.rmorie.com (db/table keys), opened by your MORIE key: rmorie pull KEY\n", n_hub))
  } else {
    out(paste0("Curated tables at data.rmorie.com appear here after `rmorie login` (GitHub) or `rmorie login --email you@example.com` (they need the MORIE key)", .morie_httr2_note(), ".\n"))
  }
  0L
}

.cli_inspect <- function(rest, flag, out) {
  if (!length(rest)) {
    out("usage: rmorie inspect PATH [--module NAME]   (a CSV, or a directory of module outputs)\n")
    return(2L)
  }
  if (!is.null(flag("--module")) && !flag("--module") %in% morie_module_names()) {
    out(sprintf("unknown module: %s (names: rmorie list-modules)\n", flag("--module")))
    return(1L)
  }
  target <- rest[[1L]]
  if (dir.exists(target)) {
    files <- .cli_csv_files(target, flag("--module"), out)
    if (!length(files)) {
      out(sprintf("%s\n", attr(files, "note") %||% paste("No CSV files found in", target)))
      return(attr(files, "rc") %||% 1L)
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
  if (!is.null(flag("--module")) && !flag("--module") %in% morie_module_names()) {
    out(sprintf("unknown module: %s (names: rmorie list-modules)\n", flag("--module")))
    return(1L)
  }
  target <- rest[[1L]]
  files <- if (dir.exists(target)) .cli_csv_files(target, flag("--module"), out) else if (file.exists(target)) target else character()
  if (!length(files)) {
    out(sprintf("%s\n", attr(files, "note") %||%
                  (if (dir.exists(target)) paste("No CSV files found in", target) else paste("Path not found:", target))))
    return(attr(files, "rc") %||% 1L)
  }
  failed <- 0L
  for (f in files) {
    rep <- if (grepl("\\.json$", f, ignore.case = TRUE)) morie_verify_statistical_output(f) else .cli_verify_csv(f)
    out(.cli_verify_text(rep))
    if (!isTRUE(rep$passed)) failed <- failed + 1L
  }
  if (length(files) > 1L) out(sprintf("\n%d tables: %d PASS, %d FAIL\n", length(files), length(files) - failed, failed))
  if (failed) 1L else 0L
}

# A module table verified as a CSV: parses, has rows and columns, no column that is entirely NA,
# no infinite numbers, the header is unique. Shaped like morie_verify_statistical_output()'s report.
.cli_verify_csv <- function(path) {
  checks <- list()
  # read.csv's warnings (an unterminated quote, a missing final newline) are judged below, not printed
  read <- function(...) tryCatch(suppressWarnings(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)),
                                 error = function(e) NULL)
  df <- read()
  lines <- tryCatch(sum(nzchar(trimws(readLines(path, warn = FALSE)))), error = function(e) 0L)
  # a header with no rows although the file has more lines: a quote that never closes swallowed them
  checks$csv_parses <- !is.null(df) && !(nrow(df) == 0L && lines > 1L)
  if (!checks$csv_parses) return(list(path = path, passed = FALSE, checks = checks))
  checks$has_columns <- ncol(df) > 0L
  checks$unique_header <- !anyDuplicated(names(df))
  num <- vapply(df, is.numeric, TRUE)
  checks$no_infinite <- !any(vapply(df[num], function(col) any(is.infinite(col)), TRUE))
  # a column of blank cells (a text field with nothing to say, e.g. missing_formula_inputs) is not empty;
  # only a column whose every cell is the NA token is
  raw <- read(colClasses = "character")
  na_col <- function(col) all(is.na(col) | col == "NA")
  # a one-row table whose text says what was not computed ("not computed", "deferred") with NA numbers is a
  # declared placeholder, written by a module that skipped an optional step on purpose; a header-only table is empty
  placeholder <- nrow(df) == 1L && !is.null(raw) && any(vapply(raw, na_col, TRUE)) &&
    any(vapply(raw, function(col) !na_col(col) && grepl("not computed|deferred|not part of|not run|not.available|not installed|skipped", col[[1L]], ignore.case = TRUE), TRUE))
  checks$no_empty_column <- nrow(df) == 0L || is.null(raw) || placeholder ||
    !any(vapply(raw, na_col, TRUE))
  # statistical sanity: a p-value lives in [0, 1]; a confidence interval's lower bound is below its upper
  pcols <- names(df)[grepl("^(p|p_value|pvalue|p\\.value|p_adj|p_adjusted|pval)$", tolower(names(df))) & num]
  checks$p_values_in_unit_interval <- !length(pcols) ||
    all(vapply(df[pcols], function(col) all(is.na(col) | (col >= 0 & col <= 1)), TRUE))
  lo <- names(df)[tolower(names(df)) %in% c("ci_lower", "ci_low", "lower", "conf_low", "conf.low") & num]
  hi <- names(df)[tolower(names(df)) %in% c("ci_upper", "ci_high", "upper", "conf_high", "conf.high") & num]
  checks$ci_bounds_ordered <- !(length(lo) == 1L && length(hi) == 1L) ||
    all(is.na(df[[lo]]) | is.na(df[[hi]]) | df[[lo]] <= df[[hi]])
  # a standard error is never negative
  secols <- names(df)[tolower(names(df)) %in% c("se", "std_error", "std.error", "stderr", "std_err", "standard_error") & num]
  checks$se_nonnegative <- all(vapply(df[secols], function(col) all(is.na(col) | col >= 0), TRUE))
  list(path = path, passed = all(unlist(checks)), checks = checks, rows = nrow(df), cols = ncol(df),
       note = if (isTRUE(placeholder)) "placeholder row: this table was declared not computed" else
         if (nrow(df) == 0L) "header only: the module wrote no rows here" else NULL)
}

.cli_profile_dataset <- function(rest, flag, has, out) {
  path <- flag("--csv") %||% (if (length(rest) && !startsWith(rest[[1L]], "--")) rest[[1L]] else NULL)
  if (is.null(path)) {
    out("usage: rmorie profile-dataset PATH|--csv PATH [--treatment COL] [--outcome COL] [--weights COL] [--suggest]\n")
    return(2L)
  }
  if (!file.exists(path)) {
    out(sprintf("File not found: %s\n", path))
    return(1L)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  for (h in c("--treatment", "--outcome", "--weights")) {
    col <- flag(h)
    if (!is.null(col) && !col %in% names(df)) {
      out(sprintf("%s %s: not a column of %s (columns: %s)\n", h, col, path, paste(names(df), collapse = ", ")))
      return(2L)
    }
  }
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
    out("usage: rmorie sample PATH|--csv PATH --n N [--method srs|stratified|cluster|pps] [--strata-col COL [--per-stratum]] [--cluster-col COL] [--size-col COL] [--seed 42] [--output FILE] [--no-weight]\n")
    return(2L)
  }
  if (!grepl("^[0-9]+$", n) || as.integer(n) < 1L) {
    out(sprintf("--n must be a whole number of rows, not '%s'\n", n))
    return(2L)
  }
  n <- as.integer(n)
  seed <- suppressWarnings(as.integer(flag("--seed") %||% "42"))
  if (is.na(seed) || !is.finite(seed)) {
    out(sprintf("--seed must be an integer, not '%s'\n", flag("--seed")))
    return(2L)
  }
  method <- flag("--method") %||% "srs"
  if (!file.exists(path)) {
    out(sprintf("File not found: %s\n", path))
    return(1L)
  }
  methods <- c("srs", "stratified", "cluster", "pps")
  if (!method %in% methods) {
    out(sprintf("unknown --method '%s' (methods: %s)\n", method, paste(methods, collapse = ", ")))
    return(2L)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  # every usage problem is named in words and exits 2, before anything is drawn
  need <- c(stratified = "--strata-col", cluster = "--cluster-col", pps = "--size-col")[method]
  col <- if (!is.na(need)) flag(need) else NULL
  if (!is.na(need) && is.null(col)) {
    out(sprintf("--method %s needs %s COL\n", method, need))
    return(2L)
  }
  if (!is.null(col) && !col %in% names(df)) {
    out(sprintf("column '%s' is not in %s (columns: %s)\n", col, path, paste(head(names(df), 12), collapse = ", ")))
    return(2L)
  }
  if (n > nrow(df) && method %in% c("srs", "stratified", "pps")) {
    out(sprintf("--n %d exceeds the %d rows in %s%s\n", n, nrow(df), path,
                if (method == "pps") " (PPS draws without replacement)" else ""))
    return(1L)
  }
  if (method == "cluster" && n > length(unique(df[[col]]))) {
    out(sprintf("--n %d clusters asked for; %s has %d clusters\n", n, col, length(unique(df[[col]]))))
    return(1L)
  }
  s <- switch(method,
    srs = morie_simple_random_sample(df, n, seed = seed),
    # --n is the total sample size, allocated across the strata in proportion to their sizes;
    # --per-stratum draws --n rows from every stratum instead
    stratified = morie_stratified_sample(df, col, n, proportional = !has("--per-stratum"), seed = seed),
    cluster = morie_cluster_sample(df, col, n, seed = seed),
    pps = morie_pps_sample(df, col, n, seed = seed))
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
  unknown <- setdiff(selected, morie_module_names())
  if (length(unknown)) {
    out(sprintf("Unknown module%s: %s (rmorie list-modules names them)\n", if (length(unknown) > 1L) "s" else "", paste(unknown, collapse = ", ")))
    return(1L)
  }
  od <- flag("--output-dir")
  if (is.null(od)) {
    # without a directory the module tables were computed and thrown away
    od <- "morie-output"
    out(sprintf("writing the module tables under %s/ (choose with --output-dir)\n", od))
  }
  cp <- flag("--cpads") %||% flag("--cpads-csv")
  if (is.null(cp) && !is.null(flag("--dataset"))) cp <- .cpads_dataset_csv(flag("--dataset"))
  before <- .cli_file_stamps(od)
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
  empty <- vapply(res, function(x) !length(x), logical(1L))
  if (any(!empty)) out(sprintf("Completed modules: %s\n", paste(names(res)[!empty], collapse = ", ")))
  n_new <- .cli_files_written(od, before)
  out(sprintf(paste0("Written to %s/ (%d new file%s; the modules of one run share this directory so figures, tables ",
                     "and final-report can read the others' tables; run-module NAME alone uses morie-output/NAME/)\n"),
              od, n_new, if (n_new == 1L) "" else "s"))
  if (any(empty)) {
    out(sprintf("Wrote nothing: %s (figures, tables and meta-synthesis collect what a project checkout wrote)\n",
                paste(names(res)[empty], collapse = ", ")))
  }
  if (all(empty)) 1L else 0L
}

# modification times of the files under `dir` (named by path), to tell what a run wrote
.cli_file_stamps <- function(dir) {
  f <- list.files(dir, recursive = TRUE, full.names = TRUE)
  stats::setNames(as.numeric(file.mtime(f)), f)
}

# files under `dir` that are new or changed since `before` (.cli_file_stamps)
.cli_files_written <- function(dir, before) {
  now <- .cli_file_stamps(dir)
  sum(!names(now) %in% names(before) | now > before[names(now)], na.rm = TRUE)  # NA for a new file: counted by the first test
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
  out(paste0(trimws(ans), "\n"))
  if (isTRUE(attr(ans, "fallback"))) {
    out(.cli_llm_fallback_cause(mdl))
    return(1L)
  }
  0L
}

# TRUE when every analysis a subject ran came back empty or failed (tables, payload and summary all empty,
# or a title marked "(failed)"): `rmorie analyze` then exits 1 instead of reporting success.
# TRUE for the text of a JSON object ({} included): what `analyze SUBJECT JSON` takes
.cli_json_object <- function(txt) {
  if (identical(trimws(txt), "{}")) return(TRUE)
  v <- tryCatch(.morie_from_json(txt, simplifyVector = FALSE), error = function(e) NULL)
  is.list(v) && length(v) > 0L && !is.null(names(v)) && all(nzchar(names(v)))
}

.cli_analyze_all_failed <- function(res) {
  if (is.character(res)) {
    res <- tryCatch(.morie_from_json(res, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (!is.list(res) || !length(res)) return(FALSE)
  if (!is.null(res$status)) return(identical(res$status, "error"))
  failed <- vapply(res, function(x) {
    if (!is.list(x)) return(FALSE)
    grepl("(failed)", x$title %||% "", fixed = TRUE) ||
      any(grepl("^missing column", unlist(x$warnings %||% character()))) ||
      (!length(x$tables %||% list()) && !length(x$payload %||% list()) && !length(x$summary_lines %||% list()))
  }, TRUE)
  length(failed) > 0L && all(failed)
}

# One line naming why no model answered, for `ask`, `percy` and friends.
.cli_llm_fallback_cause <- function(model = NULL) {
  gemini_key <- nzchar(Sys.getenv("GEMINI_API_KEY")) || nzchar(Sys.getenv("GOOGLE_API_KEY"))
  gemini_line <- paste0("your GEMINI_API_KEY (or GOOGLE_API_KEY) did not get an answer from Gemini ",
                        "(rejected key, quota or network); check it, or unset it to use the other providers\n")
  # a model the caller named leads the line; a Gemini model with a Gemini key set points at that key
  lead <- if (!is.null(model) && nzchar(model)) sprintf("no provider answered for model '%s': ", model) else ""
  if (nzchar(lead) && gemini_key && grepl("gemini", model, ignore.case = TRUE)) return(paste0(lead, gemini_line))
  base <- .morie_llm_api_base()
  if (!is.null(base) && !isTRUE(tryCatch(.morie_llm_probe_api(), error = function(e) FALSE))) {
    return(paste0(lead, sprintf("could not connect to your endpoint %s (rmorie provider show / unset)\n", base)))
  }
  if (!is.null(.morie_llm_hosted_key()) && isTRUE(tryCatch(.morie_llm_hosted_rejected(), error = function(e) FALSE))) {
    return(paste0(lead, sprintf("your hosted key was rejected by %s -- run `rmorie login` again\n", .morie_llm_hosted_base())))
  }
  if (gemini_key) return(paste0(lead, gemini_line))
  if (nzchar(lead)) return(paste0(lead, "rmorie models lists the names\n"))
  paste0("no LLM backend answered; this is the local fallback text (rmorie login with GitHub or --email", .morie_httr2_note(), ", or start Ollama)\n")
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
    # ML-KEM is rmoriebricklayer's FIPS 203 code, the symmetric layer native when libsodium is absent
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
  if (!dry && .cli_stdin_closed()) {
    # say so before the welcome and STEP 1, not after them
    out("the tutorial needs an interactive terminal (stdin is closed); `rmorie tutorial --dry-run` prints every command instead\n")
    return(2L)
  }
  out_root <- file.path(path.expand("~"), sprintf("rmorie-tutorial-%s", format(Sys.Date())))
  out(sprintf(paste0(
    "\nWelcome to the rmorie tutorial.\n\nThis walks you through one full analysis from a clean install to\n",
    "interpretable output. Every step is a real command, and we run each one\nfor you. Output lands in:\n\n    %s\n\n",
    "Press Enter to continue, s to skip a step, q to quit. If you have used\nmorie before, `rmorie cheatsheet` is the one-page reference.\n"), out_root))
  steps <- list(
    list("What does morie know how to do?",
         "morie ships 23 analysis modules. Each has a short description and a list of output files.",
         c("list-modules", "--outputs")),
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
      desc <- sub("[.]$", "", desc)
      desc <- paste0(tolower(substr(desc, 1L, 1L)), substring(desc, 2L))  # it follows "which provides"
      txt <- gsub("[REPLACE_WITH_MODULE_DESCRIPTION]", desc, txt, fixed = TRUE)
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
    txt <- paste(suppressWarnings(readLines(url, warn = FALSE)), collapse = "")
    v <- regmatches(txt, regexpr("\"Version\"\\s*:\\s*\"[^\"]+\"", txt))
    return(list(version = sub(".*\"([^\"]+)\"$", "\\1", v), source = "r-universe"))
  }
  url <- "https://raw.githubusercontent.com/rootcoder007/morie/main/r-package/morie/DESCRIPTION"
  l <- grep("^Version:", suppressWarnings(readLines(url, warn = FALSE)), value = TRUE)
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
  repos <- c("https://rootcoder007.r-universe.dev", "https://cloud.r-project.org")
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
  # the same command as printed above, called directly (no text evaluated)
  if (identical(pkg, "rmorie")) {
    utils::install.packages("rmorie", repos = repos)
  } else {
    if (!requireNamespace("remotes", quietly = TRUE)) utils::install.packages("remotes", repos = repos[[2L]])
    getExportedValue("remotes", "install_github")("rootcoder007/morie", subdir = "r-package/morie")
  }
  0L
}

.cli_keystore_path <- function() {
  file.path(path.expand("~"), ".morie", "keys", "keystore.json")
}

.cli_keystore_password <- function() {
  pw <- Sys.getenv("MORIE_KEYSTORE_PASSWORD", "")
  if (nzchar(pw)) return(pw)
  if (.cli_stdin_closed()) {
    stop("no terminal to type the keystore password into: set MORIE_KEYSTORE_PASSWORD, or use --output DIR",
         call. = FALSE)
  }
  .cli_readline("Keystore password: ")
}

# Key files written by rmorie / morie 1.4.0 start with a marker, so the two arms read each
# other's keys; a bare 1,184 / 2,400-byte file is a 1.3.x key, refused for encryption (morie
# 1.3.x key generation was not FIPS 203) and read for decryption of old files only.
.morie_pk_magic <- c(charToRaw("MORIEPK"), as.raw(2L))
.morie_sk_magic <- c(charToRaw("MORIESK"), as.raw(2L))

.cli_read_key_file <- function(path, magic) {
  raw <- readBin(path, "raw", file.info(path)$size)
  starts <- function(m) length(raw) >= length(m) && identical(raw[seq_along(m)], m)
  if (starts(magic)) return(list(key = raw[-seq_along(magic)], legacy = FALSE))
  if (identical(magic, .morie_pk_magic) && starts(.morie_sk_magic)) {
    stop(path, " is a secret key; encrypt to the public key (.moriepk) instead", call. = FALSE)
  }
  if (identical(magic, .morie_sk_magic) && starts(.morie_pk_magic)) {
    stop(path, " is a public key; decrypt needs the secret key (.moriesk)", call. = FALSE)
  }
  list(key = raw, legacy = TRUE)
}

.cli_crypto <- function(rest, flag, out) {
  sub <- if (length(rest)) rest[[1L]] else ""
  force <- "--force" %in% rest
  usage <- paste0("usage: rmorie crypto keygen [--name NAME] [--output DIR] [--force]\n",
                  "       rmorie crypto encrypt FILE --to PKFILE|KEYNAME [--out NAME] [--force]\n",
                  "       rmorie crypto decrypt FILE --key SKFILE|KEYNAME [--out NAME] [--force]\n",
                  "Keys live in ~/.morie/keys/keystore.json (password: prompt or MORIE_KEYSTORE_PASSWORD),\n",
                  "or in files: keygen --output DIR writes DIR/NAME.moriepk and DIR/NAME.moriesk.\n")
  is_path <- function(x, ext) grepl("[/\\\\]", x) || endsWith(x, ext)
  refuse_existing <- function(dest) {
    if (file.exists(dest) && !force) {
      out(sprintf("%s already exists; pass --out NAME to write elsewhere or --force to replace it\n", dest))
      return(TRUE)
    }
    FALSE
  }
  if (identical(sub, "keygen")) {
    name <- flag("--name") %||% "default"
    if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]{0,63}$", name)) {
      out(sprintf("--name '%s': a key name is letters, digits, '.', '_' or '-' (up to 64, not starting with '.')\n", name))
      return(2L)
    }
    od <- flag("--output")
    if (!is.null(od)) {
      pk_path <- file.path(od, paste0(name, ".moriepk"))
      sk_path <- file.path(od, paste0(name, ".moriesk"))
      if (file.exists(sk_path) && !force) {
        out(sprintf("%s already exists; files encrypted to it would become undecryptable. Pass --force to replace it.\n",
                    sk_path))
        return(1L)
      }
      k <- morie_crypto_hybrid_keygen()
      dir.create(od, recursive = TRUE, showWarnings = FALSE)
      writeBin(c(.morie_pk_magic, k$pk), pk_path)
      # a secret key is owner-only: create it empty with mode 600, then write it
      old <- Sys.umask("077")
      on.exit(Sys.umask(old), add = TRUE)
      if (file.exists(sk_path)) unlink(sk_path)
      file.create(sk_path)
      Sys.chmod(sk_path, "0600", use_umask = FALSE)
      writeBin(c(.morie_sk_magic, k$sk), sk_path)
      out(sprintf("Public key:  %s\nSecret key:  %s\n", pk_path, sk_path))
      return(0L)
    }
    k <- morie_crypto_hybrid_keygen()
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
    if (file.exists(rcpt) && !dir.exists(rcpt)) {
      kf <- tryCatch(.cli_read_key_file(rcpt, .morie_pk_magic), error = function(e) e)
      if (inherits(kf, "error")) {
        out(paste0(conditionMessage(kf), "\n"))
        return(1L)
      }
      if (kf$legacy) {
        out(sprintf(paste0("%s was written by morie 1.3.x, whose key generation was not FIPS 203; a file ",
                           "encrypted to it could not be opened. Ask the key's owner to run `rmorie crypto ",
                           "keygen` (or morie's) with 1.4.0 and share the new .moriepk\n"), rcpt))
        return(1L)
      }
      pk <- kf$key
    } else if (is_path(rcpt, ".moriepk")) {
      out(sprintf("%s: no such public key file\n", rcpt))
      return(1L)
    } else {
      pk <- morie_crypto_keystore_public_key(rcpt, path = .cli_keystore_path())  # public keys are in the clear
    }
    dest <- flag("--out") %||% paste0(f, ".morieenc")
    if (refuse_existing(dest)) return(1L)
    ct <- morie_crypto_hybrid_encrypt(readBin(f, "raw", file.info(f)$size), pk)
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
    ctx <- readBin(f, "raw", file.info(f)$size)
    if (file.exists(kn) && !dir.exists(kn)) {  # a secret key written by `keygen --output DIR`
      kf <- tryCatch(.cli_read_key_file(kn, .morie_sk_magic), error = function(e) e)
      if (inherits(kf, "error")) {
        out(paste0(conditionMessage(kf), "\n"))
        return(1L)
      }
      if (kf$legacy && morie_crypto_hybrid_container_version(ctx) == 2L) {
        out(sprintf(paste0("%s was encrypted by 1.4.0 to a key pair that this 1.3.x secret key did not make ",
                           "(a 1.4.0 key pair carries a marker). Encrypt it again to a key pair from ",
                           "`rmorie crypto keygen`.\n"), f))
        return(1L)
      }
      sk <- kf$key
    } else if (is_path(kn, ".moriesk")) {
      out(sprintf("%s: no such key file\n", kn))
      return(1L)
    } else {
      sk <- morie_crypto_keystore_load(kn, .cli_keystore_password(), path = .cli_keystore_path())$sk
    }
    dest <- flag("--out") %||% (if (endsWith(f, ".morieenc")) sub("\\.morieenc$", "", f) else paste0(f, ".dec"))
    if (refuse_existing(dest)) return(1L)
    notes <- character()
    pt <- tryCatch(withCallingHandlers(morie_crypto_hybrid_decrypt(ctx, sk),
                                       warning = function(w) {
                                         notes <<- c(notes, conditionMessage(w))
                                         invokeRestart("muffleWarning")
                                       }),
                   error = function(e) e)
    for (n in notes) out(sprintf("note: %s\n", n))
    if (inherits(pt, "error")) {
      out(sprintf("decrypt failed: %s (wrong key, or the file is not a morie ciphertext)\n", conditionMessage(pt)))
      return(1L)
    }
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
      failed <- 0L
      for (nm in names(res)) {
        if (!is.data.frame(res[[nm]])) next
        if (startsWith(nm, "_failed_")) {
          # a resource that did not download is reported, not written out as a one-cell "data" file
          failed <- failed + 1L
          out(sprintf("FAILED %s: %s\n", sub("^_failed_", "", nm), res[[nm]]$error[[1L]]))
          next
        }
        f <- file.path(od, paste0(gsub("[^A-Za-z0-9_.-]", "_", nm), ".csv"))
        utils::write.csv(res[[nm]], f, row.names = FALSE)
        out(sprintf("wrote %s (%d rows)\n", f, nrow(res[[nm]])))
      }
      return(if (failed) 1L else 0L)
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
    # an unknown id still answers 200 with the empty report template ("Case Number: " and nothing after)
    if (!grepl("\\b[0-9]{2}-[A-Z]{3,4}-[0-9]{3}\\b", paste(as.character(html), collapse = "\n"))) {
      out(sprintf("no SIU report with id %s (the site returned its empty template); ids: rmorie ingest siu --list\n", flag("--report-id")))
      return(1L)
    }
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
      if (is.null(r)) {
        out(sprintf("no %s found for '%s' (a2aj search QUERY finds citations)\n", if (dt == "laws") "law" else "decision", rest[[3L]]))
        return(1L)
      }
      out(paste0(paste(utils::capture.output(utils::str(r, max.level = 1L)), collapse = "\n"), "\n"))
      return(0L)
    }
    out(usage)
    return(2L)
  }
  if (!nzchar(portal)) {
    out(usage)
    return(2L)
  }
  out(sprintf("unknown portal '%s'; valid: ckan, tps, siu, a2aj\n", portal))
  2L
}

.cli_download_bootstrap <- function(flag, out, has = function(x) FALSE) {
  survey <- flag("--survey")
  if (is.null(survey)) {
    # hundreds of MB per file: never start without being told which one
    out(paste0("usage: rmorie download-bootstrap --survey KEY|all [--refresh]\n",
               "  The bootstrap-weight files are large (hundreds of MB each) and are cached under the morie cache directory.\n",
               "  Keys: ocs22bt, ocs24bt, cu20bt, cu23bt (or csads_2021, csads_2023, csus_2019, csus_2023)\n"))
    return(2L)
  }
  # the R function's own route and key resolution, so the verb and the function cannot disagree
  r <- tryCatch(morie_download_bootstrap(survey, refresh = has("--refresh")), error = function(e) e)
  if (inherits(r, "error")) {
    out(sprintf("%s\n", conditionMessage(r)))
    return(1L)
  }
  out(sprintf("%d bootstrap file(s) cached (a cached one is kept; --refresh downloads it again)\n", r))
  0L
}

.cli_exec <- function(rest, flag, out) {
  f <- flag("--file")
  if (!is.null(f) && !file.exists(f)) {
    out(sprintf("%s: no such file\n", f))
    return(1L)
  }
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
               "--exposure-mean X --exposure-prevalence P] [--outcome NAME] [--reference X (default 10 for no2, 5.8 for pm25)] [--baseline-rate 500] ",
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
    reference = as.numeric(flag("--reference") %||% (if (identical(tolower(flag("--pollutant") %||% ""), "no2")) "10" else "5.8")),
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
  country <- toupper(flag("--country") %||% "")
  if (nchar(country) == 2L) {
    # FR -> FRA; an unknown two-letter code stays as typed so the check below names it
    m <- .emissions_iso2_to_iso3(country)
    if (nzchar(m)) country <- m
  }
  if (nzchar(country) && is.null(.emissions_energy_mix()[[country]])) {
    out(sprintf(paste0("--country %s: not a country code in the energy-mix table (ISO-3 such as CAN, FRA, USA; ",
                       "ISO-2 FR also works); ignoring it and detecting the location instead (the world average ",
                       "applies only when detection fails; MORIE_COUNTRY_ISO overrides)\n"), flag("--country")))
    country <- ""
  }
  secs <- suppressWarnings(as.numeric(flag("--seconds") %||% "3"))
  if (length(secs) != 1L || is.na(secs) || !is.finite(secs) || secs <= 0) {
    out(sprintf("--seconds must be a positive number, not '%s'\n", flag("--seconds")))
    return(2L)
  }
  if (secs > 86400) {
    out(sprintf("--seconds must be at most 86400 (a day), not '%s'\n", flag("--seconds")))
    return(2L)
  }
  od <- flag("--output-dir") %||% "emissions"
  dir.create(od, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(od) || file.access(od, 2L) != 0L) {
    out(sprintf("cannot write to %s (permission denied, or the path cannot be created)\n", od))
    return(1L)
  }
  t <- morie_emissions_start(project_name = "morie-emissions-check", output_dir = od,
                             capsule = !has("--no-capsule"),
                             country_iso_code = country)
  on.exit(if (!is.null(t$sampler) && isTRUE(t$sampler)) tryCatch(.emissions_sampler_stop(), error = function(e) NULL), add = TRUE)
  t0 <- Sys.time()
  x <- 0
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) x <- x + sum(sqrt(seq_len(20000)))
  e <- morie_emissions_stop(t)
  t$sampler <- FALSE
  out(.emissions_text(e))
  0L
}


# The --options a verb takes, read from its own usage text (the help is what users are told).
.cli_flag_cache <- new.env(parent = emptyenv())
.cli_verb_flags <- function(verb) {
  if (!is.null(.cli_flag_cache[[verb]])) return(.cli_flag_cache[[verb]])
  txt <- character()
  rc <- tryCatch(.cli_verb_help(verb, function(s) txt <<- c(txt, s)), error = function(e) 2L)
  flags <- if (identical(rc, 0L)) unique(regmatches(paste(txt, collapse = " "),
                                                    gregexpr("--[a-z][a-z0-9-]*", paste(txt, collapse = " ")))[[1L]]) else NULL
  .cli_flag_cache[[verb]] <- flags
  flags
}

.cli_unknown_options <- function(verb, rest) {
  if (verb %in% c("help", "--help", "-h", "version", "--version", "-v")) return(character())
  opts <- rest[startsWith(rest, "--")]
  if (!length(opts)) return(character())
  known <- .cli_verb_flags(verb)
  if (is.null(known)) return(character())  # an unknown verb is reported as one
  # older spellings the verbs still take: pipeline --cpads-csv, crypto encrypt --recipient
  setdiff(opts, c(known, "--help", "--cpads-csv", "--recipient"))
}

# `rmorie VERB --help`: the lines of the help text that describe the verb, never the verb itself.
.cli_verb_help <- function(verb, out) {
  if (verb %in% c("crypto", "sample", "verify-pollution", "ingest", "profile-dataset", "pipeline")) {
    # their bare call prints the full usage (every flag); the help summary line had less
    full <- character()
    morie_cli(verb, out = function(s) full <<- c(full, s))
    out(paste(full, collapse = ""))
    return(0L)
  }
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
