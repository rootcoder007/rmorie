## =====================================================================
## setup_and_run.R
##
##   Brick-proof, cross-platform (macOS / Windows / Linux) entrypoint
##   for the OTIS A01RCDD MRP reproducibility bundle.
##
##   This script runs entirely in R. It does NOT need bash, zsh, python,
##   curl, brew, apt, dnf, winget, or any other shell tool. The only
##   external requirement is R itself (>= 4.0).
##
##   What it does:
##     1. Detects the operating system.
##     2. Auto-installs missing R packages (data.table, MatchIt, glmmTMB,
##        lme4, DHARMa, Hmisc, jsonlite, digest).
##     3. Locates the OTIS data file:
##          (a) vendored CSV in the same folder, if present
##          (b) probes default download/desktop locations on this OS
##          (c) downloads from the public Ontario Data Catalogue via
##              the CKAN API (handles future URL changes by resolving
##              the current resource URL by name pattern)
##     4. Verifies the SHA256 against the pinned provenance manifest.
##     5. Runs otis_MRP.R against the data.
##     6. Writes timestamped CSV + manifest.json + run.log into
##        results_YYYYMMDD-HHMMSS/ next to this script.
##
##   Vansh Singh Ruhela <vsruhela@proton.me>
##   ORCID 0009-0004-1750-3592
##
##   USAGE (from any OS terminal that can run R):
##     Rscript setup_and_run.R                # interactive
##     Rscript setup_and_run.R --quick        # non-interactive
##     Rscript setup_and_run.R --rdata PATH   # supply path directly
##     Rscript setup_and_run.R --help
##
##   On macOS, just double-click START_HERE.command.
##   On Windows, just double-click START_HERE.bat.
##   On Linux KDE, double-click start_here.sh.
##
## =====================================================================

## --------- 0. Tiny utility helpers ---------

say <- function(...) cat(..., "\n", sep = "")
hr  <- function()    cat(strrep("-", 58), "\n", sep = "")

ask_yn <- function(prompt, default = "Y", quick = FALSE) {
  if (isTRUE(quick)) return(default == "Y")
  hint <- if (default == "Y") "[Y/n]" else "[y/N]"
  repeat {
    cat(sprintf("%s %s ", prompt, hint))
    ans <- readLines(con = "stdin", n = 1, warn = FALSE)
    if (length(ans) == 0L || nchar(ans) == 0L) ans <- default
    ans <- toupper(substr(ans, 1L, 1L))
    if (ans %in% c("Y", "N")) return(ans == "Y")
    cat("Please answer y or n.\n")
  }
}

ask_path <- function(prompt, default, quick = FALSE) {
  if (isTRUE(quick)) return(default)
  cat(sprintf("%s [%s] ", prompt, default))
  ans <- readLines(con = "stdin", n = 1, warn = FALSE)
  if (length(ans) == 0L || nchar(ans) == 0L) return(default)
  ans
}

## Numbered menu with a default. Returns the 1-based index of the choice.
## In quick mode, auto-returns the default index without prompting.
ask_menu <- function(prompt, options, default_index = 1L, quick = FALSE) {
  if (isTRUE(quick)) return(default_index)
  cat(prompt, "\n", sep = "")
  for (i in seq_along(options)) {
    marker <- if (i == default_index) " *" else "  "
    cat(sprintf("    %d.%s %s\n", i, marker, options[i]))
  }
  cat(sprintf("  (* = default; press Enter to accept default)\n"))
  cat(sprintf("  Your choice [%d]: ", default_index))
  ans <- readLines(con = "stdin", n = 1, warn = FALSE)
  if (length(ans) == 0L || !nzchar(ans)) return(default_index)
  n <- suppressWarnings(as.integer(ans))
  if (is.na(n) || n < 1L || n > length(options)) {
    cat("  Invalid choice; using default.\n")
    return(default_index)
  }
  n
}

## Allow-listed save locations only. We deliberately avoid:
##   - /tmp and /var/folders/.../T (transient, OS-managed, confusing)
##   - System directories (require admin rights)
##   - iCloud-synced locations (can fail unpredictably when offline)
##   - Anything outside the user's home directory
## All four options below are user-owned, never require sudo, and
## work identically on macOS, Windows, and Linux.
ask_save_location <- function(prompt, default_key = "bundle",
                              script_dir, quick = FALSE) {
  home <- Sys.getenv("HOME", path.expand("~"))
  r_user_dir <- tryCatch(tools::R_user_dir("otis-mrp-reproducibility", which = "data"),
                         error = function(e) NA_character_)
  locs <- list(
    bundle    = list(label = paste0("This bundle folder  (",
                                    script_dir,
                                    ", self-contained — recommended)"),
                     path  = script_dir),
    downloads = list(label = paste0("Downloads folder    (",
                                    file.path(home, "Downloads"),
                                    ", browser default)"),
                     path  = file.path(home, "Downloads")),
    documents = list(label = paste0("Documents folder    (",
                                    file.path(home, "Documents"),
                                    ", organized storage)"),
                     path  = file.path(home, "Documents")),
    r_canon   = list(label = paste0("R-policy data folder (",
                                    if (is.na(r_user_dir)) "(R 4.0+ only)" else r_user_dir,
                                    ", what R itself recommends)"),
                     path  = if (is.na(r_user_dir)) NA_character_ else r_user_dir),
    custom    = list(label = "Other location (I'll type the folder path)",
                     path  = NA_character_)
  )
  keys <- names(locs)
  default_idx <- match(default_key, keys)
  labels <- vapply(locs, function(x) x$label, character(1))
  idx <- ask_menu(prompt, labels, default_idx, quick)
  chosen_key <- keys[idx]
  chosen <- locs[[chosen_key]]
  ## "custom" branch: prompt for a path
  if (chosen_key == "custom" || is.na(chosen$path)) {
    if (chosen_key == "r_canon" && is.na(chosen$path)) {
      cat("  R 4.0+ is required for that option; using bundle folder.\n")
      return(script_dir)
    }
    custom <- ask_path("  Enter the folder path:",
                       file.path(home, "Downloads"), quick)
    custom <- path.expand(custom)
    if (!dir.exists(custom)) {
      cat("  ! That folder does not exist; using bundle folder instead.\n")
      return(script_dir)
    }
    return(normalizePath(custom, mustWork = FALSE))
  }
  if (!dir.exists(chosen$path)) {
    dir.create(chosen$path, recursive = TRUE, showWarnings = FALSE)
  }
  cat("  -> Folder selected: ", chosen$path, "\n", sep = "")
  normalizePath(chosen$path, mustWork = FALSE)
}

## --------- 1. Locate script directory + parse CLI ---------

script_dir <- (function() {
  fa <- commandArgs(trailingOnly = FALSE)
  fp <- sub("^--file=", "", fa[grep("^--file=", fa)])
  if (length(fp) > 0L) normalizePath(dirname(fp[1])) else getwd()
})()
## No setwd(): the user's working directory is never changed; every path
## below is anchored on script_dir explicitly (CRAN code policy).

args <- commandArgs(trailingOnly = TRUE)
QUICK_MODE <- any(args %in% c("-q", "--quick"))
HELP_MODE  <- any(args %in% c("-h", "--help"))
RDATA_ARG  <- NULL
if (any(args == "--rdata")) {
  i <- which(args == "--rdata")[1]
  if (length(args) >= i + 1) RDATA_ARG <- args[i + 1]
}

if (HELP_MODE) {
  self <- file.path(script_dir, "setup_and_run.R")
  self_lines <- readLines(self)
  cat(self_lines[grep("^## USAGE", self_lines)[1] + 0:10], sep = "\n")
  quit(status = 0)
}

## --------- 2. Banner + OS detection ---------

OS_KIND <- {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    "macos"
  } else {
    "linux"
  }
}

cat("==========================================================\n")
cat("  OTIS A01RCDD Reproducibility Runner (cross-platform)\n")
cat("  Paper: Alert Complexity and Placement Volatility in\n")
cat("         Ontario Restrictive Confinement Data\n")
cat("  Author: Vansh Singh Ruhela\n")
cat("==========================================================\n")
say("OS detected:    ", OS_KIND)
say("R version:      ", R.version.string)
say("Mode:           ", if (QUICK_MODE) "quick (non-interactive)" else "interactive")
cat("\n")

## ---------- WHERE EVERYTHING WILL GO ----------
## Every absolute path the script could write to. Printed BEFORE any
## file operation so the user can audit what is happening.
home_dir       <- normalizePath(Sys.getenv("HOME", path.expand("~")),
                                winslash = "/", mustWork = FALSE)
default_data   <- script_dir   # downloaded / synthetic CSV defaults here
default_result <- file.path(script_dir,
                            paste0("results_",
                                   format(Sys.time(), "%Y%m%d-%H%M%S")))
r_user_data    <- tryCatch(tools::R_user_dir("otis-mrp-reproducibility",
                                             which = "data"),
                           error = function(e) "(R 4.0+ required)")

cat("----------------------------------------------------------\n")
cat("  WHERE EVERYTHING WILL GO\n")
cat("----------------------------------------------------------\n")
cat("  This script writes ONLY to the locations below. It never\n")
cat("  touches system files and never requires admin/sudo rights.\n\n")
cat("  Bundle folder (where setup_and_run.R lives — DEFAULT):\n")
cat("    ", script_dir, "\n", sep = "")
cat("  Results subfolder (created fresh on each run):\n")
cat("    ", default_result, "\n", sep = "")
cat("  Your home directory:\n")
cat("    ", home_dir, "\n", sep = "")
cat("  R's OS-canonical data folder (the 'proper' R-policy location):\n")
cat("    ", r_user_data, "\n", sep = "")
cat("\n")
cat("  Internet endpoints the script may contact:\n")
cat("    - https://data.ontario.ca/api/3/action/...   (CKAN API,  public)\n")
cat("    - https://data.ontario.ca/dataset/.../*.csv  (OTIS data, public)\n")
cat("    - https://cloud.r-project.org                (R packages, public)\n")
cat("  No other URLs are contacted. No personal data leaves your machine.\n")
cat("----------------------------------------------------------\n\n")

## --------- 3. Sanity-check companion files ---------

R_SCRIPT <- file.path(script_dir, "otis_MRP.R")
PROVENANCE_PATH <- file.path(script_dir, "data_provenance.json")

if (!file.exists(R_SCRIPT)) {
  say("ERROR: otis_MRP.R not found next to setup_and_run.R")
  say("  Expected at: ", R_SCRIPT)
  say("  Re-extract the bundle, keeping all files together.")
  quit(status = 2)
}

## --------- 4. Check required R packages (this script never installs) ---------

REQ_PKGS <- c("data.table", "MatchIt", "glmmTMB", "lme4",
              "DHARMa", "Hmisc", "jsonlite", "digest")

say("Step 1/5: Checking R packages...")
## CRAN code policy: scripts must not call install.packages() or the slow
## installed.packages(). Probe per package with requireNamespace() and print
## the exact install command for the user to run themselves.
missing_pkgs <- REQ_PKGS[!vapply(REQ_PKGS, requireNamespace,
                                 logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  say("  Missing: ", paste(missing_pkgs, collapse = ", "))
  say("  This script does not install packages itself. Install them in R,")
  say("  then re-run this script:")
  say("    install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))")
  quit(status = 3)
}
say("  \u2713 All required packages present.")
hr()

## --------- 5. Locate the OTIS data file ---------

say("Step 2/5: Locating the OTIS data file...")

## Read pinned provenance (for CKAN URL + SHA256)
prov <- NULL
if (file.exists(PROVENANCE_PATH)) {
  prov <- jsonlite::fromJSON(PROVENANCE_PATH)
}

## Helper: cross-platform default candidate paths
default_candidates <- function() {
  home <- normalizePath(Sys.getenv("HOME", path.expand("~")), winslash = "/", mustWork = FALSE)
  cands <- c(
    file.path(script_dir, "a01_restrictive_confinement_detailed_dataset.csv"),
    file.path(script_dir, "a01_RC.csv"),
    file.path(script_dir, "correctional_stats_report_environment1b.RData"),
    file.path(home, "Desktop", "a01_restrictive_confinement_detailed_dataset.csv"),
    file.path(home, "Downloads", "a01_restrictive_confinement_detailed_dataset.csv"),
    file.path(home, "Documents", "a01_restrictive_confinement_detailed_dataset.csv")
  )
  if (OS_KIND == "macos") {
    cands <- c(cands,
      "/Volumes/VSR/rootcoderfiles/OTIS-RC/correctional_stats_report_environment1b.RData")
  } else if (OS_KIND == "linux") {
    cands <- c(cands,
      "/mnt/OTIS-RC/correctional_stats_report_environment1b.RData",
      "/data/OTIS-RC/correctional_stats_report_environment1b.RData")
  }
  cands
}

## Helper: CKAN API URL resolution (no curl, no python — pure R)
resolve_via_ckan <- function() {
  if (is.null(prov)) return(NULL)
  pkg_slug <- prov$dataset$package_slug
  name_pat <- prov$resource$name_match_pattern
  if (is.null(pkg_slug) || is.null(name_pat)) return(NULL)
  api_url <- sprintf("https://data.ontario.ca/api/3/action/package_show?id=%s",
                     pkg_slug)
  resp <- tryCatch(
    jsonlite::fromJSON(api_url, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(resp) || !isTRUE(resp$success)) return(NULL)
  for (r in resp$result$resources) {
    name <- if (is.null(r$name)) "" else r$name
    if (grepl(name_pat, name, ignore.case = TRUE)) return(r$url)
  }
  NULL
}

## Helper: SHA256 of a file (uses digest package — already in REQ_PKGS)
sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

## Helper: synthetic data generator. Produces a schema-compliant CSV
## that lets the analysis pipeline run end-to-end without real data.
## The data is RANDOM — it does NOT reproduce the paper's numbers and
## MUST NOT be presented as if it does. Watermarking happens in
## otis_MRP.R when OTIS_SYNTHETIC=1.
## Synthetic-data seed.
## Chosen as a single random integer once at script-design time, recorded
## as a constant so synthetic runs are reproducible across machines.
## Anyone running synthetic mode with the same R version will produce
## the exact same synthetic CSV. Value is within INT32 range to match R's
## set.seed() contract on all platforms.
SYNTHETIC_SEED <- 91735246L

make_synthetic_otis_csv <- function(out_path, n_persons = 65000L,
                                    seed = SYNTHETIC_SEED) {
  set.seed(seed)
  regions <- c("Central", "Eastern", "Northern", "Toronto", "Western")

  ## --- Generate person-year base records ---
  yrs_base    <- sample(c(2023L, 2024L, 2025L), n_persons, replace = TRUE)
  ids_base    <- paste0(yrs_base, "-",
                        sprintf("%05d", ave(seq_along(yrs_base), yrs_base, FUN = seq_along)),
                        "-RC")
  gender_base <- sample(c("Male", "Female"), n_persons, replace = TRUE,
                        prob = c(0.92, 0.08))
  age_base    <- sample(c("18 to 24", "25 to 49", "50+"), n_persons, replace = TRUE,
                        prob = c(0.32, 0.55, 0.13))
  home_region <- sample(regions, n_persons, replace = TRUE,
                        prob = c(0.20, 0.18, 0.10, 0.32, 0.20))
  ## Each person-year has 1-4 placement records (heavy mass on 1)
  rows_per   <- sample(c(1L, 2L, 3L, 4L), n_persons, replace = TRUE,
                       prob = c(0.82, 0.12, 0.04, 0.02))

  ## Expand to row-level
  yrs    <- rep(yrs_base,    rows_per)
  ids    <- rep(ids_base,    rows_per)
  gender <- rep(gender_base, rows_per)
  age    <- rep(age_base,    rows_per)
  home   <- rep(home_region, rows_per)
  n_rows <- length(yrs)

  ## Region: usually = home region, occasionally not (drives vm)
  reg_at <- home
  swap_a <- runif(n_rows) < 0.18
  reg_at[swap_a] <- sample(regions, sum(swap_a), replace = TRUE)
  reg_mr <- home
  swap_m <- runif(n_rows) < 0.18
  reg_mr[swap_m] <- sample(regions, sum(swap_m), replace = TRUE)

  ## Alerts — moderately correlated; ~12% MH, ~8% SR, ~5% SW
  ## with positive correlation so multi-alert combos are common enough
  base_p   <- runif(n_persons, 0, 1)
  row_base <- rep(base_p, rows_per)
  mh <- ifelse(runif(n_rows) < (0.10 + 0.15 * row_base), "Yes", "No")
  sr <- ifelse(runif(n_rows) < (0.04 + 0.12 * row_base), "Yes", "No")
  sw <- ifelse(runif(n_rows) < (0.02 + 0.08 * row_base), "Yes", "No")

  np <- pmax(1L, rpois(n_rows, lambda = 25))

  out <- data.frame(
    EndFiscalYear              = yrs,
    UniqueIndividual_ID        = ids,
    Region_AtTimeOfPlacement   = reg_at,
    Region_MostRecentPlacement = reg_mr,
    Gender                     = gender,
    Age_Category               = age,
    MentalHealth_Alert         = mh,
    SuicideRisk_Alert          = sr,
    SuicideWatch_Alert         = sw,
    Number_Of_Placements       = np,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, out_path, row.names = FALSE)
  say("  Wrote ", n_rows, " synthetic rows (", n_persons,
      " person-years) to: ", out_path)
}

## Synthetic mode flag — set TRUE if synthetic data is generated.
SYNTHETIC_MODE <- FALSE

input_path <- NULL

if (!is.null(RDATA_ARG)) {
  if (!file.exists(RDATA_ARG)) {
    say("ERROR: --rdata path does not exist: ", RDATA_ARG)
    quit(status = 4)
  }
  input_path <- RDATA_ARG
  say("  Using --rdata: ", input_path)
} else {
  ## Probe defaults first — if we already have a copy, no need to ask.
  cands <- default_candidates()
  hits  <- cands[file.exists(cands)]
  if (length(hits) > 0L) {
    say("  Found a candidate file at:")
    say("    ", hits[1])
    if (ask_yn("  Use this file?", "Y", QUICK_MODE)) {
      input_path <- hits[1]
    }
  }

  if (is.null(input_path)) {
    ## --- Main menu ---
    say("")
    say("  The OTIS A01RCDD dataset is openly published by Ontario:")
    say("    https://data.ontario.ca/dataset/data-on-inmates-in-ontario")
    say("    File: 'Restrictive Confinement – Detailed Dataset (English)'")
    say("    Licence: Open Government Licence – Ontario")
    say("")
    say("  How would you like to proceed?")
    say("")
    menu_options <- c(
      "I already have the file on this computer — I'll tell you where",
      "Open the link and download it myself in a browser",
      "Have this script download it for me (needs internet)",
      "Run on SYNTHETIC fake data (no internet, no real data — testing only)",
      "Cancel and exit"
    )
    choice <- ask_menu("  Choose an option:", menu_options, 3L, QUICK_MODE)

    ## ------------- 1. I already have it -------------
    if (choice == 1L) {
      say("")
      say("  Where is the file located?")
      hint_dir <- ask_save_location(
        "  Pick the folder it's in:",
        default_key = "downloads",
        script_dir  = script_dir,
        quick       = QUICK_MODE
      )
      say("")
      say("  The file is usually named:")
      say("    a01_restrictive_confinement_detailed_dataset.csv")
      say("  (or whatever you renamed it to). It can also be the")
      say("  author's .RData workspace if you have that.")
      say("")
      default_name <- "a01_restrictive_confinement_detailed_dataset.csv"
      typed <- ask_path("  Enter the file name (just the name, not full path):",
                        default_name, QUICK_MODE)
      candidate <- file.path(hint_dir, typed)
      if (!file.exists(candidate)) {
        ## Last-resort: ask for full path
        say("  Couldn't find: ", candidate)
        candidate <- ask_path("  Enter the FULL path to the file:",
                              candidate, QUICK_MODE)
      }
      if (!file.exists(candidate)) {
        say("ERROR: file does not exist: ", candidate)
        quit(status = 4)
      }
      input_path <- normalizePath(candidate)
      say("  ✓ Using: ", input_path)
    }

    ## ------------- 2. Manual browser download -------------
    else if (choice == 2L) {
      say("")
      say("  Please open this URL in any web browser:")
      say("")
      say("    https://data.ontario.ca/dataset/data-on-inmates-in-ontario")
      say("")
      say("  On that page, find the resource named:")
      say("    'Restrictive Confinement – Detailed Dataset (English)'")
      say("  and click its Download/CSV button (≈ 4.5 MiB).")
      say("")
      say("  Browsers normally save downloads to your Downloads folder.")
      save_dir <- ask_save_location(
        "  Where will the file end up after you download it?",
        default_key = "downloads",
        script_dir  = script_dir,
        quick       = QUICK_MODE
      )
      say("")
      say("  When the download is complete, press Enter to continue.")
      if (!QUICK_MODE) {
        invisible(readLines(con = "stdin", n = 1, warn = FALSE))
      }
      default_name <- "a01_restrictive_confinement_detailed_dataset.csv"
      typed <- ask_path("  What is the downloaded file's name?",
                        default_name, QUICK_MODE)
      candidate <- file.path(save_dir, typed)
      if (!file.exists(candidate)) {
        say("  Couldn't find: ", candidate)
        ## Try common variations
        alts <- list.files(save_dir, pattern = "(?i)restrictive.*confinement.*\\.csv|a01.*RC.*\\.csv|a01_restrictive.*\\.csv",
                           full.names = TRUE)
        if (length(alts) > 0L) {
          say("  Found these CSV files that look like the right one:")
          for (a in alts) say("    ", a)
          if (ask_yn(paste0("  Use ", alts[1], "?"), "Y", QUICK_MODE)) {
            candidate <- alts[1]
          } else {
            candidate <- ask_path("  Enter the FULL path to the file:",
                                  candidate, QUICK_MODE)
          }
        } else {
          candidate <- ask_path("  Enter the FULL path to the file:",
                                candidate, QUICK_MODE)
        }
      }
      if (!file.exists(candidate)) {
        say("ERROR: file does not exist: ", candidate)
        quit(status = 4)
      }
      input_path <- normalizePath(candidate)
      say("  ✓ Using: ", input_path)
      ## Verify SHA256 if we have provenance
      if (!is.null(prov) && grepl("\\.csv$", input_path, ignore.case = TRUE)) {
        actual_sha <- sha256_file(input_path)
        if (actual_sha == prov$resource$sha256) {
          say("  ✓ SHA256 matches pinned provenance.")
        } else {
          say("  ! SHA256 differs — the file may be a newer release.")
          say("    Schema validation in otis_MRP.R will catch incompatibilities.")
        }
      }
    }

    ## ------------- 3. Auto-download via CKAN -------------
    else if (choice == 3L) {
      save_dir <- ask_save_location(
        "  Where should I save the downloaded CSV?",
        default_key = "bundle",
        script_dir  = script_dir,
        quick       = QUICK_MODE
      )
      url <- resolve_via_ckan()
      if (is.null(url)) {
        say("  CKAN lookup failed; using pinned URL from provenance.")
        url <- if (!is.null(prov)) prov$resource$direct_url else NULL
      }
      if (is.null(url)) {
        say("ERROR: no download URL available — provenance missing.")
        quit(status = 5)
      }
      target <- file.path(save_dir, "a01_restrictive_confinement_detailed_dataset.csv")
      say("  Downloading to: ", target)
      utils::download.file(url, target, mode = "wb", quiet = FALSE)
      input_path <- target
      if (!is.null(prov)) {
        actual_sha <- sha256_file(target)
        expected_sha <- prov$resource$sha256
        if (actual_sha == expected_sha) {
          say("  ✓ SHA256 matches pinned provenance (byte-for-byte).")
        } else {
          say("  ! SHA256 differs from pinned provenance.")
          say("    expected: ", expected_sha)
          say("    actual:   ", actual_sha)
          say("    (Ontario may have updated the dataset.)")
        }
      }
    }

    ## ------------- 4. Synthetic data -------------
    else if (choice == 4L) {
      say("")
      say("  *** SYNTHETIC DATA MODE ***")
      say("  This generates random fake data that matches the OTIS schema.")
      say("  The pipeline will run end-to-end so you can confirm everything")
      say("  works on your machine, but the numbers are NOT the paper's")
      say("  results and CANNOT be used to verify any claim. Every result")
      say("  is loudly watermarked as synthetic.")
      say("")
      if (!ask_yn("  Confirm — generate synthetic data and run on it?",
                  "N", QUICK_MODE)) {
        say("  Cancelled. Re-run when you're ready.")
        quit(status = 0)
      }
      save_dir <- ask_save_location(
        "  Where should I save the synthetic CSV?",
        default_key = "bundle",
        script_dir  = script_dir,
        quick       = QUICK_MODE
      )
      target <- file.path(save_dir, "SYNTHETIC_otis_data.csv")
      say("  Generating synthetic dataset (seed = ", SYNTHETIC_SEED, ")...")
      make_synthetic_otis_csv(target)
      input_path <- target
      SYNTHETIC_MODE <- TRUE
    }

    ## ------------- 5. Cancel -------------
    else {
      say("  Exiting at user request.")
      quit(status = 0)
    }
  }
}
hr()

## --------- 6. Run the analysis ---------

say("Step 3/5: Running otis_MRP.R...")
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
output_dir <- file.path(script_dir, paste0("results_", timestamp))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
say("  Input:   ", input_path)
say("  Output:  ", output_dir)
say("")
if (!ask_yn("  Start the analysis now?", "Y", QUICK_MODE)) {
  say("OK — exiting without running.")
  quit(status = 0)
}
hr()

log_path <- file.path(output_dir, "run.log")
rscript_bin <- file.path(R.home("bin"), if (OS_KIND == "windows") "Rscript.exe" else "Rscript")

if (isTRUE(SYNTHETIC_MODE)) {
  cat("\n")
  cat("##########################################################\n")
  cat("#                                                        #\n")
  cat("#   ⚠  SYNTHETIC DATA MODE — RESULTS ARE NOT REAL  ⚠     #\n")
  cat("#                                                        #\n")
  cat("#   The pipeline is running on randomly-generated data   #\n")
  cat("#   that matches the OTIS schema but contains NO real    #\n")
  cat("#   information. The numbers it produces CANNOT be       #\n")
  cat("#   used to verify the paper. Use this mode only to      #\n")
  cat("#   confirm the machinery works on your machine.         #\n")
  cat("#                                                        #\n")
  cat("##########################################################\n\n")
  Sys.setenv(OTIS_SYNTHETIC = "1")
}

## Invoke otis_MRP.R as a subprocess and tee output to the run.log.
## This keeps otis_MRP.R's own stdout/CSV writes independent.
run_args <- c(R_SCRIPT, shQuote(input_path), shQuote(output_dir))
exit_code <- system2(rscript_bin, run_args,
                     stdout = log_path, stderr = log_path,
                     env = if (isTRUE(SYNTHETIC_MODE)) "OTIS_SYNTHETIC=1" else character(0))

cat(readLines(log_path, warn = FALSE), sep = "\n")
hr()

## --------- 7. Parse manifest + open results folder ---------

say("Step 4/5: Results summary")
manifest_path <- file.path(output_dir, "manifest.json")
if (file.exists(manifest_path)) {
  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  statuses <- vapply(m$results, function(x) x$status, character(1))
  pass_n <- sum(statuses == "PASS")
  diff_n <- sum(statuses == "DIFFER")
  info_n <- sum(statuses == "INFO")
  total  <- length(statuses)
  say(sprintf("  Total:  %d", total))
  say(sprintf("  PASS:   %d", pass_n))
  say(sprintf("  DIFFER: %d", diff_n))
  say(sprintf("  INFO:   %d", info_n))
} else {
  say("  WARNING: manifest.json not produced.")
}
hr()

## Drop a plain-language SUMMARY.txt in the results folder.
summary_lines <- c(
  "##########################################################",
  "#                                                        #",
  "#   OTIS MRP REPRODUCIBILITY RUN — SUMMARY               #",
  "#                                                        #",
  "##########################################################",
  "",
  paste0("When:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste0("OS:     ", OS_KIND),
  paste0("R:      ", R.version.string),
  paste0("Mode:   ", if (isTRUE(SYNTHETIC_MODE))
                         "SYNTHETIC (not real data — pipeline check only)"
                     else "real OTIS data"),
  "",
  "----------------------------------------------------------",
  "  PATHS — exact absolute locations used in this run",
  "----------------------------------------------------------",
  paste0("Bundle folder:  ", script_dir),
  paste0("Input data:     ", input_path),
  paste0("Results folder: ", output_dir),
  paste0("R script:       ", R_SCRIPT),
  paste0("Provenance:     ", PROVENANCE_PATH),
  "",
  "----------------------------------------------------------",
  "  RESULT COUNTS",
  "----------------------------------------------------------",
  if (file.exists(manifest_path)) {
    c(paste0("Total checks: ", total),
      paste0("PASS:         ", pass_n),
      paste0("DIFFER:       ", diff_n),
      paste0("INFO:         ", info_n))
  } else "(manifest.json not produced — see run.log)",
  "",
  "----------------------------------------------------------",
  "  FILES IN THIS RESULTS FOLDER",
  "----------------------------------------------------------",
  paste0("  ", sort(list.files(output_dir))),
  "",
  "----------------------------------------------------------",
  "  WHAT WAS DONE",
  "----------------------------------------------------------",
  "1. Located the OTIS A01RCDD dataset (the source above).",
  "2. Built the person-year analytical panel.",
  "3. Computed full-sample and matched-sample descriptives.",
  "4. Fit the negative-binomial GLMM (glmmTMB nbinom2).",
  if (!isTRUE(SYNTHETIC_MODE) && grepl("\\.RData$", input_path, ignore.case = TRUE))
    "5. Cross-checked Double Machine Learning estimates."
  else
    "5. (DML estimates require the author's .RData; not run here.)",
  "6. Wrote every result to its own CSV (numbered 01..07).",
  "7. Wrote manifest.json with expected vs observed for every claim.",
  "",
  paste0("Contact: vsruhela@proton.me"),
  paste0("ORCID:   0009-0004-1750-3592"),
  "Licence: AGPL-3.0-or-later (scripts); OGL-Ontario (data)"
)
writeLines(summary_lines, file.path(output_dir, "SUMMARY.txt"))

say("Step 5/5: Done.")
say("  Results folder: ", output_dir)
say("  Plain-language summary written to: SUMMARY.txt in that folder.")
say("  Open the folder in your file browser to inspect everything.")

if (interactive() ||
    ask_yn("  Open the results folder now?", "Y", QUICK_MODE)) {
  if (OS_KIND == "macos") {
    system2("open", shQuote(output_dir))
  } else if (OS_KIND == "windows") {
    shell.exec(output_dir)
  } else {
    tryCatch(system2("xdg-open", shQuote(output_dir)),
             error = function(e) invisible(NULL))
  }
}

quit(status = exit_code)
