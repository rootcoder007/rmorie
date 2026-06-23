#!/usr/bin/env bash
# run_otis_mrp.sh
#
# Interactive, supervisor-friendly reproducibility runner for the
# OTIS A01RCDD MRP analysis. Walks the user through:
#
#   1. Checking that R is installed
#   2. Locating the OTIS .RData input (or pointing to where to get it)
#   3. Installing missing R packages
#   4. Running otis_MRP.R end-to-end
#   5. Opening the timestamped results folder
#
# All numbered results are written to results_<TIMESTAMP>/ with a
# manifest.json that records expected vs observed for every claim.
#
# Vansh Singh Ruhela <vansh.ruhela@mail.utoronto.ca>
# ORCID 0009-0004-1750-3592
#
# USAGE:
#   ./run_otis_mrp.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
R_SCRIPT="${SCRIPT_DIR}/otis_MRP.R"
TEST_WRAPPER="${SCRIPT_DIR}/test_morie_otis_mrp.sh"

# Possible default locations to probe for the RData file.
DEFAULT_RDATA_CANDIDATES=(
  "/Volumes/VSR/rootcoderfiles/OTIS-RC/correctional_stats_report_environment1b.RData"
  "${HOME}/Desktop/OTIS-RC/correctional_stats_report_environment1b.RData"
  "${HOME}/Documents/OTIS-RC/correctional_stats_report_environment1b.RData"
  "${HOME}/OTIS-RC/correctional_stats_report_environment1b.RData"
  "${SCRIPT_DIR}/correctional_stats_report_environment1b.RData"
)

# ============================================================
# helpers
# ============================================================
say()  { printf '%s\n' "$*"; }
hr()   { printf -- '----------------------------------------------------------\n'; }
ask_yn() {
  # ask_yn "Prompt text? [Y/n]" default_answer(Y|N)
  local prompt="$1"
  local default="${2:-Y}"
  local hint="[Y/n]"
  [[ "${default}" == "N" ]] && hint="[y/N]"
  local reply
  while true; do
    read -r -p "${prompt} ${hint} " reply || reply=""
    reply="${reply:-${default}}"
    case "${reply}" in
      [Yy]|[Yy][Ee][Ss]) return 0 ;;
      [Nn]|[Nn][Oo])     return 1 ;;
      *) say "Please answer y or n." ;;
    esac
  done
}
ask_path() {
  # ask_path "Prompt" "default_path"
  local prompt="$1"
  local default="$2"
  local reply
  read -r -p "${prompt} [${default}] " reply || reply=""
  printf '%s\n' "${reply:-${default}}"
}

# ============================================================
# 0. Banner
# ============================================================
cat <<BANNER
==========================================================
  OTIS A01RCDD Reproducibility Runner
  Paper: Alert Complexity and Placement Volatility in
         Ontario Restrictive Confinement Data
  Author: Vansh Singh Ruhela (U of T Centre for Criminology)
==========================================================

This script will walk you through running the full
reproducibility check for the MRP analysis. It will:

  1. Check that R is installed.
  2. Ask where your OTIS .RData file lives (or how to get it).
  3. Install any missing R packages.
  4. Run the analysis end-to-end.
  5. Save every result as a CSV plus a manifest.json file
     that records expected vs observed values.

You'll be asked yes/no questions along the way.

BANNER

if ! ask_yn "Ready to begin?" Y; then
  say "OK — exiting. Re-run anytime."
  exit 0
fi
hr

# ============================================================
# 1. Sanity-check companion files
# ============================================================
if [[ ! -f "${R_SCRIPT}" ]]; then
  say "ERROR: otis_MRP.R not found next to this script."
  say "  Expected: ${R_SCRIPT}"
  say "  Make sure otis_MRP.R is in the same folder as run_otis_mrp.sh."
  exit 2
fi
if [[ ! -f "${TEST_WRAPPER}" ]]; then
  say "ERROR: test_morie_otis_mrp.sh not found next to this script."
  say "  Expected: ${TEST_WRAPPER}"
  exit 2
fi

# ============================================================
# 2. R / Rscript detection
# ============================================================
say "Step 1/5: Checking that R is installed..."
RSCRIPT="$(command -v Rscript 2>/dev/null || true)"
if [[ -z "${RSCRIPT}" ]]; then
  for cand in /opt/homebrew/bin/Rscript /usr/local/bin/Rscript /usr/bin/Rscript; do
    [[ -x "${cand}" ]] && RSCRIPT="${cand}" && break
  done
fi

if [[ -z "${RSCRIPT}" ]]; then
  say ""
  say "  R is NOT installed (or not on PATH)."
  say ""
  say "  To install R:"
  say "    macOS:        brew install --cask r"
  say "                  or download from https://cran.r-project.org/bin/macosx/"
  say "    Linux (Ubuntu/Debian):  sudo apt install r-base"
  say "    Windows:      https://cran.r-project.org/bin/windows/base/"
  say ""
  say "  After installing, re-run this script."
  exit 3
fi

R_VERSION="$("${RSCRIPT}" --version 2>&1 | head -1)"
say "  Found: ${RSCRIPT}"
say "         ${R_VERSION}"
hr

# ============================================================
# 3. Locate the OTIS .RData file
# ============================================================
say "Step 2/5: Locating the OTIS A01RCDD dataset..."
say ""
say "This analysis needs the file:"
say "    correctional_stats_report_environment1b.RData"
say ""
say "It contains the OTIS A01RCDD (Restrictive Confinement Detailed"
say "Dataset) extract from the Ontario Ministry of the Solicitor"
say "General, accompanying the published OTIS Stats Report."
say ""

if ! ask_yn "Do you already have this .RData file on this machine?" Y; then
  say ""
  say "  No problem. The dataset is restricted-access and is not"
  say "  hosted publicly. To obtain it, please:"
  say ""
  say "    1. If you are a U of T collaborator on this MRP, contact"
  say "       Vansh Singh Ruhela <vansh.ruhela@mail.utoronto.ca>"
  say "       (ORCID 0009-0004-1750-3592) to request the secure"
  say "       transfer."
  say ""
  say "    2. For background and provenance, see the published"
  say "       OTIS Stats Report from the Ontario Ministry of the"
  say "       Solicitor General."
  say ""
  say "  Once you have the .RData file, re-run this script and"
  say "  answer 'y' to this question."
  exit 0
fi

# Probe default locations
FOUND_DEFAULT=""
for cand in "${DEFAULT_RDATA_CANDIDATES[@]}"; do
  if [[ -f "${cand}" ]]; then
    FOUND_DEFAULT="${cand}"
    break
  fi
done

if [[ -n "${FOUND_DEFAULT}" ]]; then
  say ""
  say "  Found a candidate file at:"
  say "    ${FOUND_DEFAULT}"
  if ask_yn "  Use this file?" Y; then
    INPUT_RDATA="${FOUND_DEFAULT}"
  else
    INPUT_RDATA="$(ask_path "  Enter full path to the .RData file:" "${FOUND_DEFAULT}")"
  fi
else
  say ""
  say "  Couldn't auto-detect the file in the usual locations."
  INPUT_RDATA="$(ask_path "  Enter full path to the .RData file:" "${HOME}/correctional_stats_report_environment1b.RData")"
fi

if [[ ! -f "${INPUT_RDATA}" ]]; then
  say ""
  say "ERROR: file does not exist:"
  say "    ${INPUT_RDATA}"
  say "Double-check the path and re-run."
  exit 4
fi
say "  Using: ${INPUT_RDATA}"
hr

# ============================================================
# 4. R package installation
# ============================================================
say "Step 3/5: R packages..."
say ""
say "The analysis needs these packages:"
say "    data.table, MatchIt, glmmTMB, lme4, DHARMa, Hmisc, jsonlite"
say ""

if ask_yn "Check for missing packages and install them now?" Y; then
  say "  Running package check (this may take a few minutes the first time)..."
  "${RSCRIPT}" -e '
    pkgs <- c("data.table", "MatchIt", "glmmTMB", "lme4", "DHARMa", "Hmisc", "jsonlite")
    installed <- rownames(installed.packages())
    missing <- setdiff(pkgs, installed)
    if (length(missing) == 0L) {
      cat("  All required packages already installed.\n")
    } else {
      cat("  Installing:", paste(missing, collapse = ", "), "\n")
      install.packages(missing, repos = "https://cloud.r-project.org")
      still_missing <- setdiff(missing, rownames(installed.packages()))
      if (length(still_missing) > 0L) {
        cat("  WARNING: failed to install:", paste(still_missing, collapse = ", "), "\n")
        quit(status = 1)
      }
      cat("  All packages installed OK.\n")
    }
  ' || {
    say ""
    say "WARNING: package installation hit an error."
    if ! ask_yn "Continue anyway?" N; then
      exit 5
    fi
  }
else
  say "  Skipping package check. (If a package is missing, otis_MRP.R will fail.)"
fi
hr

# ============================================================
# 5. Run the analysis
# ============================================================
say "Step 4/5: Running the analysis..."
say ""
say "Calling: ${TEST_WRAPPER}"
say "Input:   ${INPUT_RDATA}"
say ""
if ! ask_yn "Start the run now?" Y; then
  say "OK — exiting without running. Re-run when ready."
  exit 0
fi
hr

# Delegate to the existing test wrapper (it handles timestamps,
# CSV outputs, manifest.json, and PASS/DIFFER summary).
set +e
"${TEST_WRAPPER}" "${INPUT_RDATA}"
RUN_EXIT=$?
set -e

hr

# ============================================================
# 6. Open results
# ============================================================
say "Step 5/5: Results"
say ""

# Find the newest results folder created by the wrapper
LATEST_RESULTS="$(ls -1dt "${SCRIPT_DIR}"/results_* 2>/dev/null | head -1 || true)"

if [[ -n "${LATEST_RESULTS}" && -d "${LATEST_RESULTS}" ]]; then
  say "Results folder: ${LATEST_RESULTS}"
  say ""
  say "Contents:"
  ls -la "${LATEST_RESULTS}" | sed 's/^/    /'
  say ""
  if ask_yn "Open this folder in your file browser?" Y; then
    if command -v open >/dev/null 2>&1; then
      open "${LATEST_RESULTS}"
    elif command -v xdg-open >/dev/null 2>&1; then
      xdg-open "${LATEST_RESULTS}"
    else
      say "  (No GUI opener found. Path is above — navigate to it manually.)"
    fi
  fi
else
  say "WARNING: couldn't locate a results_* folder."
fi

hr
if [[ ${RUN_EXIT} -eq 0 ]]; then
  say "Done. The analysis ran successfully."
else
  say "Done. The analysis exited with code ${RUN_EXIT} — see run.log for details."
fi
say ""
say "Questions? Contact Vansh Singh Ruhela <vansh.ruhela@mail.utoronto.ca>"
exit ${RUN_EXIT}
