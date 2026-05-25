# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3LLL.13: user-facing VPD download walkthrough.
#
# VPD's GeoDASH Open Data portal gates downloads behind a manual
# terms-of-service accept + a per-(year,neighbourhood) popup-driven
# .zip download. There is no automation-friendly API. This helper
# prints step-by-step instructions so morie users know exactly what
# to do, what file to expect, and how to feed it back to morie.

#' Print step-by-step VPD GeoDASH download instructions
#'
#' VPD's GeoDASH Open Data portal (\url{https://geodash.vpd.ca/opendata/})
#' has no automation API: every download requires a manual click-
#' through of VPD's terms-of-use plus a popup-based file save per
#' (year, neighbourhood) selection. Calling
#' \code{morie_vpd_download_instructions()} prints the exact steps
#' the user needs to follow to get a full-fidelity crime CSV that
#' \code{\link{morie_datasets_vpd_crime}} can consume.
#'
#' @param to Optional file path. If supplied, the instructions are
#'   ALSO written to that file (so the user can read them outside R).
#'   Default `NULL` = print to console only.
#' @return Invisibly the instruction text (character vector, one
#'   line per element).
#' @seealso [morie_datasets_vpd_crime()] for the loader that
#'   accepts the downloaded file.
#' @examples
#' morie_vpd_download_instructions()
#' @export
morie_vpd_download_instructions <- function(to = NULL) {
  lines <- c(
    "============================================================",
    "  morie -- VPD GeoDASH crime data: how to get the full feed ",
    "============================================================",
    "",
    "VPD's open-data portal at https://geodash.vpd.ca/opendata/ has",
    "no automation API; you must download the file manually once.",
    "Bundled 550-row sample is loaded for you by default.",
    "",
    "----- 1. Browser setup -----",
    "  * Open https://geodash.vpd.ca/opendata/ in your browser.",
    "  * Allow pop-ups for that page (the download triggers a popup).",
    "  * Read + accept the disclaimer:",
    "      'I have read and understood the disclaimer above.'",
    "",
    "----- 2. Pick the data scope -----",
    "  * Year:          click '--All Years--' at the top of the list",
    "                   (covers 2003 - present).",
    "  * Neighbourhood: click '--All Neighbourhoods--' at the top",
    "                   (all 24 incl. Musqueam + Stanley Park).",
    "  * Click 'Download'.",
    "",
    "----- 3. The file you get -----",
    "  * Filename: crimedata_csv_AllNeighbourhoods_AllYears.zip",
    "  * Contains a single CSV:",
    "      crimedata_csv_AllNeighbourhoods_AllYears.csv",
    "  * Schema: 10 columns",
    "      TYPE, YEAR, MONTH, DAY, HOUR, MINUTE,",
    "      HUNDRED_BLOCK, NEIGHBOURHOOD, X, Y",
    "    (X / Y are UTM Zone 10 N, NAD83 / EPSG:26910.)",
    "",
    "----- 4. Load it into morie -----",
    "",
    "  # Option A: pass the zip path directly",
    "  df <- morie_datasets_vpd_crime(",
    "    zip_path     = '~/Downloads/crimedata_csv_AllNeighbourhoods_AllYears.zip',",
    "    accept_terms = TRUE",
    "  )",
    "",
    "  # Option B: unzip yourself, then pass the CSV path",
    "  df <- morie_datasets_vpd_crime(",
    "    csv_path     = '~/Downloads/crimedata_csv_AllNeighbourhoods_AllYears.csv',",
    "    accept_terms = TRUE",
    "  )",
    "",
    "  nrow(df)  # ~870,000 rows for All Years across All Neighbourhoods",
    "",
    "----- 5. Important interpretation caveats -----",
    "",
    "  * 'Offence Against a Person' is INTENTIONALLY aggregated to",
    "    protect PII. Do NOT try to disaggregate.",
    "  * VPD locations are DELIBERATELY randomized (person crimes)",
    "    or hundred-block (property crimes). Do NOT geocode rows.",
    "  * VPD uses 'All Offence + Founded' reporting -- NOT comparable",
    "    to Statistics Canada's UCR-MSO numbers.",
    "  * VPD updates the feed every Sunday morning. Re-download to",
    "    refresh; morie does not auto-pull.",
    "",
    "See ?morie_datasets_vpd_crime for the full data-quality section."
  )
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  if (!is.null(to)) {
    writeLines(lines, to)
    message(sprintf("Also written to: %s", to))
  }
  invisible(lines)
}
