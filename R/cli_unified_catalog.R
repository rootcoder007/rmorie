# SPDX-License-Identifier: AGPL-3.0-or-later

#' Emit a unified catalog CSV for the rmorie-cli binary
#'
#' Walks `morie_datasets_browse()` (9242 datasets across all portals)
#' and writes a single CSV with the columns the C++ `rmorie` binary's
#' catalog parser expects: `dataset_key`, `portal`, `title`,
#' `description`, `url`, `license`, `formats`.
#'
#' The CLI binary prefers this unified file when present; otherwise it
#' falls back to scanning every `inst/extdata/*_catalog.csv` (which
#' have heterogeneous per-portal schemas and don't parse cleanly).
#'
#' Idempotent: regenerate after any change to the catalog registry.
#'
#' @param out_path Where to write. Defaults to
#'   `inst/extdata/_unified_catalog.csv` inside the installed package
#'   (or the dev tree if running under `devtools::load_all()`).
#' @return Invisibly, the path written.
#' @examples
#' out <- morie_cli_dump_catalog(out_path = tempfile(fileext = ".csv"))
#' file.exists(out)
#' @export
morie_cli_dump_catalog <- function(out_path = NULL) {
  if (is.null(out_path)) {
    extdata <- system.file("extdata", package = "rmorie")
    if (!nzchar(extdata)) {
      # dev tree fallback
      extdata <- file.path(getwd(), "inst", "extdata")
    }
    out_path <- file.path(extdata, "_unified_catalog.csv")
  }

  df <- morie_datasets_browse()

  # The browse df has: dataset_key, source, id, api_modes, loader,
  # dict_url, n_rows_bundled. Map to CLI schema.
  unified <- data.frame(
    dataset_key = df$dataset_key,
    portal      = df$source,
    title       = ifelse(is.na(df$id) | !nzchar(df$id),
                         df$dataset_key, df$id),
    description = ifelse(is.na(df$loader), "",
                         paste0("loader=", df$loader)),
    url         = ifelse(is.na(df$dict_url), "", df$dict_url),
    license     = "",
    formats     = ifelse(is.na(df$api_modes), "", df$api_modes),
    stringsAsFactors = FALSE
  )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(unified, out_path, row.names = FALSE, na = "")
  invisible(out_path)
}
