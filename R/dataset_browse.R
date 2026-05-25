# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD4: interactive browser for the cross-portal dataset
# catalog (built atop morie_dataset_portal_catalog() from 3CCC4).

#' Browse + filter the morie cross-portal dataset catalog
#'
#' Phase 3DDD4. Convenience wrapper over
#' [morie_dataset_portal_catalog()] that lets callers filter +
#' search by keyword, portal, api_mode, or loader pattern without
#' writing subset expressions by hand.
#'
#' Filters compose with AND semantics. A `keyword` matches against
#' `dataset_key` + `id` + `loader` (case-insensitive). To match
#' anywhere including the dict URL, pass `keyword_includes_url =
#' TRUE`.
#'
#' @param keyword Optional case-insensitive substring to grep against
#'   `dataset_key`/`id`/`loader`. `NULL` (default) skips this filter.
#' @param portal Optional portal name (see
#'   [morie_dataset_portal_catalog()] for the canonical list).
#'   Accepts a character vector for multi-portal queries.
#' @param api_mode Optional API mode substring to match against the
#'   `api_modes` column (e.g., `"soda3"`, `"arcgis"`,
#'   `"opendatasoft"`, `"statcan_wds"`, `"manual_download"`).
#'   Accepts a character vector.
#' @param loader_pattern Optional perl-style regex against the
#'   `loader` column (e.g., `"^morie_datasets_tps"`).
#' @param keyword_includes_url If `TRUE`, also greps the `dict_url`.
#' @param sort_by Sort order: `"dataset_key"` (default), `"source"`,
#'   `"n_rows_bundled"` (descending), or `"id"`.
#' @return A `data.frame` -- the filtered subset of the catalog
#'   with the same 7-column schema.
#' @examples
#' # All TPS datasets, alphabetical (offline; reads the cached
#' # cross-portal catalog -- no network).
#' tps <- morie_datasets_browse(portal = "tps_arcgis_hub")
#' nrow(tps)
#'
#' # Anything mentioning "homicide"
#' h <- morie_datasets_browse(keyword = "homicide")
#' head(h$dataset_key)
#' @export
morie_datasets_browse <- function(keyword = NULL,
                                    portal = NULL,
                                    api_mode = NULL,
                                    loader_pattern = NULL,
                                    keyword_includes_url = FALSE,
                                    sort_by = c("dataset_key", "source",
                                                 "n_rows_bundled", "id")) {
  sort_by <- match.arg(sort_by)
  d <- morie_dataset_portal_catalog()

  if (!is.null(portal)) {
    d <- d[d$source %in% portal, , drop = FALSE]
  }
  if (!is.null(api_mode)) {
    keep <- rep(FALSE, nrow(d))
    for (m in api_mode) {
      keep <- keep | grepl(m, d$api_modes, ignore.case = TRUE,
                            fixed = FALSE)
    }
    d <- d[keep, , drop = FALSE]
  }
  if (!is.null(loader_pattern)) {
    d <- d[grepl(loader_pattern, d$loader, perl = TRUE), ,
            drop = FALSE]
  }
  if (!is.null(keyword)) {
    haystack <- paste(d$dataset_key, d$id, d$loader,
                       if (isTRUE(keyword_includes_url))
                         ifelse(is.na(d$dict_url), "", d$dict_url)
                       else "")
    d <- d[grepl(keyword, haystack, ignore.case = TRUE,
                  perl = TRUE), , drop = FALSE]
  }

  if (sort_by == "n_rows_bundled") {
    d <- d[order(-d$n_rows_bundled, d$dataset_key,
                  method = "radix",
                  na.last = TRUE), , drop = FALSE]
  } else {
    d <- d[order(d[[sort_by]]), , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

#' Tally the cross-portal catalog by source + api_mode
#'
#' Phase 3DDD4. Returns a compact summary of how many datasets
#' each portal contributes + which API protocols are used. Useful
#' as a quick "what does morie ship?" smoke test.
#'
#' @return A `data.frame` with one row per portal: `source`,
#'   `n_datasets`, `api_modes`, `n_with_bundled_fixture`.
#' @examples
#' morie_datasets_summary()
#' @export
morie_datasets_summary <- function() {
  d <- morie_dataset_portal_catalog()
  by_src <- split(d, d$source)
  rows <- lapply(names(by_src), function(s) {
    g <- by_src[[s]]
    data.frame(
      source = s,
      n_datasets = nrow(g),
      api_modes = paste(sort(unique(unlist(strsplit(g$api_modes, ",")))),
                          collapse = ","),
      n_with_bundled_fixture = sum(!is.na(g$n_rows_bundled)),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$n_datasets), , drop = FALSE]
  rownames(out) <- NULL
  out
}
