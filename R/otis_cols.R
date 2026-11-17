# SPDX-License-Identifier: AGPL-3.0-or-later

#' Internal helper: the OTIS column map (old name, new name per table)
#'
#' Ontario republished the OTIS files on 2026-10-30 with every column
#' renamed. \code{inst/extdata/otis_colmap.csv} holds the pairs, one row
#' per column of each of the 29 tables.
#' @noRd
.otis_colmap <- local({
  map <- NULL
  function() {
    if (is.null(map)) {
      map <<- utils::read.csv(
        system.file("extdata", "otis_colmap.csv", package = "rmorie"),
        stringsAsFactors = FALSE)
    }
    map
  }
})

#' Internal helper: bring an OTIS table to the column names the code expects
#'
#' Each expected name is looked up, in order, as the exact name; then as
#' a name equal to it without regard to case or underscores; then as the
#' new name Ontario gave it (\code{.otis_colmap()}). The column found is
#' renamed to the expected name. Two columns that resolve to one expected
#' name are refused, naming both. A name in \code{need} still missing
#' after that is an error that names it and the names the table has.
#'
#' @param data A data frame.
#' @param need Names that must be present.
#' @param want Names to resolve when present.
#' @param table Optional table id (\code{"b01"}): adds every name of that
#'   table to \code{want}.
#' @param what Label for messages.
#' @return \code{data} with the resolved columns renamed.
#' @noRd
.otis_cols <- function(data, need = character(), want = character(),
                       table = NULL, what = NULL) {
  if (!is.data.frame(data)) stop("data must be a data frame", call. = FALSE)
  map <- .otis_colmap()
  what <- what %||% if (is.null(table)) "the OTIS table" else
    sprintf("the OTIS %s table", table)
  key <- function(x) tolower(gsub("_", "", x, fixed = TRUE))
  have <- names(data)
  hk <- key(have)
  taken <- logical(length(have))
  miss <- character()
  for (e in unique(c(need, want, map$old[map$table %in% table]))) {
    hit <- which(have == e | hk == key(e) | hk %in% key(map$new[map$old == e]))
    if (length(hit) > 1L) {
      stop(sprintf("%s: columns %s all resolve to %s; keep one",
                   what, paste(have[hit], collapse = " and "), e),
           call. = FALSE)
    }
    if (length(hit) == 0L || taken[hit]) {
      if (e %in% need && !(e %in% names(data))) miss <- c(miss, e)
      next
    }
    taken[hit] <- TRUE
    names(data)[hit] <- e
  }
  if (length(miss)) {
    stop(sprintf("%s: missing column(s): %s; the table has: %s",
                 what, paste(miss, collapse = ", "),
                 paste(have, collapse = ", ")), call. = FALSE)
  }
  data
}
