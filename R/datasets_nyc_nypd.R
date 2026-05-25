# SPDX-License-Identifier: AGPL-3.0-or-later
#
# NYC NYPD criminal-justice Socrata wrappers.
#
# Eight canonical NYPD datasets published under the NYC OpenData
# program (data.cityofnewyork.us). Resource ids verified live via
# https://api.us.socrata.com/api/catalog/v1?q=NYPD&domains=data.cityofnewyork.us
# during phase 3NN. Each loader follows the offline-default pattern
# established in phase 3LL:
#
#   offline = TRUE  (default) -> read bundled inst/extdata/*.csv
#   offline = FALSE            -> hit SODA2 endpoint via the mockable
#                                  .morie_dataset_socrata_fetch helper
#
# Live mode honours an explicit resource_id override; otherwise the
# canonical id from .MORIE_NYC_NYPD_REGISTRY is used.

# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

.MORIE_NYC_NYPD_REGISTRY <- list(
  nypd_arrests_historic = list(
    resource_id = "8h9b-rp9u",
    label = "NYPD Arrests Data (Historic)",
    fixture = "nypd_arrests_historic_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/8h9b-rp9u",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_arrests_ytd = list(
    resource_id = "uip8-fykc",
    label = "NYPD Arrest Data (Year to Date)",
    fixture = "nypd_arrests_ytd_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/uip8-fykc",
    data_dictionary_url = paste0(
      "https://data.cityofnewyork.us/api/views/uip8-fykc/files/",
      "f0dbff24-5794-4034-a52d-b091e8dd61a8?download=true",
      "&filename=NYPD_Arrest_YTD_DataDictionary.xlsx"),
    footnotes_url = paste0(
      "https://data.cityofnewyork.us/api/views/uip8-fykc/files/",
      "62a746df-66ca-4603-aae4-46c02bac2972?download=true",
      "&filename=NYPD_Arrest_Incident_Level_Data_Footnotes.pdf")),
  nypd_complaint_historic = list(
    resource_id = "qgea-i56i",
    label = "NYPD Complaint Data Historic",
    fixture = "nypd_complaint_historic_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/qgea-i56i",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_complaint_ytd = list(
    resource_id = "5uac-w243",
    label = "NYPD Complaint Data Current (Year To Date)",
    fixture = "nypd_complaint_ytd_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/5uac-w243",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_hate_crimes = list(
    resource_id = "bqiq-cu78",
    label = "NYPD Hate Crimes",
    fixture = "nypd_hate_crimes_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/bqiq-cu78",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_uof_incidents = list(
    resource_id = "f4tj-796d",
    label = "NYPD Use of Force Incidents",
    fixture = "nypd_uof_incidents_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/f4tj-796d",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_uof_subjects = list(
    resource_id = "dufe-vxb7",
    label = "NYPD Use of Force: Subjects",
    fixture = "nypd_uof_subjects_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/dufe-vxb7",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_),
  nypd_vehicle_stops = list(
    resource_id = "hn9i-dwpr",
    label = "NYPD Vehicle Stop Reports",
    fixture = "nypd_vehicle_stops_sample.csv",
    permalink = "https://data.cityofnewyork.us/d/hn9i-dwpr",
    data_dictionary_url = NA_character_,
    footnotes_url = NA_character_))

#' List the NYPD criminal-justice Socrata datasets wrapped by morie
#'
#' @return A `data.frame` with 8 columns:
#'   `dataset_key`, `label`, `resource_id`, `resource_url`,
#'   `permalink` (`data.cityofnewyork.us/d/<id>` stable redirect),
#'   `data_dictionary_url` (XLSX, when published as a dataset
#'   attachment; `NA_character_` otherwise),
#'   `footnotes_url` (PDF, when published; `NA_character_` otherwise),
#'   `fixture` (bundled-fixture filename).
#'
#' Currently only `nypd_arrests_ytd` carries the
#' canonical NYC OpenData attachment URLs (XLSX dictionary + PDF
#' footnotes). The other 7 entries leave those slots `NA`; PRs
#' welcome to fill them in when the asset UUIDs are looked up at
#' the dataset's landing page.
#'
#' @export
morie_datasets_nyc_nypd_layers <- function() {
  rows <- lapply(names(.MORIE_NYC_NYPD_REGISTRY), function(k) {
    e <- .MORIE_NYC_NYPD_REGISTRY[[k]]
    data.frame(
      dataset_key = k, label = e$label,
      resource_id = e$resource_id,
      resource_url = sprintf(
        "https://data.cityofnewyork.us/resource/%s.json",
        e$resource_id),
      permalink = e$permalink,
      data_dictionary_url = e$data_dictionary_url,
      footnotes_url = e$footnotes_url,
      fixture = e$fixture,
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Socrata default-API-cap note + pagination wiring
#'
#' All NYC OpenData SODA2 endpoints apply a default cap of 1,000 rows
#' per request unless an explicit `$limit` (or `$$app_token` for
#' authenticated requests) is supplied. For the NYPD CJ datasets
#' wrapped here that means:
#'
#'   * `morie_datasets_nyc_nypd_arrests_ytd(offline = FALSE)` returns
#'     **only 1,000 rows** by default, even though the live feed
#'     carries ~69,300 rows.
#'   * Pass `max_features = N` to lift the single-request cap to N
#'     rows (Socrata enforces a hard server-side cap of 50,000 rows
#'     per request).
#'   * **Pagination (wired in 3OO).** For full pulls over the cap,
#'     pass `paginate = TRUE`. morie walks SODA2 `$offset` in
#'     `page_size`-row chunks until the server returns a short page
#'     (exhausted) or `max_features` is reached. Without an app_token
#'     the per-request ceiling is 1,000 rows so `page_size = 1000` is
#'     the default; with `page_size = 50000` + `app_token` you can
#'     pull the full ~69K-row arrests_ytd feed in two requests.
#'     `max_pages` (default 200) is a safety net against runaway pulls.
#'
#' Worked example:
#'
#' ```r
#' # Full live pull of the YTD arrests feed (~69K rows over ~70 pages).
#' df <- morie_datasets_nyc_nypd_arrests_ytd(
#'   offline = FALSE, paginate = TRUE)
#'
#' # First 5,000 rows only (5 paged requests of 1,000 each).
#' df <- morie_datasets_nyc_nypd_arrests_ytd(
#'   offline = FALSE, paginate = TRUE, max_features = 5000L)
#' ```
#'
#' The bundled fixtures (offline mode) are unaffected -- they ship 5
#' rows each as deterministic sample data, and `max_features` simply
#' truncates the fixture.
#'
#' @name morie_nyc_nypd_socrata_cap_note
NULL

# ---------------------------------------------------------------------------
# Shared factory
# ---------------------------------------------------------------------------

.morie_nyc_nypd_dispatch <- function(dataset_key, year, max_features,
                                       offline, resource_id,
                                       mode = c("soda2", "soda3"),
                                       paginate = FALSE,
                                       page_size = 1000L,
                                       max_pages = 200L,
                                       app_token = NULL) {
  mode <- match.arg(mode)
  if (!(dataset_key %in% names(.MORIE_NYC_NYPD_REGISTRY))) {
    stop(sprintf(paste0(
      "unknown NYC NYPD dataset_key '%s'. Available: %s"),
      dataset_key,
      paste(names(.MORIE_NYC_NYPD_REGISTRY), collapse = ", ")),
      call. = FALSE)
  }
  entry <- .MORIE_NYC_NYPD_REGISTRY[[dataset_key]]
  if (isTRUE(offline)) {
    path <- system.file("extdata", entry$fixture, package = "morie")
    if (!nzchar(path)) {
      stop(sprintf("bundled NYC NYPD fixture %s missing",
                   entry$fixture), call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- entry$resource_id
  # Build the year-filter clause using the right date column per
  # dataset_key. Same logic across SODA2 + SODA3 paths.
  year_clause <- NULL
  if (!is.null(year)) {
    year_col <- switch(dataset_key,
      "nypd_arrests_historic"   = "arrest_date",
      "nypd_arrests_ytd"        = "arrest_date",
      "nypd_complaint_historic" = "cmplnt_fr_dt",
      "nypd_complaint_ytd"      = "cmplnt_fr_dt",
      "nypd_hate_crimes"        = "complaint_year_number",
      "nypd_uof_incidents"      = "occurrence_date",
      "nypd_uof_subjects"       = NULL,
      "nypd_vehicle_stops"      = "occur_dt",
      NULL)
    if (!is.null(year_col)) {
      year_clause <- if (year_col == "complaint_year_number") {
        sprintf("%s = %d", year_col, as.integer(year))
      } else {
        sprintf("date_extract_y(%s) = %d", year_col, as.integer(year))
      }
    }
  }
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofnewyork.us/resource/%s.json",
                   resource_id)
    return(.morie_dataset_socrata_fetch(
      url, where = year_clause,
      max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  soql <- if (is.null(year_clause)) {
    "SELECT *"
  } else {
    sprintf("SELECT * WHERE %s", year_clause)
  }
  .morie_dataset_soda3_query(
    resource_id, soql = soql,
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features,
    base_url = "https://data.cityofnewyork.us")
}

#' Generic NYC NYPD dataset loader by registry key
#'
#' @param dataset_key One of the keys in
#'   [morie_datasets_nyc_nypd_layers()].
#' @param year Optional year filter (server-side SoQL).
#' @param max_features Optional row cap. When `paginate = TRUE` this
#'   is the total cap across walked pages.
#' @param offline If `TRUE` (default), read the bundled fixture.
#' @param resource_id Optional Socrata resource id override.
#' @param paginate Logical; if `TRUE` and `offline = FALSE`, walk
#'   SODA2 `$offset` in `page_size` chunks until exhausted or
#'   `max_features` is reached. Default `FALSE` (single 1,000-row
#'   request, matching the historical pre-3OO behaviour).
#' @param page_size Per-page row count when paginating (default 1,000,
#'   the unauthenticated SODA2 ceiling).
#' @param max_pages Safety net on paginated walks (default 200 ->
#'   up to 200,000 rows without an app_token).
#' @return A `data.frame`.
#' @export
morie_datasets_nyc_nypd_by_key <- function(dataset_key,
                                             year = NULL,
                                             max_features = NULL,
                                             offline = TRUE,
                                             resource_id = NULL,
                                             paginate = FALSE,
                                             page_size = 1000L,
                                             max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch(dataset_key, year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

# ---------------------------------------------------------------------------
# Eight per-dataset wrappers
# ---------------------------------------------------------------------------
#
# All 8 share an identical signature -- year + max_features + offline +
# resource_id (existing) plus the 3-arg pagination tail wired in 3OO
# (paginate + page_size + max_pages). They forward verbatim to
# .morie_nyc_nypd_dispatch().

#' NYPD Arrests Data (Historic)
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_arrests_historic <- function(year = NULL,
                                                       max_features = NULL,
                                                       offline = TRUE,
                                                       resource_id = NULL,
                                                       paginate = FALSE,
                                                       page_size = 1000L,
                                                       max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_arrests_historic", year,
                             max_features, offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Arrest Data (Year to Date)
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_arrests_ytd <- function(year = NULL,
                                                  max_features = NULL,
                                                  offline = TRUE,
                                                  resource_id = NULL,
                                                  paginate = FALSE,
                                                  page_size = 1000L,
                                                  max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_arrests_ytd", year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Complaint Data Historic
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_complaint_historic <- function(year = NULL,
                                                         max_features = NULL,
                                                         offline = TRUE,
                                                         resource_id = NULL,
                                                         paginate = FALSE,
                                                         page_size = 1000L,
                                                         max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_complaint_historic", year,
                             max_features, offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Complaint Data Current (Year To Date)
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_complaint_ytd <- function(year = NULL,
                                                    max_features = NULL,
                                                    offline = TRUE,
                                                    resource_id = NULL,
                                                    paginate = FALSE,
                                                    page_size = 1000L,
                                                    max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_complaint_ytd", year,
                             max_features, offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Hate Crimes
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_hate_crimes <- function(year = NULL,
                                                  max_features = NULL,
                                                  offline = TRUE,
                                                  resource_id = NULL,
                                                  paginate = FALSE,
                                                  page_size = 1000L,
                                                  max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_hate_crimes", year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Use of Force Incidents
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_uof_incidents <- function(year = NULL,
                                                    max_features = NULL,
                                                    offline = TRUE,
                                                    resource_id = NULL,
                                                    paginate = FALSE,
                                                    page_size = 1000L,
                                                    max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_uof_incidents", year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Use of Force: Subjects
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_uof_subjects <- function(year = NULL,
                                                   max_features = NULL,
                                                   offline = TRUE,
                                                   resource_id = NULL,
                                                   paginate = FALSE,
                                                   page_size = 1000L,
                                                   max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_uof_subjects", year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

#' NYPD Vehicle Stop Reports
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @export
morie_datasets_nyc_nypd_vehicle_stops <- function(year = NULL,
                                                    max_features = NULL,
                                                    offline = TRUE,
                                                    resource_id = NULL,
                                                    paginate = FALSE,
                                                    page_size = 1000L,
                                                    max_pages = 200L,
                                                       mode = c("soda2", "soda3"),
                                                       app_token = NULL) {
  .morie_nyc_nypd_dispatch("nypd_vehicle_stops", year, max_features,
                             offline, resource_id,
                             paginate = paginate,
                             page_size = page_size,
                             max_pages = max_pages,
                             mode = mode,
                             app_token = app_token)
}

# ---------------------------------------------------------------------------
# NYC NYPD foreign-key resolvers (3AAA)
# ---------------------------------------------------------------------------

#' NYC Police Precincts boundary layer (`y76i-bdw7`)
#'
#' Wraps the NYC OpenData "Police Precincts" feed (77 precincts +
#' the special precinct 22 / Central Park alias = 78 rows in this
#' fixture). Used as the resolver for the `arrest_precinct` /
#' `addr_pct_cd` / `complaint_precinct_code` foreign keys on every
#' NYPD CJ dataset (3NN).
#'
#' Attribute schema: `precinct` (string, "1"-"123"), `shape_leng`,
#' `shape_area`. Live mode also returns `the_geom` MultiPolygon
#' when `geometry = TRUE`.
#'
#' @inheritParams morie_datasets_chicago_wards
#' @return A `data.frame`.
#' @export
morie_datasets_nyc_police_precincts <- function(offline = TRUE,
                                                  geometry = FALSE,
                                                  max_features = NULL,
                                                  resource_id = NULL,
                                                  mode = c("soda2", "soda3"),
                                                  paginate = FALSE,
                                                  page_size = 1000L,
                                                  max_pages = 200L,
                                                  app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "nyc_police_precincts.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled NYC police precincts fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(precinct = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "y76i-bdw7"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofnewyork.us/resource/%s.json",
                   resource_id)
    if (!isTRUE(geometry)) {
      url <- paste0(url, "?$select=precinct,shape_leng,shape_area")
    }
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  select_clause <- if (isTRUE(geometry)) "*" else "precinct, shape_leng, shape_area"
  .morie_dataset_soda3_query(
    resource_id,
    soql = sprintf("SELECT %s", select_clause),
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features,
    base_url = "https://data.cityofnewyork.us")
}

#' NYC Borough Boundaries (`gthc-hcne`)
#'
#' Wraps the NYC OpenData "Borough Boundaries" feed (5 NYC
#' boroughs: Manhattan, Bronx, Brooklyn, Queens, Staten Island).
#' Used as the resolver for the `arrest_boro` (1-letter codes) /
#' `boro_nm` (full names) / `patrol_borough_name` foreign keys on
#' NYPD CJ datasets (3NN).
#'
#' Attribute schema: `borocode` (string, "1"-"5"), `boroname`
#' (capitalised name), `shape_area`, `shape_leng`. Live mode also
#' returns `the_geom` MultiPolygon when `geometry = TRUE`.
#'
#' @inheritParams morie_datasets_nyc_police_precincts
#' @return A `data.frame`.
#' @export
morie_datasets_nyc_boroughs <- function(offline = TRUE,
                                          geometry = FALSE,
                                          max_features = NULL,
                                          resource_id = NULL,
                                          mode = c("soda2", "soda3"),
                                          paginate = FALSE,
                                          page_size = 1000L,
                                          max_pages = 200L,
                                          app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "nyc_borough_boundaries.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled NYC borough boundaries fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(borocode = "character",
                                          boroname = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "gthc-hcne"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofnewyork.us/resource/%s.json",
                   resource_id)
    if (!isTRUE(geometry)) {
      url <- paste0(url, "?$select=borocode,boroname,shape_area,shape_leng")
    }
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  select_clause <- if (isTRUE(geometry)) {
    "*"
  } else {
    "borocode, boroname, shape_area, shape_leng"
  }
  .morie_dataset_soda3_query(
    resource_id,
    soql = sprintf("SELECT %s", select_clause),
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features,
    base_url = "https://data.cityofnewyork.us")
}

# ---------------------------------------------------------------------------
# NYPD borough-code cross-reference. arrest_boro carries 1-letter
# codes (M/B/K/Q/S); boro_nm carries the full UPPER name; borough
# boundaries (gthc-hcne) carries borocode "1"-"5" + boroname
# Title-Case. This table makes all three forms join-able.
# ---------------------------------------------------------------------------

.MORIE_NYPD_BORO_MAP <- data.frame(
  arrest_boro = c("M", "B", "K", "Q", "S"),
  boro_nm     = c("MANHATTAN", "BRONX", "BROOKLYN", "QUEENS",
                   "STATEN ISLAND"),
  borocode    = c("1", "2", "3", "4", "5"),
  boroname    = c("Manhattan", "Bronx", "Brooklyn", "Queens",
                   "Staten Island"),
  stringsAsFactors = FALSE)

#' NYPD borough-code cross-reference (1-letter / UPPER / numeric)
#'
#' NYPD CJ datasets reference boroughs through three different
#' encodings depending on the table:
#'
#' \describe{
#'   \item{`arrest_boro`}{1-letter code "B/Q/M/S/K" (Arrests).}
#'   \item{`boro_nm`}{UPPER full name "MANHATTAN" etc. (Complaints).}
#'   \item{`borocode` / `boroname`}{numeric "1"-"5" + Title-Case
#'     (Borough Boundaries gthc-hcne).}
#' }
#'
#' This helper returns a 5-row crosswalk between all four forms so
#' callers can left-join NYPD data against the
#' [morie_datasets_nyc_boroughs()] boundary table regardless of
#' which encoding their source uses. Used internally by
#' [morie_datasets_nyc_nypd_resolved()].
#'
#' @return A `data.frame` with 4 columns: `arrest_boro`, `boro_nm`,
#'   `borocode`, `boroname`.
#' @export
morie_datasets_nyc_nypd_boro_crosswalk <- function() {
  .MORIE_NYPD_BORO_MAP
}

# ---------------------------------------------------------------------------
# NYC multi-boundary loaders (3CCC2)
# ---------------------------------------------------------------------------

.morie_nyc_boundary_fixture <- function(fname, expected_rows = NULL) {
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path)) {
    stop(sprintf("bundled NYC boundary fixture missing: %s", fname),
         call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                         check.names = FALSE)
  if (!is.null(expected_rows) && nrow(df) != expected_rows) {
    warning(sprintf("fixture %s row count drift: have %d, expected %d",
                     fname, nrow(df), expected_rows), call. = FALSE)
  }
  df
}

#' NYC public school district boundaries (NYS K-12)
#'
#' Phase 3CCC2. Bundled snapshot of NYC OpenData
#' `8ugf-3d8u` (33 districts).
#'
#' @param offline If `TRUE` (default), reads the bundled CSV; if
#'   `FALSE`, fetches via SODA2.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `schooldist`, `shape_leng`, `shape_area`.
#' @examples
#' df <- morie_datasets_nyc_school_districts(offline = TRUE)
#' nrow(df)  # 33
#' @export
morie_datasets_nyc_school_districts <- function(offline = TRUE,
                                                  max_features = NULL) {
  if (offline) {
    df <- .morie_nyc_boundary_fixture("nyc_school_districts.csv", 33L)
  } else {
    url <- "https://data.cityofnewyork.us/resource/8ugf-3d8u.json"
    df <- .morie_dataset_socrata_fetch(url,
                                         select = "schooldist,shape_leng,shape_area")
  }
  if (!is.null(max_features)) df <- utils::head(df, as.integer(max_features))
  df$schooldist <- as.character(df$schooldist)
  df
}

#' NYC City Council district boundaries
#'
#' Phase 3CCC2. Bundled snapshot of NYC OpenData
#' `872g-cjhh` (51 districts).
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `coundist`, `shape_leng`, `shape_area`.
#' @export
morie_datasets_nyc_council_districts <- function(offline = TRUE,
                                                   max_features = NULL) {
  if (offline) {
    df <- .morie_nyc_boundary_fixture("nyc_council_districts.csv", 51L)
  } else {
    url <- "https://data.cityofnewyork.us/resource/872g-cjhh.json"
    df <- .morie_dataset_socrata_fetch(url,
                                         select = "coundist,shape_leng,shape_area")
  }
  if (!is.null(max_features)) df <- utils::head(df, as.integer(max_features))
  df$coundist <- as.character(df$coundist)
  df
}

#' NYC community district boundaries
#'
#' Phase 3CCC2. Bundled snapshot of NYC OpenData
#' `5crt-au7u` (71 districts).
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `boro_cd`, `shape_leng`, `shape_area`.
#' @export
morie_datasets_nyc_community_districts <- function(offline = TRUE,
                                                     max_features = NULL) {
  if (offline) {
    df <- .morie_nyc_boundary_fixture("nyc_community_districts.csv", 71L)
  } else {
    url <- "https://data.cityofnewyork.us/resource/5crt-au7u.json"
    df <- .morie_dataset_socrata_fetch(url,
                                         select = "boro_cd,shape_leng,shape_area")
  }
  if (!is.null(max_features)) df <- utils::head(df, as.integer(max_features))
  df$boro_cd <- as.character(df$boro_cd)
  df
}

#' NYC Neighborhood Tabulation Areas (2020)
#'
#' Phase 3CCC2. Bundled snapshot of NYC OpenData
#' `9nt8-h7nd` (262 NTAs from the 2020 census revision).
#' Carries boro + county FIPS + parent CDTA so it can be aggregated
#' upward without spatial intersection.
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with 11 cols including `nta2020`, `ntaname`,
#'   `borocode`, `boroname`, `countyfips`, `cdta2020`, `cdtaname`.
#' @export
morie_datasets_nyc_ntas_2020 <- function(offline = TRUE,
                                           max_features = NULL) {
  if (offline) {
    df <- .morie_nyc_boundary_fixture("nyc_ntas_2020.csv", 262L)
  } else {
    url <- "https://data.cityofnewyork.us/resource/9nt8-h7nd.json"
    cols <- paste(c("borocode", "boroname", "countyfips",
                     "nta2020", "ntaname", "ntaabbrev", "ntatype",
                     "cdta2020", "cdtaname",
                     "shape_leng", "shape_area"), collapse = ",")
    df <- .morie_dataset_socrata_fetch(url, select = cols)
  }
  if (!is.null(max_features)) df <- utils::head(df, as.integer(max_features))
  df$borocode <- as.character(df$borocode)
  df$nta2020  <- as.character(df$nta2020)
  df$cdta2020 <- as.character(df$cdta2020)
  df
}

.morie_nyc_zcta_fixture <- function(fname, expected_rows = 221L) {
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path))
    stop(sprintf("bundled NYC boundary fixture missing: %s", fname),
         call. = FALSE)
  # zcta5 MUST be character to preserve NJ-area leading zeros
  # ("07305" otherwise becomes integer 7305).
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                         check.names = FALSE,
                         colClasses = c(zcta5 = "character"))
  if (nrow(df) != expected_rows)
    warning(sprintf("fixture %s row count drift: have %d, expected %d",
                     fname, nrow(df), expected_rows), call. = FALSE)
  df
}

#' NYC ZIP Code Tabulation Areas (ZCTAs)
#'
#' Phase 3CCC2. Bundled snapshot of NYC OpenData
#' `35j5-n34v` (221 ZCTAs intersecting NYC). ZCTAs are the Census
#' Bureau's geographic approximation of USPS ZIP code service areas
#' -- pair with NYPD address-bearing data via ZIP code lookups for
#' a coarser-than-precinct, finer-than-borough geography.
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `zcta5`, `arealand`, `areawater`,
#'   `centlat`, `centlon`, `intptlat`, `intptlon`.
#' @export
morie_datasets_nyc_zctas <- function(offline = TRUE,
                                       max_features = NULL) {
  if (offline) {
    df <- .morie_nyc_zcta_fixture("nyc_zctas.csv", 221L)
  } else {
    url <- "https://data.cityofnewyork.us/resource/35j5-n34v.json"
    df <- .morie_dataset_socrata_fetch(url,
                                         select = "zcta5,arealand,areawater,centlat,centlon,intptlat,intptlon")
  }
  if (!is.null(max_features)) df <- utils::head(df, as.integer(max_features))
  df$zcta5 <- as.character(df$zcta5)
  df
}

#' Unified catalog of NYC OpenData boundary loaders
#'
#' Phase 3CCC2. One-stop index of every NYC boundary fixture
#' morie ships, with its loader, SODA id, expected row count, and a
#' note on its join key.
#'
#' NOTE: school/council/community/NTA boundaries are NOT directly
#' row-key joinable to NYPD CJ data -- the CJ rows carry lat/long
#' (or just precinct/borough), not a district ID. Use these loaders
#' standalone for geographic context, or pair with a spatial join
#' via the `sf` package on `the_geom` (not bundled to keep morie
#' lightweight).
#'
#' @return A `data.frame` with one row per boundary fixture.
#' @examples
#' morie_datasets_nyc_boundaries_catalog()
#' @export
morie_datasets_nyc_boundaries_catalog <- function() {
  data.frame(
    boundary = c("borough", "police_precinct",
                  "school_district", "council_district",
                  "community_district", "nta_2020", "zcta"),
    loader = c("morie_datasets_nyc_boroughs",
                "morie_datasets_nyc_police_precincts",
                "morie_datasets_nyc_school_districts",
                "morie_datasets_nyc_council_districts",
                "morie_datasets_nyc_community_districts",
                "morie_datasets_nyc_ntas_2020",
                "morie_datasets_nyc_zctas"),
    soda_id = c("gthc-hcne", "78dh-3ptz",
                 "8ugf-3d8u", "872g-cjhh",
                 "5crt-au7u", "9nt8-h7nd",
                 "35j5-n34v"),
    n_rows = c(5L, 78L, 33L, 51L, 71L, 262L, 221L),
    join_key = c("borocode", "precinct",
                  "schooldist", "coundist",
                  "boro_cd", "nta2020", "zcta5"),
    row_key_joinable_to_nypd = c(TRUE, TRUE,
                                   FALSE, FALSE,
                                   FALSE, FALSE,
                                   FALSE),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# NYPD Offense Code dictionary (3BBB)
# ---------------------------------------------------------------------------

#' NYPD offense-code dictionary (`ky_cd` + `pd_cd` + descriptions)
#'
#' NYC OpenData does NOT publish a standalone NYPD-offense-code
#' table; the canonical mapping is implicit in the
#' (`ky_cd`, `ofns_desc`, `pd_cd`, `pd_desc`, `law_cat_cd`) tuples
#' carried by every Arrests / Complaints record. This bundled
#' fixture was derived by running a `$group` query on the NYPD
#' Arrests YTD feed (`uip8-fykc`) at fixture-creation time, giving
#' the 246 distinct offense tuples currently in active use.
#' Mirrors the chicago_iucr_codes pattern (3UU).
#'
#' Schema (all character):
#' \describe{
#'   \item{ky_cd}{3-digit Key Code (top-level offense category).}
#'   \item{ofns_desc}{Description for `ky_cd` (NYPD-truncated to
#'     30 chars; e.g. "MURDER & NON-NEGL. MANSLAUGHTE").}
#'   \item{pd_cd}{3-digit Penal-Detailed code (subcategory).}
#'   \item{pd_desc}{Description for `pd_cd` (same truncation).}
#'   \item{law_cat_cd}{Penal classification: "F" felony / "M"
#'     misdemeanor / "V" violation / "I" infraction / (blank).}
#' }
#'
#' The string descriptions ARE truncated at 30 chars in the
#' upstream NYPD feeds; this is NOT a morie processing bug -- it's
#' how NYPD's NYS DCJS warehouse stores them. PRs welcome to add a
#' parallel `pd_desc_full` column once a canonical un-truncated
#' source is identified.
#'
#' Refreshing the fixture:
#' ```r
#' # Re-derive when the YTD feed adds new offense tuples (rare):
#' #   curl "https://data.cityofnewyork.us/resource/uip8-fykc.json
#' #         ?$select=ky_cd,ofns_desc,pd_cd,pd_desc,law_cat_cd
#' #         &$group=ky_cd,ofns_desc,pd_cd,pd_desc,law_cat_cd
#' #         &$order=ky_cd,pd_cd&$limit=10000"
#' # then write to inst/extdata/nyc_nypd_offense_codes.csv.
#' ```
#'
#' @param max_features Optional row cap.
#' @return A `data.frame` with 246 rows x 5 cols.
#' @examples
#' codes <- morie_datasets_nyc_nypd_offense_codes()
#' subset(codes, ky_cd == "104")  # all RAPE subcategories
#' @export
morie_datasets_nyc_nypd_offense_codes <- function(max_features = NULL) {
  path <- system.file("extdata", "nyc_nypd_offense_codes.csv",
                      package = "morie")
  if (!nzchar(path)) {
    stop("bundled NYPD offense codes fixture missing", call. = FALSE)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                         check.names = FALSE,
                         colClasses = c(ky_cd = "character",
                                        pd_cd = "character",
                                        law_cat_cd = "character"))
  if (!is.null(max_features)) {
    df <- utils::head(df, as.integer(max_features))
  }
  df
}

# ---------------------------------------------------------------------------
# NYC NYPD statute book dictionary (3CCC1)
# ---------------------------------------------------------------------------

#' NYS / NYC statute book code dictionary
#'
#' Phase 3CCC1. Maps the leading alpha prefix of an NYPD `law_code`
#' (e.g., `"PL"` in `"PL 1601005"`) to its human-readable statute
#' book name + jurisdiction (NYS vs NYC). Covers all 22 distinct
#' prefixes observed in the YTD arrests feed + 24 additional canonical
#' NYS / NYC codes that appear in complaint, summons, and historical
#' arrest data.
#'
#' @return A `data.frame` with columns `book`, `name`, `jurisdiction`.
#' @examples
#' books <- morie_datasets_nyc_nypd_law_books()
#' subset(books, book == "PL")
#' @export
morie_datasets_nyc_nypd_law_books <- function() {
  path <- system.file("extdata", "nyc_nypd_law_books.csv",
                      package = "morie")
  if (!nzchar(path)) {
    stop("bundled NYPD law books fixture missing", call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE,
                  check.names = FALSE)
}

#' Parse an NYPD `law_code` string into its structural fields
#'
#' Phase 3CCC1. NYPD law codes are space-or-zero-padded composites of
#' a 1-4 char statute book prefix and a numeric/alpha section
#' identifier. Examples:
#'
#' \itemize{
#'   \item `"PL 1601005"`  -> book=PL, section=1601005 (Penal Law)
#'   \item `"VTL0511000"`  -> book=VTL, section=0511000
#'   \item `"AC 0019190"`  -> book=AC, section=0019190 (NYC Admin Code)
#'   \item `"ABC0064A00"`  -> book=ABC, section=0064A00
#' }
#'
#' The book prefix is extracted as the leading run of uppercase
#' ASCII letters; the section is everything after the prefix with
#' leading whitespace stripped. NA / empty inputs return NA fields.
#'
#' @param law_code Character vector of NYPD `law_code` strings.
#' @return A `data.frame` with `book`, `section` columns aligned to
#'   `law_code`. Length-preserving.
#' @examples
#' morie_parse_nypd_law_code(c("PL 1601005", "AC 0019190", "ABC0064A00"))
#' @export
morie_parse_nypd_law_code <- function(law_code) {
  law_code <- as.character(law_code)
  out <- data.frame(book = rep(NA_character_, length(law_code)),
                     section = rep(NA_character_, length(law_code)),
                     stringsAsFactors = FALSE)
  ok <- !is.na(law_code) & nzchar(law_code)
  if (!any(ok)) return(out)
  # Leading uppercase-alpha run is the book.
  m <- regmatches(law_code[ok], regexpr("^[A-Z]+", law_code[ok]))
  # regmatches drops positions with no match -- align by re-running.
  has_book <- grepl("^[A-Z]+", law_code[ok])
  books <- rep(NA_character_, sum(ok))
  books[has_book] <- m
  # Section = post-prefix, stripped of leading whitespace.
  secs <- sub("^[A-Z]+\\s*", "", law_code[ok])
  secs[!nzchar(secs)] <- NA_character_
  out$book[ok] <- books
  out$section[ok] <- secs
  # Section without a recognised book prefix is meaningless --
  # drop it to NA so the resolver join behaves consistently.
  out$section[is.na(out$book)] <- NA_character_
  out
}

# ---------------------------------------------------------------------------
# NYC NYPD resolved-joins analyzer (3AAA)
# ---------------------------------------------------------------------------

#' One-call NYPD data + borough + precinct join
#'
#' Phase 3AAA. Pulls a slice of any
#' [morie_datasets_nyc_nypd_by_key()]-resolvable dataset and
#' left-joins its borough + precinct foreign keys against the
#' bundled resolvers ([morie_datasets_nyc_boroughs()] +
#' [morie_datasets_nyc_police_precincts()]).
#'
#' Auto-detects the borough + precinct columns per dataset:
#'
#' \tabular{lll}{
#'   \strong{NYPD dataset}      \tab \strong{boro column}        \tab \strong{precinct column} \cr
#'   nypd_arrests_historic     \tab `arrest_boro` (M/B/K/Q/S)    \tab `arrest_precinct`     \cr
#'   nypd_arrests_ytd          \tab `arrest_boro`                \tab `arrest_precinct`     \cr
#'   nypd_complaint_historic   \tab `boro_nm` (UPPER)            \tab `addr_pct_cd`         \cr
#'   nypd_complaint_ytd        \tab `boro_nm` (UPPER)            \tab `addr_pct_cd`         \cr
#'   nypd_hate_crimes          \tab `patrol_borough_name`        \tab `complaint_precinct_code` \cr
#'   nypd_uof_incidents        \tab (none directly; precinct only)\tab `precinct`            \cr
#' }
#'
#' Resolver columns prefixed `boro_*` + `precinct_*` to avoid
#' collisions. Left-join semantics (row count preserved).
#'
#' @inheritParams morie_datasets_nyc_nypd_by_key
#' @param resolvers Character subset of `c("boro", "precinct")` to
#'   join. Default joins both.
#' @return A wide `data.frame`: NYPD columns first, then prefixed
#'   resolver columns.
#' @examples
#' df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
#'                                          offline = TRUE)
#' names(df)
#' @export
morie_datasets_nyc_nypd_resolved <- function(
    dataset_key,
    year = NULL,
    max_features = NULL,
    offline = TRUE,
    resource_id = NULL,
    mode = c("soda2", "soda3"),
    paginate = FALSE,
    page_size = 1000L,
    max_pages = 200L,
    app_token = NULL,
    resolvers = c("boro", "precinct", "offense", "law_code")) {
  mode <- match.arg(mode)
  resolvers <- match.arg(resolvers,
                          choices = c("boro", "precinct", "offense",
                                       "law_code"),
                          several.ok = TRUE)
  out <- morie_datasets_nyc_nypd_by_key(
    dataset_key,
    year = year,
    max_features = max_features,
    offline = offline,
    resource_id = resource_id,
    mode = mode,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    app_token = app_token)
  if (nrow(out) == 0L) return(out)

  prefix_cols <- function(df, drop, prefix) {
    keep <- setdiff(names(df), drop)
    names(df)[match(keep, names(df))] <- paste0(prefix, "_", keep)
    df
  }

  # Borough join. Detect which encoding the NYPD dataset uses:
  if ("boro" %in% resolvers) {
    boro_col <- intersect(c("arrest_boro", "boro_nm",
                              "patrol_borough_name"),
                            names(out))[1L]
    if (!is.na(boro_col)) {
      cw <- .MORIE_NYPD_BORO_MAP
      bb <- morie_datasets_nyc_boroughs(offline = TRUE)
      cw <- merge(cw, bb, by = c("borocode", "boroname"),
                   all.x = TRUE, sort = FALSE)
      # Pick the crosswalk column matching the NYPD encoding.
      cw_join_col <- switch(boro_col,
        "arrest_boro"         = "arrest_boro",
        "boro_nm"             = "boro_nm",
        "patrol_borough_name" = "boro_nm")
      if (boro_col == "patrol_borough_name") {
        # Normalise patrol_borough_name -> boro_nm (UPPER) before join.
        out$.__patrol_upper <- toupper(out$patrol_borough_name)
        cw$.__join <- cw$boro_nm
        out <- merge(out,
                      prefix_cols(cw[, c(".__join", setdiff(names(cw), c(".__join")))],
                                    drop = ".__join", prefix = "boro"),
                      by.x = ".__patrol_upper", by.y = ".__join",
                      all.x = TRUE, sort = FALSE)
        out$.__patrol_upper <- NULL
      } else {
        out_join <- cw_join_col
        cwp <- prefix_cols(cw, drop = out_join, prefix = "boro")
        out <- merge(out, cwp, by = out_join,
                      all.x = TRUE, sort = FALSE)
      }
    }
  }

  # Precinct join.
  if ("precinct" %in% resolvers) {
    pct_col <- intersect(c("arrest_precinct", "addr_pct_cd",
                             "complaint_precinct_code", "precinct"),
                           names(out))[1L]
    if (!is.na(pct_col)) {
      p <- morie_datasets_nyc_police_precincts(offline = TRUE)
      names(p)[names(p) == "precinct"] <- pct_col
      p <- prefix_cols(p, drop = pct_col, prefix = "precinct")
      out[[pct_col]] <- as.character(out[[pct_col]])
      p[[pct_col]] <- as.character(p[[pct_col]])
      out <- merge(out, p, by = pct_col,
                    all.x = TRUE, sort = FALSE)
    }
  }

  # Offense join (3BBB). Available only on Arrests + Complaints
  # datasets that carry both ky_cd + pd_cd; other NYPD datasets
  # (hate_crimes / uof / vehicle_stops) silently fall through.
  if ("offense" %in% resolvers &&
      "ky_cd" %in% names(out) && "pd_cd" %in% names(out)) {
    odc <- morie_datasets_nyc_nypd_offense_codes()
    # The NYPD source data carries ofns_desc + pd_desc + law_cat_cd
    # on the row itself. Keep both: rename the dictionary's copies
    # with the `offense_` prefix so the row's own values + the
    # canonical lookup land side-by-side (rows may carry NA or local
    # variations; the dictionary is the canonical join target).
    rename_map <- c(ofns_desc  = "offense_ofns_desc",
                     pd_desc    = "offense_pd_desc",
                     law_cat_cd = "offense_law_cat_cd")
    for (old in names(rename_map)) {
      if (old %in% names(odc))
        names(odc)[names(odc) == old] <- rename_map[[old]]
    }
    out$ky_cd <- as.character(out$ky_cd)
    out$pd_cd <- as.character(out$pd_cd)
    odc$ky_cd <- as.character(odc$ky_cd)
    odc$pd_cd <- as.character(odc$pd_cd)
    out <- merge(out, odc, by = c("ky_cd", "pd_cd"),
                  all.x = TRUE, sort = FALSE)
  }

  # Law code parse + book-name join (3CCC1). Splits each row's
  # `law_code` into `law_book` + `law_section`, then left-joins
  # against the bundled statute book dictionary for `law_book_name`
  # + `law_jurisdiction`. Silently no-ops if law_code is absent.
  if ("law_code" %in% resolvers && "law_code" %in% names(out)) {
    parsed <- morie_parse_nypd_law_code(out$law_code)
    out$law_book <- parsed$book
    out$law_section <- parsed$section
    books <- morie_datasets_nyc_nypd_law_books()
    names(books)[names(books) == "name"] <- "law_book_name"
    names(books)[names(books) == "jurisdiction"] <- "law_jurisdiction"
    names(books)[names(books) == "book"] <- "law_book"
    out <- merge(out, books, by = "law_book",
                  all.x = TRUE, sort = FALSE)
  }

  rownames(out) <- NULL
  out
}
