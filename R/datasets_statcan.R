# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD3: Statistics Canada WDS REST API loader for the
# Canadian Centre for Justice and Community Safety Statistics
# (CCJS) cube family.
#
# StatCan publishes federal crime + justice statistics through the
# Common Output Data Repository (CODR) which is served by the Web
# Data Service (WDS) REST API:
#
#   Base URL: https://www150.statcan.gc.ca/t1/wds/rest/
#   Docs:    https://www.statcan.gc.ca/eng/developers/wds
#
# Key endpoints used:
#   * getCubeMetadata          (POST array of {productId}) -- dims + members
#   * getDataFromVectorsAndLatestNPeriods
#                              (POST array of {vectorId,latestN}) -- slice
#   * getFullTableDownloadCSV  (GET /<productId>/en)        -- bulk ZIP URL
#
# A `productId` is the cube's 8-digit catalogue ID (e.g.
# 35100177 = CCJS Incident-based crime statistics). The dashed
# display form "35-10-0177-01" is the same cube + view number.

.MORIE_STATCAN_WDS_BASE <- "https://www150.statcan.gc.ca/t1/wds/rest"

#' Statistics Canada CCJS cube registry (curated subset)
#'
#' Phase 3DDD3. Bundled 10-row registry of high-traffic
#' Canadian Centre for Justice and Community Safety Statistics
#' cubes published through StatCan CODR -- the federal-level
#' complement to morie's provincial loaders (Ontario OTIS, BC VPD,
#' etc.).
#'
#' @return A `data.frame` with `product_id`, `cube_title_en`,
#'   `dimensions`, `frequency`.
#' @examples
#' cubes <- morie_datasets_statcan_ccjs_cubes()
#' subset(cubes, grepl("homicide", cube_title_en, ignore.case = TRUE))
#' @export
morie_datasets_statcan_ccjs_cubes <- function() {
  path <- system.file("extdata", "statcan_ccjs_cubes.csv",
                      package = "morie")
  if (!nzchar(path))
    stop("bundled StatCan CCJS cube registry missing", call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE,
                  check.names = FALSE,
                  colClasses = c(product_id = "integer"))
}

#' Fetch a StatCan cube's metadata (dimensions + members) via WDS
#'
#' Phase 3DDD3. Wraps the `getCubeMetadata` POST endpoint.
#'
#' @param product_id Integer cube ID (e.g., `35100177`).
#' @param timeout_s HTTP timeout in seconds.
#' @return A list with `status` and `object` (dimensions, members,
#'   release info, etc.). Errors if `status != "SUCCESS"`.
#' @examples
#' \dontrun{
#' meta <- morie_datasets_statcan_cube_metadata(35100177)
#' meta$object$cubeTitleEn
#' length(meta$object$dimension)
#' }
#' @export
morie_datasets_statcan_cube_metadata <- function(product_id,
                                                   timeout_s = 60L) {
  body <- list(list(productId = as.integer(product_id)))
  r <- .morie_dataset_http_post_json(
    sprintf("%s/getCubeMetadata", .MORIE_STATCAN_WDS_BASE),
    body = body, timeout_s = timeout_s)
  if (length(r) == 0L || is.null(r[[1]]$status))
    stop("StatCan WDS returned empty or malformed response", call. = FALSE)
  if (r[[1]]$status != "SUCCESS")
    stop(sprintf("StatCan WDS status=%s: %s",
                  r[[1]]$status,
                  paste(unlist(r[[1]]), collapse = "; ")),
          call. = FALSE)
  r[[1]]
}

#' Fetch the latest N periods for a set of StatCan vectors via WDS
#'
#' Phase 3DDD3. Wraps the `getDataFromVectorsAndLatestNPeriods`
#' POST endpoint. A "vector" is StatCan's atomic time series ID
#' (e.g., `v109502878` for "Canada -- Total, all violations").
#'
#' @param vector_ids Integer vector of StatCan vector IDs (no `v`
#'   prefix; the API takes raw integers).
#' @param n_periods Number of latest periods per vector (default 5).
#' @param timeout_s HTTP timeout.
#' @return A `data.frame` with one row per (vector, period)
#'   observation: `vector_id`, `coordinate`, `ref_period`, `value`,
#'   `decimals`, `status`, `symbol`, `scalar_factor`.
#' @examples
#' \dontrun{
#' df <- morie_datasets_statcan_vectors(c(109502878L, 109502879L),
#'                                         n_periods = 3)
#' nrow(df)  # ~6
#' }
#' @export
morie_datasets_statcan_vectors <- function(vector_ids,
                                             n_periods = 5L,
                                             timeout_s = 60L) {
  body <- lapply(as.integer(vector_ids), function(v) {
    list(vectorId = v, latestN = as.integer(n_periods))
  })
  r <- .morie_dataset_http_post_json(
    sprintf("%s/getDataFromVectorsAndLatestNPeriods",
             .MORIE_STATCAN_WDS_BASE),
    body = body, timeout_s = timeout_s)
  rows <- list()
  for (e in r) {
    if (is.null(e$status) || e$status != "SUCCESS") next
    o <- e$object
    if (is.null(o$vectorDataPoint)) next
    for (p in o$vectorDataPoint) {
      rows[[length(rows) + 1L]] <- data.frame(
        vector_id     = o$vectorId,
        coordinate    = o$coordinate %||% NA_character_,
        ref_period    = p$refPer,
        value         = if (is.null(p$value)) NA_real_ else as.numeric(p$value),
        decimals      = p$decimals %||% NA_integer_,
        status        = p$statusCode %||% NA_character_,
        symbol        = p$symbolCode %||% NA_character_,
        scalar_factor = p$scalarFactorCode %||% NA_integer_,
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L)
    return(data.frame(vector_id = integer(0), coordinate = character(0),
                       ref_period = character(0), value = numeric(0)))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Get the bulk-CSV download URL for a StatCan cube
#'
#' Phase 3DDD3. Wraps the `getFullTableDownloadCSV/<productId>/en`
#' GET endpoint. Returns the temporary download URL for the cube's
#' bulk CSV ZIP. Caller is responsible for downloading the ZIP
#' (typically large -- often hundreds of MB).
#'
#' @param product_id Integer cube ID.
#' @param language One of `"en"` or `"fr"`.
#' @return Character URL string.
#' @examples
#' \dontrun{
#' url <- morie_datasets_statcan_full_csv_url(35100177)
#' # download.file(url, "ccjs_177.zip")
#' }
#' @export
morie_datasets_statcan_full_csv_url <- function(product_id,
                                                  language = c("en", "fr")) {
  language <- match.arg(language)
  url <- sprintf("%s/getFullTableDownloadCSV/%d/%s",
                  .MORIE_STATCAN_WDS_BASE,
                  as.integer(product_id), language)
  r <- .morie_dataset_http_json(url)
  if (is.null(r$status) || r$status != "SUCCESS")
    stop(sprintf("StatCan WDS getFullTableDownloadCSV status=%s",
                  r$status %||% "NULL"),
          call. = FALSE)
  r$object
}

`%||%` <- function(a, b) if (is.null(a)) b else a
