# NAPS (National Air Pollution Surveillance) hourly files from ECCC's data catalogue, the
# R arm of morie.earth.fetch_naps(): one CSV per pollutant and year, wide (H01..H24),
# bilingual header, -999 for a missing hour; returned long, one row per station-hour.

.NAPS_UNITS <- c(no2 = "ppb", no = "ppb", nox = "ppb", o3 = "ppb", so2 = "ppb", co = "ppm",
                 pm25 = "ug/m3", pm10 = "ug/m3")
.NAPS_PROVINCES <- c(ON = "Ontario", QC = "Quebec", BC = "British Columbia", AB = "Alberta",
                     NS = "Nova Scotia", CA = "Canada")

.morie_naps_hourly_path <- function(year, pollutant) {
  sprintf(paste0("/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/%d/",
                 "ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/%s_%d.csv"),
          as.integer(year), toupper(pollutant), as.integer(year))
}

#' Fetch one NAPS hourly air-quality file
#'
#' Downloads the Environment and Climate Change Canada NAPS hourly file for a pollutant
#' and year (\code{<POLLUTANT>_<year>.csv} in the ECCC data catalogue) and returns it in long
#' form, one row per station and hour, with \code{-999} (a missing hour) dropped. Hours are
#' hour-ending local standard time, so \code{H01} of a date is 01:00 on that date. The same
#' table as \code{morie.earth.fetch_naps()} in the Python package; the catalog keys
#' \code{naps-<pollutant>-<province>-<year>} (\code{rmorie list-datasets}) call this.
#'
#' @param year Calendar year, e.g. \code{2023}.
#' @param pollutant One of \code{"no2"}, \code{"pm25"}, \code{"o3"}, \code{"so2"},
#'   \code{"co"}, \code{"pm10"}.
#' @param province Optional two-letter province or territory code (\code{"ON"}); omitted
#'   or \code{"CA"} keeps every station.
#' @param timeout Download timeout in seconds (the Canada-wide files are large).
#' @return A data.frame with \code{station_id}, \code{station_name}, \code{latitude},
#'   \code{longitude}, \code{province}, \code{datetime_local}, \code{value}, \code{unit}.
#' @examples
#' \dontrun{
#' no2 <- morie_fetch_naps(2023, "no2", province = "ON")
#' head(no2)
#' }
#' @export
morie_fetch_naps <- function(year, pollutant = "no2", province = NULL, timeout = 600) {
  pollutant <- tolower(pollutant)
  if (!pollutant %in% names(.NAPS_UNITS)) {
    stop("pollutant must be one of ", paste(names(.NAPS_UNITS), collapse = ", "), call. = FALSE)
  }
  path <- .morie_naps_hourly_path(year, pollutant)
  url <- paste0("https://data-donnees.az.ec.gc.ca/api/file?path=", utils::URLencode(path, reserved = TRUE))
  dest <- tempfile(fileext = ".csv")
  on.exit(unlink(dest), add = TRUE)
  # the catalogue answers 404 to non-browser agents
  .morie_dl(url, dest, headers = c("User-Agent" = "Mozilla/5.0 (compatible; morie/1; +https://rmorie.com)"),
            label = basename(path), timeout = timeout)
  text <- readLines(dest, warn = FALSE, encoding = "UTF-8")
  if (length(text)) text[1L] <- sub("^\ufeff", "", text[1L])
  out <- .morie_parse_naps_hourly(text, pollutant)
  if (!nrow(out)) stop(sprintf("NAPS has no %s hourly file for %s", toupper(pollutant), year), call. = FALSE)
  if (!is.null(province) && nzchar(province) && toupper(province) != "CA") {
    out <- out[toupper(out$province) == toupper(province), , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

.morie_parse_naps_hourly <- function(lines, pollutant) {
  empty <- data.frame(station_id = character(), station_name = character(), latitude = numeric(),
                      longitude = numeric(), province = character(), datetime_local = character(),
                      value = numeric(), unit = character(), stringsAsFactors = FALSE)
  start <- which(startsWith(lines, "Pollutant//Polluant"))[1L]
  if (is.na(start)) stop("NAPS file has no header row starting with Pollutant//Polluant", call. = FALSE)
  wide <- utils::read.csv(text = paste(lines[start:length(lines)], collapse = "\n"), check.names = FALSE,
                          stringsAsFactors = FALSE, na.strings = character())
  if (!nrow(wide)) return(empty)
  names(wide) <- trimws(sub("//.*$", "", names(wide)))
  hours <- grep("^H[0-9]{2}$", names(wide), value = TRUE)
  need <- c("NAPS ID", "City", "Latitude", "Longitude", "Province/Territory", "Date")
  miss <- setdiff(need, names(wide))
  if (length(miss)) stop("NAPS file lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  unit <- unname(.NAPS_UNITS[pollutant])
  rows <- lapply(hours, function(h) {
    v <- suppressWarnings(as.numeric(wide[[h]]))
    keep <- !is.na(v) & v != -999
    if (!any(keep)) return(NULL)
    data.frame(
      station_id = as.character(wide[["NAPS ID"]][keep]),
      station_name = as.character(wide[["City"]][keep]),
      latitude = suppressWarnings(as.numeric(wide[["Latitude"]][keep])),
      longitude = suppressWarnings(as.numeric(wide[["Longitude"]][keep])),
      province = as.character(wide[["Province/Territory"]][keep]),
      datetime_local = sprintf("%s %02d:00", as.character(wide[["Date"]][keep]), as.integer(substr(h, 2L, 3L))),
      value = v[keep],
      unit = unit,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out <- out[order(out$station_id, out$datetime_local), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# "pollutant=no2;year=2023;province=ON" -> list(pollutant = "no2", year = 2023, province = "ON")
.morie_parse_fetcher_args <- function(spec) {
  if (is.null(spec) || !nzchar(spec)) return(list())
  parts <- strsplit(strsplit(spec, ";", fixed = TRUE)[[1L]], "=", fixed = TRUE)
  out <- lapply(parts, function(p) {
    v <- trimws(p[2L])
    if (grepl("^-?[0-9]+$", v)) as.integer(v) else if (grepl("^-?[0-9.]+$", v)) as.numeric(v) else v
  })
  stats::setNames(out, vapply(parts, function(p) trimws(p[1L]), character(1L)))
}

# the NAPS keys of the dataset catalog, the same 25 as the Python package
.naps_catalog_entries <- function() {
  spec <- list(
    c("co", "ON", 2023), c("no2", "AB", 2023), c("no2", "BC", 2023), c("no2", "CA", 2023),
    c("no2", "NS", 2023), c("no2", "ON", 2022), c("no2", "ON", 2023), c("no2", "QC", 2023),
    c("o3", "CA", 2023), c("o3", "ON", 2021), c("o3", "ON", 2022), c("o3", "ON", 2023),
    c("pm10", "ON", 2023), c("pm25", "AB", 2023), c("pm25", "BC", 2023), c("pm25", "CA", 2023),
    c("pm25", "NS", 2023), c("pm25", "ON", 2019), c("pm25", "ON", 2020), c("pm25", "ON", 2021),
    c("pm25", "ON", 2022), c("pm25", "ON", 2023), c("pm25", "QC", 2023), c("so2", "ON", 2023)
  )
  lapply(spec, function(s) {
    pol <- s[[1L]]
    prov <- s[[2L]]
    yr <- s[[3L]]
    list(
      key = sprintf("naps-%s-%s-%s", pol, tolower(prov), yr),
      name = sprintf("NAPS %s %s %s", toupper(pol), unname(.NAPS_PROVINCES[prov]), yr),
      source = "naps", survey = "naps", year = as.character(yr),
      format = "fetcher", type = "air-quality", large_file = prov == "CA",
      local_path = "", table_name = sprintf("naps_%s_%s_%s", pol, tolower(prov), yr),
      ckan_resource_id = "", fetcher = "morie_fetch_naps",
      fetcher_args = sprintf("pollutant=%s;year=%s;province=%s", pol, yr, prov)
    )
  })
}

.statcan_catalog_entries <- function() {
  list(list(
    key = "cchs22", name = "CCHS 2022 PUMF (Canadian Community Health Survey)",
    source = "statcan", survey = "cchs", year = "2022",
    format = "csv", type = "pumf", large_file = TRUE,
    local_path = "data/datasets/statcan/CCHS/2022/cchs-2022-pumf.csv",
    table_name = "cchs22", ckan_resource_id = "",
    # the product zip holds one table plus codebooks: ".csv" names its CSV member (a substring match)
    download_url = "https://www150.statcan.gc.ca/n1/pub/82m0013x/2024001/2022_CSV.zip", zip_member = ".csv"
  ))
}
