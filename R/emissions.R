# Compute-emissions tracker: the R twin of morie.emissions (Python), which
# follows the CodeCarbon methodology. Energy = power x time; power is the CPU
# thermal design power scaled by utilisation (TDP x (0.1 + 0.9 u^3)) plus a
# DIMM-count RAM heuristic; emissions = energy x the grid's carbon intensity
# (IEA / Our World in Data energy mix per country) x PUE. Every run can be
# sealed in a bricklayer capsule: a manifest with the measurements and the
# environment, the file digests, and a post-quantum signature.
#
# Utilisation is sampled by a C++ background thread (src/morie_emissions_sampler.cpp)
# reading the kernel's aggregate CPU counters, and power is integrated per
# sample as the Python arm does (machine mode). Without counters (neither
# /proc/stat nor host_statistics) the R process's CPU time over wall time is
# used (process mode). The mode is stated in the tracking_mode column.

.emissions_data_file <- function(name) {
  system.file("extdata", "emissions", name, package = utils::packageName(), mustWork = FALSE)
}

.emissions_source_intensity <- function() {
  path <- .emissions_data_file("carbon_intensity_per_source.json")
  fallback <- list(coal = 995, petroleum = 816, natural_gas = 743, fossil = 635,
                   geothermal = 38, hydroelectricity = 26, nuclear = 29, solar = 48, wind = 26)
  if (!nzchar(path)) return(fallback)
  d <- tryCatch(.morie_from_json(paste(readLines(path, warn = FALSE), collapse = "\n")),
                error = function(e) NULL)
  if (is.null(d)) return(fallback)
  Filter(is.numeric, d[setdiff(names(d), c("unit", "world_average"))])
}

.emissions_energy_mix <- function() {
  path <- .emissions_data_file("global_energy_mix.json")
  if (!nzchar(path)) return(list())
  tryCatch(.morie_from_json(paste(readLines(path, warn = FALSE), collapse = "\n")),
           error = function(e) list())
}

#' Carbon intensity of a country's grid
#'
#' Kilograms of CO2-equivalent per kilowatt-hour from the IEA / Our World in
#' Data energy mix shipped with the package (213 countries, as in
#' CodeCarbon): the country's published intensity when it has one, otherwise
#' the generation-weighted mean of the per-source intensities, and the world
#' average (475 g/kWh) for an unknown country.
#' @param country_iso Three-letter ISO code, e.g. \code{"CAN"}.
#' @param region Unused; kept for parity with the Python arm.
#' @return A number in kg CO2eq per kWh.
#' @examples
#' morie_emissions_carbon_intensity("CAN")
#' morie_emissions_carbon_intensity("")   # world average
#' @export
morie_emissions_carbon_intensity <- function(country_iso = "", region = "") {
  mix <- .emissions_energy_mix()
  iso <- toupper(as.character(country_iso)[1L])
  if (nzchar(iso) && !is.null(mix[[iso]]) && is.list(mix[[iso]])) {
    cd <- mix[[iso]]
    if (is.numeric(cd$carbon_intensity)) return(as.numeric(cd$carbon_intensity) / 1000)
    src <- .emissions_source_intensity()
    total <- 0
    weighted <- 0
    for (s in names(src)) {
      twh <- cd[[s]]
      if (is.numeric(twh)) {
        total <- total + twh
        weighted <- weighted + twh * src[[s]]
      }
    }
    if (total > 0) return(weighted / total / 1000)
  }
  475 / 1000
}

.emissions_total_ram_gb <- function() {
  if (file.exists("/proc/meminfo")) {
    l <- grep("^MemTotal:", readLines("/proc/meminfo", warn = FALSE), value = TRUE)
    if (length(l)) return(as.numeric(strsplit(trimws(l[1L]), "\\s+")[[1L]][2L]) / 1024^2)
  }
  if (Sys.info()[["sysname"]] == "Darwin") {
    v <- tryCatch(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE),
                  error = function(e) "", warning = function(w) "")
    if (length(v) && nzchar(v[1L])) return(as.numeric(v[1L]) / 1024^3)
  }
  16
}

.emissions_ram_percent <- function() {
  if (!file.exists("/proc/meminfo")) return(0)
  l <- readLines("/proc/meminfo", warn = FALSE)
  get <- function(k) {
    m <- grep(paste0("^", k, ":"), l, value = TRUE)
    if (length(m)) as.numeric(strsplit(trimws(m[1L]), "\\s+")[[1L]][2L]) else NA_real_
  }
  total <- get("MemTotal")
  avail <- get("MemAvailable")
  if (is.na(total) || total <= 0) return(0)
  if (is.na(avail)) avail <- total
  100 * (1 - avail / total)
}

.emissions_cpu_brand <- function() {
  if (file.exists("/proc/cpuinfo")) {
    l <- grep("^model name", readLines("/proc/cpuinfo", warn = FALSE), value = TRUE, ignore.case = TRUE)
    if (length(l)) return(trimws(sub("^[^:]*:", "", l[1L])))
  }
  if (Sys.info()[["sysname"]] == "Darwin") {
    v <- tryCatch(system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = FALSE),
                  error = function(e) "", warning = function(w) "")
    if (length(v) && nzchar(v[1L])) return(v[1L])
  }
  as.character(Sys.info()[["machine"]])
}

.emissions_is_apple_silicon <- function() {
  Sys.info()[["sysname"]] == "Darwin" && Sys.info()[["machine"]] %in% c("arm64", "aarch64")
}

.emissions_cpu_tdp <- function(brand = .emissions_cpu_brand()) {
  # The model name decides; the host being Apple Silicon only matters when
  # the name is empty or unrecognised (an Apple runner must still map
  # "Intel Xeon Gold" to 150 W).
  b <- tolower(brand)
  if (grepl("apple m[34]|\\bm[34]\\b", b)) return(12)
  if (grepl("apple m2|\\bm2\\b", b)) return(15)
  if (grepl("apple m1|\\bm1\\b", b)) return(10)
  if (grepl("i9", b)) return(125)
  if (grepl("i7|i5", b)) return(65)
  if (grepl("ryzen 9", b)) return(105)
  if (grepl("ryzen 7", b)) return(65)
  if (grepl("xeon", b)) return(150)
  if (.emissions_is_apple_silicon() && (!nzchar(b) || grepl("apple", b))) return(12)
  85
}

.emissions_ram_power <- function(total_gb = .emissions_total_ram_gb()) {
  is_arm <- Sys.info()[["machine"]] %in% c("arm64", "aarch64")
  base <- if (is_arm) 1.5 else 5
  minimum <- if (is_arm) 3 else 10
  dimms <- if (total_gb <= 16) 2L else if (total_gb <= 64) 4L else if (total_gb <= 128) 8L else min(as.integer(total_gb / 16), 32L)
  power <- if (dimms <= 4L) {
    base * dimms
  } else if (dimms <= 8L) {
    base * 4 + base * 0.9 * (dimms - 4)
  } else if (dimms <= 16L) {
    base * 4 + base * 0.9 * 4 + base * 0.8 * (dimms - 8)
  } else {
    base * 4 + base * 0.9 * 4 + base * 0.8 * 8 + base * 0.7 * (dimms - 16)
  }
  max(power, minimum)
}

.emissions_cpu_ticks <- function() {
  if (!file.exists("/proc/stat")) return(NULL)
  v <- as.numeric(strsplit(trimws(readLines("/proc/stat", n = 1L, warn = FALSE)), "\\s+")[[1L]][-1L])
  v <- v[seq_len(min(8L, length(v)))]
  c(idle = v[4L] + if (length(v) > 4L) v[5L] else 0, total = sum(v))
}

.emissions_detect_location <- function() {
  env <- Sys.getenv("MORIE_COUNTRY_ISO", "")
  if (nzchar(env)) return(list(iso = toupper(env), region = Sys.getenv("MORIE_REGION", ""), name = "", lat = 0, lon = 0))
  if (nzchar(Sys.getenv("MORIE_EMISSIONS_OFFLINE", "")) || !requireNamespace("httr2", quietly = TRUE)) {
    return(list(iso = "", region = "", name = "", lat = 0, lon = 0))
  }
  tryCatch({
    req <- httr2::req_timeout(httr2::request("https://ipapi.co/json/"), 5)
    d <- httr2::resp_body_json(httr2::req_perform(req))
    list(iso = toupper(d$country_code %||% ""), region = d$region %||% "",
         name = d$country_name %||% "", lat = as.numeric(d$latitude %||% 0),
         lon = as.numeric(d$longitude %||% 0))
  }, error = function(e) list(iso = "", region = "", name = "", lat = 0, lon = 0))
}

.emissions_iso2_to_iso3 <- function(iso) {
  # ipapi returns two-letter codes; the energy mix is keyed by three letters.
  if (nchar(iso) != 2L) return(iso)
  map <- c(CA = "CAN", US = "USA", GB = "GBR", FR = "FRA", DE = "DEU", IN = "IND", CN = "CHN",
           AU = "AUS", JP = "JPN", BR = "BRA", MX = "MEX", IT = "ITA", ES = "ESP", NL = "NLD",
           SE = "SWE", NO = "NOR", FI = "FIN", DK = "DNK", CH = "CHE", IE = "IRL", NZ = "NZL",
           KR = "KOR", SG = "SGP", ZA = "ZAF", PL = "POL", BE = "BEL", AT = "AUT", PT = "PRT")
  unname(map[iso]) %||% ""
}

.emissions_csv_header <- c(
  "timestamp", "project_name", "run_id", "experiment_id", "duration", "emissions",
  "emissions_rate", "cpu_power", "gpu_power", "ram_power", "cpu_energy", "gpu_energy",
  "ram_energy", "energy_consumed", "water_consumed", "country_name", "country_iso_code",
  "region", "os", "python_version", "codecarbon_version", "cpu_count", "cpu_model",
  "gpu_count", "gpu_model", "ram_total_size", "tracking_mode", "cpu_utilization_percent",
  "gpu_utilization_percent", "ram_utilization_percent", "ram_used_gb", "on_cloud", "pue",
  "wue", "cloud_provider", "cloud_region")

#' Track the energy and carbon cost of a computation
#'
#' \code{morie_emissions_start()} opens a tracker; \code{morie_emissions_stop()}
#' closes it, appends one row to \code{emissions.csv} in \code{output_dir}
#' (the CodeCarbon column layout, the same file the Python package writes)
#' and, unless \code{capsule = FALSE}, seals the run in a bricklayer capsule:
#' \code{emissions_manifest.json} (the measurements, the method and the R
#' environment) and \code{capsule_bundle.json} (SHA-256 digests of both files
#' under a post-quantum ML-DSA signature). \code{morie_emissions_track()}
#' wraps an expression in the two calls.
#'
#' Power follows the CodeCarbon methodology: CPU thermal design power from
#' the model name scaled by utilisation as \eqn{TDP (0.1 + 0.9 u^3)}, RAM by
#' DIMM count, no GPU. Utilisation is sampled every \code{measure_power_secs}
#' by a C++ background thread reading the kernel's aggregate CPU counters
#' (\code{/proc/stat} on Linux, \code{host_statistics} on macOS) and power is
#' integrated per sample, as the Python tracker does (\code{"machine"}
#' tracking mode). Where no counters exist the R process's CPU time over
#' wall time is used instead (\code{"process"} mode). Emissions are energy times the grid's carbon
#' intensity (\code{\link{morie_emissions_carbon_intensity}}) times PUE. The
#' country comes from \code{country_iso_code}, else the
#' \code{MORIE_COUNTRY_ISO} environment variable, else a geolocation lookup
#' (skipped when \code{MORIE_EMISSIONS_OFFLINE} is set).
#'
#' The capsule's signing key is generated for the run unless \code{key} is
#' given, so a verifier learns that the files have not changed since they
#' were sealed, not who sealed them; pass your own
#' \code{rmoriebricklayer::fips_keygen()} key to bind the run to you.
#' @param project_name Label written to the CSV and the manifest.
#' @param output_dir Where \code{emissions.csv} and the capsule go.
#' @param output_file CSV file name.
#' @param pue,wue Power and water usage effectiveness multipliers.
#' @param country_iso_code,region Location overrides.
#' @param save_to_file Write the CSV row.
#' @param capsule Seal the run in a bricklayer capsule.
#' @param key A signing key from \code{rmoriebricklayer::fips_keygen()} or
#'   \code{pqc_keygen()}; generated per run when \code{NULL}.
#' @param measure_power_secs Seconds between utilisation samples of the
#'   background sampler (the Python arm's default is 15; 1 here, since R
#'   runs are usually shorter).
#' @param tracker The object from \code{morie_emissions_start()}.
#' @param expr An expression to evaluate under tracking.
#' @param ... Passed to \code{morie_emissions_start()}.
#' @return \code{morie_emissions_stop()}: the emissions in kg CO2eq, with
#'   the full row as attribute \code{data} and the capsule paths as attribute
#'   \code{capsule}. \code{morie_emissions_track()}: a list with
#'   \code{value}, \code{emissions_kg}, \code{data} and \code{capsule}.
#' @examples
#' dir <- tempfile()
#' r <- morie_emissions_track(sum(sqrt(1:1e5)), project_name = "demo",
#'                            output_dir = dir, country_iso_code = "CAN")
#' r$emissions_kg > 0
#' list.files(dir)
#' unlink(dir, recursive = TRUE)
#' @export
morie_emissions_start <- function(project_name = "morie", output_dir = ".",
                                  output_file = "emissions.csv", pue = 1, wue = 0,
                                  country_iso_code = "", region = "",
                                  save_to_file = TRUE, capsule = TRUE, key = NULL,
                                  measure_power_secs = 1) {
  t <- new.env(parent = emptyenv())
  t$project_name <- project_name
  t$output_dir <- output_dir
  t$output_file <- output_file
  t$pue <- pue
  t$wue <- wue
  t$save_to_file <- isTRUE(save_to_file)
  t$capsule <- isTRUE(capsule)
  t$key <- key
  t$run_id <- sprintf("r-%s-%d", format(Sys.time(), "%Y%m%d%H%M%OS3"), Sys.getpid())
  t$experiment_id <- t$run_id
  loc <- if (nzchar(country_iso_code)) {
    list(iso = toupper(country_iso_code), region = region, name = "", lat = 0, lon = 0)
  } else {
    .emissions_detect_location()
  }
  t$country_iso <- .emissions_iso2_to_iso3(loc$iso)
  t$region <- if (nzchar(region)) region else loc$region
  t$country_name <- loc$name
  t$latitude <- loc$lat
  t$longitude <- loc$lon
  t$cpu_model <- .emissions_cpu_brand()
  t$tdp <- .emissions_cpu_tdp(t$cpu_model)
  t$ram_power <- .emissions_ram_power()
  t$carbon_intensity <- morie_emissions_carbon_intensity(t$country_iso, t$region)
  t$ticks0 <- .emissions_cpu_ticks()
  t$proc0 <- proc.time()
  t$start <- Sys.time()
  t$sampler <- tryCatch(isTRUE(.emissions_sampler_start(measure_power_secs)), error = function(e) FALSE)
  class(t) <- "morie_emissions_tracker"
  t
}

.emissions_utilisation <- function(t, elapsed) {
  ticks1 <- .emissions_cpu_ticks()
  if (!is.null(t$ticks0) && !is.null(ticks1) && ticks1[["total"]] > t$ticks0[["total"]]) {
    didle <- ticks1[["idle"]] - t$ticks0[["idle"]]
    dtotal <- ticks1[["total"]] - t$ticks0[["total"]]
    return(list(pct = max(0, min(100, 100 * (1 - didle / dtotal))), mode = "machine"))
  }
  p <- proc.time() - t$proc0
  cores <- max(1L, parallel::detectCores(logical = TRUE))
  cpu_s <- sum(p[c("user.self", "sys.self")], na.rm = TRUE)
  pct <- if (elapsed > 0) 100 * cpu_s / (elapsed * cores) else 0
  list(pct = max(0, min(100, pct)), mode = "process")
}

#' @rdname morie_emissions_start
#' @export
morie_emissions_stop <- function(tracker) {
  t <- tracker
  if (!inherits(t, "morie_emissions_tracker")) stop("tracker must come from morie_emissions_start()", call. = FALSE)
  duration <- as.numeric(difftime(Sys.time(), t$start, units = "secs"))
  samples <- if (isTRUE(t$sampler)) tryCatch(.emissions_sampler_stop(), error = function(e) NULL) else NULL
  if (!is.null(samples) && nrow(samples) && all(samples[, "util_pct"] >= 0)) {
    dts <- samples[, "dt"]
    utils <- samples[, "util_pct"]
    cpu_energy <- sum(t$tdp * (0.1 + 0.9 * (utils / 100)^3) * dts) / 3.6e6
    u <- list(pct = sum(utils * dts) / sum(dts), mode = "machine")
    cpu_w <- if (sum(dts) > 0) cpu_energy * 3.6e6 / sum(dts) else 0
  } else {
    u <- .emissions_utilisation(t, duration)
    cpu_w <- t$tdp * (0.1 + 0.9 * (u$pct / 100)^3)
    cpu_energy <- cpu_w * duration / 3.6e6
  }
  ram_energy <- t$ram_power * duration / 3.6e6
  energy <- cpu_energy + ram_energy
  emissions <- energy * t$pue * t$carbon_intensity
  water <- energy * t$pue * t$wue
  ram_pct <- .emissions_ram_percent()
  total_gb <- .emissions_total_ram_gb()
  data <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    project_name = t$project_name, run_id = t$run_id, experiment_id = t$experiment_id,
    duration = duration, emissions = emissions,
    emissions_rate = if (duration > 0) emissions / duration else 0,
    cpu_power = cpu_w, gpu_power = 0, ram_power = t$ram_power,
    cpu_energy = cpu_energy, gpu_energy = 0, ram_energy = ram_energy,
    energy_consumed = energy, water_consumed = water,
    country_name = t$country_name, country_iso_code = t$country_iso, region = t$region,
    os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]], Sys.info()[["machine"]]),
    python_version = paste("R", getRversion()), codecarbon_version = "morie-r-1.0.0",
    cpu_count = parallel::detectCores(logical = TRUE), cpu_model = t$cpu_model,
    gpu_count = 0L, gpu_model = "", ram_total_size = total_gb, tracking_mode = u$mode,
    cpu_utilization_percent = u$pct, gpu_utilization_percent = 0,
    ram_utilization_percent = ram_pct, ram_used_gb = total_gb * ram_pct / 100,
    on_cloud = "N", pue = t$pue, wue = t$wue, cloud_provider = "", cloud_region = "")
  capsule <- NULL
  if (t$save_to_file) {
    dir.create(t$output_dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(t$output_dir, t$output_file)
    row <- vapply(.emissions_csv_header, function(k) {
      v <- data[[k]]
      if (is.numeric(v)) format(v, digits = 17) else gsub(",", ";", as.character(v))
    }, character(1))
    if (!file.exists(path)) cat(paste(.emissions_csv_header, collapse = ","), "\n", sep = "", file = path)
    cat(paste(row, collapse = ","), "\n", sep = "", file = path, append = TRUE)
    if (t$capsule) capsule <- .emissions_capsule(t, data, path)
  }
  structure(emissions, data = data, capsule = capsule)
}

.emissions_capsule <- function(t, data, csv_path) {
  dir <- t$output_dir
  manifest <- rmoriebricklayer::make_manifest(list(
    project = t$project_name, run_id = t$run_id, tool = "morie_emissions (R)",
    method = paste("CodeCarbon methodology: energy = (TDP x (0.1 + 0.9 u^3) + RAM) x time;",
                   "emissions = energy x carbon intensity x PUE"),
    sources = c("https://github.com/mlco2/codecarbon", "IEA / Our World in Data energy mix"),
    measurements = data[c("duration", "emissions", "energy_consumed", "cpu_power", "ram_power",
                          "cpu_utilization_percent", "tracking_mode", "country_iso_code")],
    carbon_intensity_kg_per_kwh = t$carbon_intensity,
    emissions_csv = basename(csv_path)), environment = TRUE)
  mpath <- file.path(dir, "emissions_manifest.json")
  rmoriebricklayer::write_manifest_json(manifest, mpath)
  bpath <- file.path(dir, "capsule_bundle.json")
  signed <- FALSE
  if (exists("capsule_bundle", envir = asNamespace("rmoriebricklayer"), inherits = FALSE)) {
    key <- t$key %||% rmoriebricklayer::fips_keygen("ML-DSA-44")
    rmoriebricklayer::capsule_bundle(dir, manifest, key,
                                     files = c(basename(csv_path), basename(mpath)),
                                     note = paste("morie emissions run", t$run_id), path = bpath)
    signed <- TRUE
  }
  list(manifest = mpath, bundle = if (signed) bpath else NULL, signed = signed)
}

#' @rdname morie_emissions_start
#' @export
morie_emissions_track <- function(expr, ...) {
  t <- morie_emissions_start(...)
  value <- force(expr)
  e <- morie_emissions_stop(t)
  list(value = value, emissions_kg = as.numeric(e), data = attr(e, "data"), capsule = attr(e, "capsule"))
}

#' Verify an emissions capsule
#'
#' Re-hashes \code{emissions.csv} and \code{emissions_manifest.json} in
#' \code{dir} and checks them and the manifest digest against the signed
#' \code{capsule_bundle.json}.
#' @param dir The directory \code{morie_emissions_stop()} wrote to.
#' @return A list with \code{ok} and the bricklayer \code{checks} table.
#' @examples
#' dir <- tempfile()
#' morie_emissions_track(sum(1:10), output_dir = dir, country_iso_code = "CAN")
#' morie_emissions_verify(dir)$ok
#' unlink(dir, recursive = TRUE)
#' @export
morie_emissions_verify <- function(dir) {
  bpath <- file.path(dir, "capsule_bundle.json")
  if (!file.exists(bpath)) stop("no capsule_bundle.json in ", dir, call. = FALSE)
  rmoriebricklayer::capsule_bundle_verify(bpath, dir)
}

.emissions_text <- function(e) {
  d <- attr(e, "data")
  c <- attr(e, "capsule")
  lines <- c(
    sprintf("Emissions:         %.6g kg CO2eq over %.1f s", as.numeric(e), d$duration),
    sprintf("Energy:            %.6g kWh (CPU %.6g, RAM %.6g)", d$energy_consumed, d$cpu_energy, d$ram_energy),
    sprintf("CPU:               %s, TDP-scaled %.1f W at %.1f%% utilisation (%s mode)",
            d$cpu_model, d$cpu_power, d$cpu_utilization_percent, d$tracking_mode),
    sprintf("Carbon intensity:  %.4g kg/kWh (%s)", if (d$energy_consumed > 0) as.numeric(e) / d$energy_consumed / d$pue else NA,
            if (nzchar(d$country_iso_code)) d$country_iso_code else "world average; set MORIE_COUNTRY_ISO"))
  if (!is.null(c)) {
    lines <- c(lines, sprintf("Capsule:           %s%s", c$manifest,
                              if (c$signed) paste0(" + signed ", c$bundle) else " (unsigned: rmoriebricklayer too old for capsule_bundle)"))
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}
