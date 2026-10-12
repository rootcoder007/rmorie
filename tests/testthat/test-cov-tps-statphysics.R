# R/tps_statphysics.R on exact synthetic frames served through the loader options, so every
# data-seeded routine runs offline and each reported number is recomputed in the test body.

.sp_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.sp <- function(name) get(name, envir = environment(morie_tps_urban_scaling_beta))

.sp_wards <- function(beta = 1.2, y0 = 0.002, n = 158, pop_col = "POPULATION_2024", crime_col = "ASSAULT_2024") {
  set.seed(5)
  pop <- round(stats::runif(n, 5000, 60000))
  out <- data.frame(HOOD_ID = seq_len(n), stringsAsFactors = FALSE)
  out[[pop_col]] <- pop
  out[[crime_col]] <- y0 * pop^beta
  out
}

test_that("the figure writer says when it skips and writes a PNG when asked", {
  fig <- .sp(".tps_sp_fig")
  expect_equal(fig(NULL, "a.png", function() NULL, save_fig = FALSE), "(skipped)")
  expect_equal(fig(NULL, "a.png", function() NULL), "(skipped: pass fig_dir= to write the PNG)")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  dir <- withr::local_tempdir()
  p <- fig(file.path(dir, "sub"), "a.png", function() graphics::plot(1:3))
  expect_equal(p, file.path(dir, "sub", "a.png"))
  expect_true(file.exists(p))
})

test_that("the urban-scaling fit recovers an exact power law and labels the regime", {
  for (beta in c(1.2, 1, 0.8)) {
    w <- .sp_wards(beta)
    withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) w)
    r <- morie_tps_urban_scaling_beta("Assault", year = 2024L, save_fig = FALSE)
    expect_s3_class(r, "morie_tps_statphysics_result")
    expect_length(r$warnings, 0)
    expect_equal(r$payload$beta, beta, tolerance = 1e-10)
    expect_equal(r$payload$Y0, 0.002, tolerance = 1e-8)
    expect_equal(r$payload$se_beta, 0, tolerance = 1e-8)
    expect_equal(r$payload$r2, 1, tolerance = 1e-10)
    sl <- r$summary_lines
    expect_equal(sl$n_wards, 158L)
    expect_equal(sl$crime_col, "ASSAULT_2024")
    expect_equal(sl$pop_col, "POPULATION_2024")
    expect_equal(sl$beta, round(beta, 3))
    expect_equal(sl$Regime, switch(as.character(beta), "1.2" = "super-linear (beta > 1)", "1" = "linear (beta ~ 1)",
                                   "0.8" = "sub-linear (beta < 1)"))
    expect_equal(sl$Figure, "(skipped)")
    expect_match(r$interpretation, sprintf("pop\\^%.2f", beta))
  }
  # a category outside the prefix map is upper-cased; a population column without the year still serves
  w <- .sp_wards(1.1, pop_col = "POPULATION", crime_col = "FOO_2024")
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) w)
  r <- morie_tps_urban_scaling_beta("Foo", year = 2024L, save_fig = FALSE)
  expect_equal(r$summary_lines$crime_col, "FOO_2024")
  expect_equal(r$summary_lines$pop_col, "POPULATION")
  expect_equal(r$payload$beta, 1.1, tolerance = 1e-10)
  # the fit only uses wards above 100 people with a positive count
  w2 <- .sp_wards(1.2)
  w2$POPULATION_2024[1:3] <- 50
  w2$ASSAULT_2024[4:6] <- 0
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) w2)
  expect_equal(morie_tps_urban_scaling_beta("Assault", save_fig = FALSE)$summary_lines$n_wards, 152L)
  # failures
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) .sp_wards(1.2)[1:20, ])
  expect_equal(morie_tps_urban_scaling_beta("Assault", save_fig = FALSE)$warnings, "only 20 usable wards")
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) .sp_wards(1.2))
  expect_equal(morie_tps_urban_scaling_beta("Robbery", save_fig = FALSE)$warnings,
               "missing pop or ROBBERY_2024 column (have pop_col=POPULATION_2024)")
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) data.frame(ASSAULT_2024 = 1:5))
  expect_equal(morie_tps_urban_scaling_beta("Assault", save_fig = FALSE)$warnings,
               "missing pop or ASSAULT_2024 column (have pop_col=NULL)")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) .sp_wards(1.2))
  dir <- withr::local_tempdir()
  r <- morie_tps_urban_scaling_beta("Assault", year = 2024L, fig_dir = dir)
  expect_true(file.exists(file.path(dir, "urban_scaling_assault_2024.png")))
  expect_equal(r$summary_lines$Figure, file.path(dir, "urban_scaling_assault_2024.png"))
})

test_that("the Lotka-Volterra proxy fit follows its medians and the linearised period", {
  counts <- c(120, 150, 170, 160, 190, 230, 210, 250, 240, 280)
  years <- 2012:2021
  df <- data.frame(OCC_YEAR = rep(years, counts))
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  r <- morie_tps_lotka_volterra_police_crime("Assault", save_fig = FALSE)
  expect_length(r$warnings, 0)
  x <- counts[years >= 2014]
  y <- as.numeric(stats::filter(x, rep(1 / 3, 3), sides = 1))
  y[1:2] <- x[1:2]
  alpha <- stats::median(diff(log(x))) + 0.05
  gamma <- -stats::median(diff(log(y))) + 0.05
  period <- 2 * pi / sqrt(max(alpha * gamma, 1e-6))
  expect_equal(r$payload$alpha, alpha)
  expect_equal(r$payload$gamma, gamma)
  expect_equal(r$payload$beta, alpha / stats::median(y))
  expect_equal(r$payload$delta, gamma / stats::median(x))
  expect_equal(r$payload$cycle_period_years, period)
  expect_equal(r$payload$years, 2014:2021)
  expect_equal(r$payload$x, x)
  expect_equal(r$summary_lines$Years, "2014-2021")
  expect_equal(r$summary_lines$cycle_yr, round(period, 1))
  expect_match(r$interpretation, sprintf("T ~ %.1f years", period))
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) data.frame(x = 1))
  expect_equal(morie_tps_lotka_volterra_police_crime("Assault", save_fig = FALSE)$warnings, "no OCC_YEAR column")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) data.frame(OCC_YEAR = rep(2014:2017, 5)))
  expect_equal(morie_tps_lotka_volterra_police_crime("Assault", save_fig = FALSE)$warnings, "only 4 years")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  dir <- withr::local_tempdir()
  r <- morie_tps_lotka_volterra_police_crime("Assault", fig_dir = dir)
  expect_true(file.exists(file.path(dir, "lotka_volterra_Assault.png")))
})

test_that("the premise network counts shared neighbourhoods and names its failures", {
  hood <- c("001", "001", "002", "002", "003", "003", "004", "004", "004", "005")
  prem <- c("House", "Apartment", "House", "Outside", "Apartment", "Outside", "House", "Apartment", "Transit", "Transit")
  df <- data.frame(HOOD_158 = hood, PREMISES_TYPE = prem, stringsAsFactors = FALSE)
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  r <- morie_tps_criminal_network_graph("Assault", top_n_premises = 3L, save_fig = FALSE)
  expect_length(r$warnings, 0)
  # the three most frequent premises: House, Apartment (3 each) and Outside / Transit (2 each; the first wins)
  freq <- sort(table(prem), decreasing = TRUE)
  top <- names(freq)[1:3]
  sub <- df[df$PREMISES_TYPE %in% top, ]
  pb <- pmin(unclass(table(sub$HOOD_158, sub$PREMISES_TYPE)), 1L)
  co <- t(pb) %*% pb
  diag(co) <- 0L
  expect_equal(r$payload$nodes, colnames(co))
  expect_equal(unname(r$payload$co), unname(co))
  expect_equal(r$summary_lines$Nodes, 3L)
  expect_equal(r$summary_lines$Edges_pos, sum(co > 0) %/% 2L)
  expect_equal(r$summary_lines$max_edge_weight, as.integer(max(co)))
  expect_equal(r$payload$sizes, as.numeric(freq[1:3]))
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df[, "PREMISES_TYPE", drop = FALSE])
  expect_equal(morie_tps_criminal_network_graph("Assault", save_fig = FALSE)$warnings, "missing HOOD_158")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df[, "HOOD_158", drop = FALSE])
  expect_match(morie_tps_criminal_network_graph("Assault", save_fig = FALSE)$warnings, "no usable node-attribute column")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  expect_equal(morie_tps_criminal_network_graph("Assault", top_n_premises = 1L, save_fig = FALSE)$warnings, "only 1 nodes")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  dir <- withr::local_tempdir()
  r <- morie_tps_criminal_network_graph("Assault", fig_dir = dir)
  expect_true(file.exists(file.path(dir, "network_Assault.png")))
})

test_that("the Levy tail exponent is the Hill estimate on steps above the floor", {
  # a walk through Toronto with Pareto step lengths, in time order
  set.seed(9)
  n <- 400
  steps <- 0.3 * (1 - stats::runif(n - 1))^(-1 / 1.6)
  ang <- stats::runif(n - 1, 0, 2 * pi)
  x <- cumsum(c(0, steps * cos(ang)))
  y <- cumsum(c(0, steps * sin(ang)))
  # fold the walk back into a 20 km box so every point stays inside the bbox
  fold <- function(v, w) abs(((v + w) %% (2 * w)) - w)
  x <- fold(x, 10)
  y <- fold(y, 10)
  lat_ref <- (43.55 + 43.90) / 2
  lon_ref <- (-79.65 + -79.10) / 2
  lat <- lat_ref + y / 111.32
  lon <- lon_ref + x / (111.32 * cos(lat_ref * pi / 180))
  when <- as.POSIXct("2020-01-01", tz = "UTC") + 3600 * seq_len(n)
  df <- data.frame(LAT_WGS84 = lat, LONG_WGS84 = lon, OCC_DATE = format(when, "%Y-%m-%dT%H:%M:%S"),
                   stringsAsFactors = FALSE)
  # shuffle: the routine sorts by time itself
  df <- df[sample(n), ]
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  r <- morie_tps_levy_flight_alpha("Assault", lmin_km = 0.5, save_fig = FALSE)
  expect_length(r$warnings, 0)
  prj <- .sp(".tps_sp_project_xy")(lat, lon)
  s <- sqrt(diff(prj$x)^2 + diff(prj$y)^2)
  s <- s[s > 0.5]
  alpha <- 1 + length(s) / sum(log(s / 0.5))
  expect_equal(r$payload$alpha, alpha, tolerance = 1e-12)
  expect_equal(r$payload$n, length(s))
  expect_equal(sort(r$payload$steps), sort(s))
  .sp(".rmorie_local_seed")(11L)
  boots <- vapply(seq_len(200L), function(i) {
    b <- sample(r$payload$steps, length(s), replace = TRUE)
    1 + length(s) / sum(log(b / 0.5))
  }, numeric(1))
  expect_equal(r$payload$se, stats::sd(boots))
  expect_equal(r$summary_lines$alpha, round(alpha, 3))
  expect_equal(r$summary_lines$n_tail_steps, length(s))
  expect_equal(r$summary_lines$max_step_km, round(max(s), 2))
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df[, 1:2])
  expect_equal(morie_tps_levy_flight_alpha("Assault", save_fig = FALSE)$warnings, "missing columns: OCC_DATE")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df[1:100, ])
  expect_equal(morie_tps_levy_flight_alpha("Assault", save_fig = FALSE)$warnings, "too few rows for Levy fit")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  expect_match(morie_tps_levy_flight_alpha("Assault", lmin_km = 1e4, save_fig = FALSE)$warnings,
               "^only 0 tail steps >= 10000 km$")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  dir <- withr::local_tempdir()
  r <- morie_tps_levy_flight_alpha("Assault", fig_dir = dir)
  expect_true(file.exists(file.path(dir, "levy_alpha_Assault.png")))
})

test_that("the SDB reaction-diffusion run reports its grid, spikes and the DBSCAN count it was given", {
  set.seed(2)
  n <- 300
  df <- data.frame(LAT_WGS84 = stats::runif(n, 43.6, 43.85), LONG_WGS84 = stats::runif(n, -79.6, -79.15))
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  run <- function(...) morie_tps_sdb_reaction_diffusion("Assault", nx = 12L, ny = 8L, n_steps = 10L, save_fig = FALSE, ...)
  testthat::local_mocked_bindings(morie_tps_dbscan_clusters = function(df, ...) list(payload = list(n_clusters = 3L)),
                                  .package = .sp_pkg)
  r <- run()
  expect_length(r$warnings, 0)
  sl <- r$summary_lines
  expect_equal(sl$DBSCAN_clusters, 3L)
  expect_match(sl$Grid, "^12x8 cos-corrected")
  expect_equal(sl$Parameters, "eta=0.05 omega=0.3 theta=1.5 D=0.1 gamma=0.05 dt=0.04 steps=10")
  expect_true(is.numeric(sl$SteadySpikes) && sl$SteadySpikes >= 0)
  expect_true(sl$MeanA > 0 && sl$MeanRho >= 0)
  expect_equal(sl$Figure, "(skipped)")
  # the DBSCAN count: missing, unparsable, and failing companions
  testthat::local_mocked_bindings(morie_tps_dbscan_clusters = function(df, ...) list(payload = list()), .package = .sp_pkg)
  expect_equal(run()$summary_lines$DBSCAN_clusters, 0L)
  testthat::local_mocked_bindings(morie_tps_dbscan_clusters = function(df, ...) list(payload = list(n_clusters = "many")),
                                  .package = .sp_pkg)
  expect_equal(run()$summary_lines$DBSCAN_clusters, 0L)
  testthat::local_mocked_bindings(morie_tps_dbscan_clusters = function(df, ...) stop("no clusters today"), .package = .sp_pkg)
  expect_equal(run()$summary_lines$DBSCAN_clusters, "(unavailable)")
  # failures
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df[, 1, drop = FALSE])
  expect_equal(run()$warnings, "missing columns: LONG_WGS84")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) transform(df, LAT_WGS84 = 45))
  expect_equal(run()$warnings, "Assault: no in-bbox rows")
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df)
  dir <- withr::local_tempdir()
  r <- morie_tps_sdb_reaction_diffusion("Assault", nx = 12L, ny = 8L, n_steps = 5L, fig_dir = dir)
  expect_true(file.exists(file.path(dir, "sdb_pde_Assault.png")))
})

test_that("the inspection game sweeps a small temptation-by-cost grid", {
  r <- morie_tps_inspection_game_phase(n_temptations = 4L, n_costs = 3L, n_steps = 30L, save_fig = FALSE)
  expect_s3_class(r, "morie_tps_statphysics_result")
  crime <- r$payload$crime
  expect_equal(dim(crime), c(4L, 3L))
  expect_true(all(crime >= 0 & crime <= 1))
  skip_if_not(isTRUE(capabilities("png")), "no png device")
  dir <- withr::local_tempdir()
  r <- morie_tps_inspection_game_phase(n_temptations = 3L, n_costs = 3L, n_steps = 10L, fig_dir = dir)
  expect_true(file.exists(file.path(dir, "inspection_game_phase.png")))
})

test_that("the hub-layer loader honours its override, its cache, and pages the ArcGIS feed", {
  withr::local_options(morie.tps_loader = function(name, format = "geojson", ...) data.frame(a = 1))
  expect_equal(morie_tps_load_tps("NeighbourhoodCrimeRates"), data.frame(a = 1))
  withr::local_options(morie.tps_loader = NULL)
  expect_error(morie_tps_load_tps("NoSuchLayer"), "Unknown TPS layer: NoSuchLayer")
  dir <- withr::local_tempdir()
  cached <- data.frame(HOOD_ID = 1:2, POPULATION_2024 = c(10, 20))
  utils::write.csv(cached, file.path(dir, "tps_hub_NeighbourhoodCrimeRates.csv"), row.names = FALSE)
  expect_equal(morie_tps_load_tps("NeighbourhoodCrimeRates", cache_dir = dir), cached)
  # two pages, then the cache is written
  dir2 <- withr::local_tempdir()
  seen <- character()
  testthat::local_mocked_bindings(
    .morie_from_json = function(url, ...) {
      seen <<- c(seen, url)
      off <- as.integer(sub(".*resultOffset=", "", url))
      if (off == 0L) {
        list(features = list(list(attributes = list(HOOD_ID = 1L, POP = 10L)),
                             list(attributes = list(HOOD_ID = 2L, POP = NULL))),
             exceededTransferLimit = TRUE)
      } else {
        list(features = list(list(attributes = list(HOOD_ID = 3L, POP = 30L))), exceededTransferLimit = FALSE)
      }
    },
    .package = .sp_pkg)
  got <- morie_tps_load_tps("NeighbourhoodCrimeRates", cache_dir = dir2)
  expect_equal(got$HOOD_ID, 1:3)
  expect_equal(got$POP, c(10L, NA, 30L))
  expect_length(seen, 2)
  expect_match(seen[2], "resultOffset=2$")
  expect_true(file.exists(file.path(dir2, "tps_hub_NeighbourhoodCrimeRates.csv")))
  # a dead feed, and an empty one
  testthat::local_mocked_bindings(.morie_from_json = function(url, ...) stop("down"), .package = .sp_pkg)
  expect_error(morie_tps_load_tps("NeighbourhoodCrimeRates", cache_dir = withr::local_tempdir()),
               "ArcGIS request failed at offset 0")
  testthat::local_mocked_bindings(.morie_from_json = function(url, ...) list(features = list()), .package = .sp_pkg)
  expect_error(morie_tps_load_tps("NeighbourhoodCrimeRates", cache_dir = withr::local_tempdir()),
               "No features returned")
  # an incident category goes to the dataset loader
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) data.frame(cat = category))
  expect_equal(morie_tps_load_tps("Assault"), data.frame(cat = "Assault"))
})

test_that("the dataset loader tries the project export, then the live fetch with a row cap", {
  withr::local_options(morie.tps_loader_dataset = NULL)
  testthat::local_mocked_bindings(morie_tps_load_dataset = function(name, nrows = NULL, ...) data.frame(n = nrows),
                                  .package = .sp_pkg)
  expect_equal(morie_tps_load_tps_dataset("Assault", nrows = 7), data.frame(n = 7))
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(OCC_YEAR = 2014:2023, X = 1:10), csv, row.names = FALSE)
  testthat::local_mocked_bindings(
    morie_tps_load_dataset = function(name, nrows = NULL, ...) stop("no export"),
    morie_fetch_tps = function(category, cache_dir, ...) csv,
    .package = .sp_pkg)
  full <- morie_tps_load_tps_dataset("Assault")
  expect_equal(nrow(full), 10L)
  expect_equal(nrow(morie_tps_load_tps_dataset("Assault", nrows = 4)), 4L)
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) data.frame(cat = category, n = nrows))
  expect_equal(morie_tps_load_tps_dataset("Robbery", nrows = 3), data.frame(cat = "Robbery", n = 3))
})

test_that("analyze_all runs the four data-seeded routines per category", {
  counts <- c(120, 150, 170, 160, 190, 230, 210)
  df <- data.frame(OCC_YEAR = rep(2014:2020, counts), LAT_WGS84 = 43.7, LONG_WGS84 = -79.4,
                   OCC_DATE = "2020-01-01T00:00:00", HOOD_158 = "001")
  withr::local_options(morie.tps_loader_dataset = function(category, nrows = NULL) df,
                       morie.tps_loader = function(name, format = "geojson", ...) .sp_wards(1.2))
  out <- morie_tps_statphysics_analyze_all(c("Assault", "Robbery"), save_fig = FALSE)
  expect_equal(names(out), c("Assault", "Robbery"))
  expect_equal(names(out$Assault), c("sdb_pde", "levy", "urban_scaling", "lotka_volterra"))
  expect_s3_class(out$Assault$lotka_volterra, "morie_tps_statphysics_result")
  expect_length(out$Assault$lotka_volterra$warnings, 0)
  expect_equal(out$Robbery$urban_scaling$warnings,
               "missing pop or ROBBERY_2024 column (have pop_col=POPULATION_2024)")
  expect_equal(out$Assault$levy$warnings, "only 0 tail steps >= 0.5 km")
})
