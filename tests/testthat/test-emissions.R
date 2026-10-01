# Compute-emissions tracker: carbon intensities from the shipped energy mix,
# the CodeCarbon power formula recomputed from the row, CSV layout, capsule.

test_that("carbon intensity: country ordering, world average, weighted mix", {
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  fra <- morie_emissions_carbon_intensity("FRA")
  usa <- morie_emissions_carbon_intensity("USA")
  can <- morie_emissions_carbon_intensity("CAN")
  expect_true(fra < usa)
  expect_true(can < usa)
  expect_true(fra > 0 && usa < 1)
  expect_equal(morie_emissions_carbon_intensity("ZZZ"), 0.475)
  expect_equal(morie_emissions_carbon_intensity(""), 0.475)
  mix <- .emissions_energy_mix()
  expect_equal(can, mix$CAN$carbon_intensity / 1000)
})

test_that("hardware heuristics follow the CodeCarbon tables", {
  expect_equal(.emissions_cpu_tdp("Intel(R) Core(TM) i7-1185G7"), 65)
  expect_equal(.emissions_cpu_tdp("AMD Ryzen 9 5950X"), 105)
  expect_equal(.emissions_cpu_tdp("Intel Xeon Gold"), 150)
  expect_equal(.emissions_cpu_tdp("something else"), if (.emissions_is_apple_silicon()) 12 else 85)
  is_arm <- Sys.info()[["machine"]] %in% c("arm64", "aarch64")
  expect_equal(.emissions_ram_power(16), if (is_arm) 3 else 10)
  expect_equal(.emissions_ram_power(64), if (is_arm) 6 else 20)
  expect_equal(.emissions_ram_power(128), if (is_arm) 1.5 * 4 + 1.5 * 0.9 * 4 else 5 * 4 + 5 * 0.9 * 4)
})

test_that("a tracked run writes the CodeCarbon CSV row and the formula holds", {
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  d <- withr::local_tempdir()
  r <- morie_emissions_track({
    x <- 0
    for (i in 1:30) x <- x + sum(sqrt(seq_len(20000)))
    x
  }, project_name = "t", output_dir = d, country_iso_code = "CAN", capsule = FALSE,
  measure_power_secs = 0.2)
  expect_true(r$value > 0)
  dat <- r$data
  expect_true(r$emissions_kg > 0)
  expect_equal(r$emissions_kg, dat$energy_consumed * dat$pue * morie_emissions_carbon_intensity("CAN"))
  expect_equal(dat$energy_consumed, dat$cpu_energy + dat$ram_energy)
  expect_equal(dat$ram_energy, dat$ram_power * dat$duration / 3.6e6)
  expect_true(dat$cpu_utilization_percent >= 0 && dat$cpu_utilization_percent <= 100)
  expect_true(dat$tracking_mode %in% c("machine", "process"))
  if (file.exists("/proc/stat") || Sys.info()[["sysname"]] == "Darwin") expect_equal(dat$tracking_mode, "machine")
  csv <- utils::read.csv(file.path(d, "emissions.csv"), stringsAsFactors = FALSE)
  expect_equal(names(csv), .emissions_csv_header)
  expect_length(.emissions_csv_header, 36L)
  expect_equal(nrow(csv), 1L)
  expect_equal(csv$emissions, r$emissions_kg, tolerance = 1e-12)
  expect_equal(csv$country_iso_code, "CAN")
  morie_emissions_track(1, output_dir = d, country_iso_code = "CAN", capsule = FALSE)
  expect_equal(nrow(utils::read.csv(file.path(d, "emissions.csv"))), 2L)
})

test_that("the background sampler reports intervals that sum to the run", {
  skip_if_not(file.exists("/proc/stat") || Sys.info()[["sysname"]] == "Darwin", "no CPU counters")
  expect_true(.emissions_sampler_start(0.1))
  expect_true(.emissions_sampler_running())
  expect_false(.emissions_sampler_start(0.1))
  Sys.sleep(0.55)
  m <- .emissions_sampler_stop()
  expect_false(.emissions_sampler_running())
  expect_true(nrow(m) >= 4L)
  expect_equal(colnames(m), c("dt", "util_pct"))
  expect_true(all(m[, "util_pct"] >= 0 & m[, "util_pct"] <= 100))
  expect_equal(sum(m[, "dt"]), 0.55, tolerance = 0.25)
  expect_equal(nrow(.emissions_sampler_stop()), 0L)
})

test_that("the capsule seals the CSV and manifest and verifies, and detects tampering", {
  withr::local_envvar(MORIE_EMISSIONS_OFFLINE = "1")
  d <- withr::local_tempdir()
  r <- morie_emissions_track(sum(1:10), output_dir = d, country_iso_code = "CAN")
  expect_true(file.exists(r$capsule$manifest))
  skip_if_not(isTRUE(r$capsule$signed), "rmoriebricklayer without capsule_bundle")
  expect_true(file.exists(file.path(d, "capsule_bundle.json")))
  v <- morie_emissions_verify(d)
  expect_true(v$ok)
  cat("tampered\n", file = file.path(d, "emissions.csv"), append = TRUE)
  expect_false(morie_emissions_verify(d)$ok)
  expect_error(morie_emissions_verify(withr::local_tempdir()), "no capsule_bundle")
})
