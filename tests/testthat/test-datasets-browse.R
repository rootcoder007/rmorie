# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD4: morie_datasets_browse() + morie_datasets_summary()
# interactive helpers over the cross-portal catalog.

test_that("morie_datasets_browse() default returns full catalog", {
  d <- morie_datasets_browse()
  full <- morie_dataset_portal_catalog()
  expect_equal(nrow(d), nrow(full))
  expect_setequal(names(d), names(full))
})

test_that("portal filter narrows to a single portal", {
  d <- morie_datasets_browse(portal = "tps_psdp")
  expect_true(nrow(d) >= 11L)
  expect_true(all(d$source == "tps_psdp"))
})

test_that("portal filter accepts multiple portals", {
  d <- morie_datasets_browse(portal = c("nyc_nypd", "chicago"))
  expect_true(all(d$source %in% c("nyc_nypd", "chicago")))
  expect_true(nrow(d) >= 10L)
})

test_that("keyword filter matches dataset_key + id + loader", {
  d <- morie_datasets_browse(keyword = "homicide")
  # 'homicides' TPS PSDP layer + '35100026' StatCan homicide victims
  # (matches via the loader). The TPS one definitely shows.
  expect_true("homicides" %in% d$dataset_key)
  # Case insensitive
  d2 <- morie_datasets_browse(keyword = "HOMICIDE")
  expect_equal(nrow(d), nrow(d2))
})

test_that("api_mode filter restricts to a protocol substring", {
  d <- morie_datasets_browse(api_mode = "soda3")
  # All matched rows must mention soda3.
  expect_true(all(grepl("soda3", d$api_modes)))
  # Socrata-backed sources: NYC NYPD + Chicago + NYC OpenData +
  # Calgary + Edmonton (3FFF3).
  expect_setequal(unique(d$source),
                  c("nyc_nypd", "chicago", "nyc_opendata",
                    "calgary_opendata", "edmonton_opendata"))
})

test_that("api_mode filter accepts multi-protocol vector", {
  d <- morie_datasets_browse(api_mode = c("ckan", "statcan_wds"))
  # ckan matches ontario_ckan + montreal_opendata + toronto_opendata;
  # statcan_wds matches statcan_ccjs. (Calgary + Edmonton are soda*,
  # Ottawa is arcgis.)
  expect_setequal(unique(d$source),
                  c("ontario_ckan", "montreal_opendata",
                    "toronto_opendata", "statcan_ccjs"))
})

test_that("loader_pattern uses perl-style regex", {
  d <- morie_datasets_browse(loader_pattern = "^morie_datasets_tps")
  # All loader names start with morie_datasets_tps
  expect_true(all(grepl("^morie_datasets_tps", d$loader)))
})

test_that("filters compose with AND semantics", {
  d <- morie_datasets_browse(portal = "tps_psdp",
                              keyword = "assault")
  expect_true(all(d$source == "tps_psdp"))
  expect_true(any(grepl("assault", d$dataset_key, ignore.case = TRUE)))
})

test_that("sort_by orders rows", {
  d_key <- morie_datasets_browse(sort_by = "dataset_key")
  expect_equal(d_key$dataset_key, sort(d_key$dataset_key))
  d_src <- morie_datasets_browse(sort_by = "source")
  expect_equal(d_src$source, sort(d_src$source))
})

test_that("invalid sort_by raises", {
  expect_error(morie_datasets_browse(sort_by = "magic"))
})

test_that("morie_datasets_summary() returns one row per portal", {
  s <- morie_datasets_summary()
  expect_s3_class(s, "data.frame")
  expect_setequal(names(s),
                  c("source", "n_datasets", "api_modes",
                    "n_with_bundled_fixture"))
  # Should match the catalog's 14 portals
  # (3FFF3 added Calgary + Edmonton + Ottawa).
  expect_equal(nrow(s), 14L)
  # n_datasets must sum to the catalog total.
  expect_equal(sum(s$n_datasets),
                nrow(morie_dataset_portal_catalog()))
})

test_that("summary api_modes column is comma-separated unique", {
  s <- morie_datasets_summary()
  # No duplicates within any single row's api_modes csv.
  for (i in seq_len(nrow(s))) {
    modes <- strsplit(s$api_modes[i], ",")[[1]]
    expect_equal(length(modes), length(unique(modes)),
                  info = sprintf("source=%s has dup modes", s$source[i]))
  }
})
