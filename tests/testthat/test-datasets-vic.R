# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Victorian crime data loaders and analyses. No network: the loaders are
# offline by default and the analyses are driven from fixtures shaped
# like the published tables.

test_that("the catalog lists every published workbook", {
  cd <- morie_datasets_vic_catalog()
  expect_s3_class(cd, "data.frame")
  expect_equal(nrow(cd), 27L)
  expect_setequal(names(cd), c("key", "file", "url"))
  expect_false(any(duplicated(cd$key)))
  expect_true(all(nzchar(cd$key)))
  expect_true(all(grepl("^https://", cd$url)))
  expect_true(all(grepl("\\.xlsx$", cd$file)))
  # the keys the analyses below name must exist
  for (k in c("criminal_incidents", "lga_criminal_incidents",
              "indigenous_victim_reports")) {
    expect_true(k %in% cd$key, info = k)
  }
})

test_that("an unknown key is rejected rather than silently empty", {
  expect_error(morie_datasets_vic_table("no_such_table"), "unknown key")
  expect_error(morie_datasets_vic_sheets("no_such_table"), "unknown key")
})

test_that("the loader stays offline by default", {
  d <- morie_datasets_vic_table("criminal_incidents", table = 1,
                                cache_dir = tempfile())
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 0L)
  expect_equal(morie_datasets_vic_sheets("criminal_incidents",
                                         cache_dir = tempfile()),
               character(0))
})

test_that("offence trend reports the change per division", {
  d <- data.frame(
    Year = c(2024, 2025, 2024, 2025),
    `Offence Division` = c("A", "A", "B", "B"),
    `Incidents Recorded` = c(100, 150, 80, 60),
    check.names = FALSE
  )
  tr <- morie_vic_offence_trend(d)
  expect_equal(nrow(tr), 2L)
  a <- tr[tr$division == "A", ]
  b <- tr[tr$division == "B", ]
  # 100 -> 150 is +50 and +50%; 80 -> 60 is -20 and -25%
  expect_equal(a$abs_change, 50)
  expect_equal(a$pct_change, 50)
  expect_equal(b$abs_change, -20)
  expect_equal(b$pct_change, -25)
  expect_error(morie_vic_offence_trend(data.frame(Year = 1)),
               "missing column")
})

test_that("LGA rates rank by rate and honour top", {
  d <- data.frame(
    Year = c(2025, 2025, 2025),
    `Local Government Area` = c("Alpha", "Beta", "Gamma"),
    `Incidents Recorded` = c(500, 300, 900),
    `Rate per 100,000 population` = c(1200, 800, 2500),
    check.names = FALSE
  )
  r <- morie_vic_lga_rates(d, top = 2)
  expect_equal(nrow(r), 2L)
  expect_equal(r$lga, c("Gamma", "Alpha"))
  expect_equal(r$rank, c(1L, 2L))
  expect_true(all(diff(r$rate) <= 0))
})

test_that("the Indigenous ratio excludes the Total People rows", {
  # The published table carries a third "Total People" status. Folding
  # it into the non-Indigenous count roughly doubles the denominator,
  # so this fixture fails loudly if the filter is dropped.
  d <- data.frame(
    Year = c(2026, 2026, 2026),
    `Offence Division` = c("A", "A", "A"),
    `Indigenous Status` = c("Aboriginal and/or Torres Strait Islander",
                            "Non-Indigenous", "Total People"),
    `Victim Reports` = c(10, 90, 100),
    check.names = FALSE
  )
  r <- morie_vic_indigenous_ratio(d)
  expect_equal(nrow(r), 1L)
  expect_equal(r$indigenous, 10)
  expect_equal(r$non_indigenous, 90)      # NOT 190
  expect_equal(r$count_ratio, 10 / 90)
})

test_that("the Indigenous ratio uses one year at a time", {
  d <- data.frame(
    Year = c(2025, 2025, 2026, 2026),
    `Offence Division` = c("A", "A", "A", "A"),
    `Indigenous Status` = rep(c("Aboriginal and/or Torres Strait Islander",
                                "Non-Indigenous"), 2),
    `Victim Reports` = c(1, 9, 20, 80),
    check.names = FALSE
  )
  latest <- morie_vic_indigenous_ratio(d)
  expect_equal(latest$indigenous, 20)     # 2026 only, not 21
  expect_equal(latest$count_ratio, 20 / 80)
  earlier <- morie_vic_indigenous_ratio(d, year = 2025)
  expect_equal(earlier$indigenous, 1)
  expect_equal(earlier$count_ratio, 1 / 9)
})
