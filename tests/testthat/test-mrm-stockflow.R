# MRM stock and flow across the OTIS strata.
#
# The anchors are of two kinds. The arithmetic is checked against
# Lakner's published worked examples, and the multilevel behaviour is
# checked on data built so that the strata DISAGREE, because a
# reconciliation that only ever sees agreeing strata proves nothing.

test_that("the measures match Lakner's worked example", {
  # p.15: 13,500 detention days over a year is an average daily
  # population of 36.986. Built here as one row per person.
  person <- data.frame(
    EndFiscalYear = rep(2023, 3),
    UniqueIndividual_ID = c("a", "b", "c"),
    TotalAggregatedDays_Segregation = c(4500, 4500, 4500))
  out <- mrm_otis_stock_flow(person)
  expect_equal(out$stock_flow$days, 13500)
  expect_equal(out$stock_flow$adp, 13500 / 365)
  expect_equal(round(out$stock_flow$adp, 8), 36.98630137)
  # and the stay is days per person
  expect_equal(out$stock_flow$alos, 4500)
})

test_that("the strata are reconciled, and disagreement is reported", {
  # Three people at the person stratum; the placement stratum knows only
  # two of them, and the aggregate stratum states four. Nothing agrees,
  # which is what this has to notice.
  person <- data.frame(
    EndFiscalYear = rep(2024, 3),
    UniqueIndividual_ID = c("a", "b", "c"),
    TotalAggregatedDays_Segregation = c(10, 20, 30))
  placement <- data.frame(
    EndFiscalYear = rep(2024, 3),
    UniqueIndividual_ID = c("a", "a", "b"),
    NumberConsecutiveDays_Segregation = c(5, 4, 20),
    Number_Of_Placements = c(1, 1, 1))
  totals <- data.frame(EndFiscalYear = 2024,
                       NumberIndividuals_Segregation = 4)
  out <- mrm_otis_stock_flow(person, placement, totals)
  r <- out$reconciliation
  expect_identical(r$person_stratum_people, 3)
  expect_identical(r$placement_stratum_people, 2)
  expect_identical(r$aggregate_stratum_total, 4)
  expect_false(r$strata_agree)
  expect_false(out$strata_agree)
  # summed spell lengths are 29 against 60 person-days
  expect_equal(r$consecutive_days, 29)
  expect_equal(r$consecutive_over_person_days, 29 / 60)
  expect_lt(r$consecutive_over_person_days, 1)
})

test_that("agreeing strata are reported as agreeing", {
  person <- data.frame(
    EndFiscalYear = rep(2024, 2),
    UniqueIndividual_ID = c("a", "b"),
    TotalAggregatedDays_Segregation = c(10, 20))
  placement <- data.frame(
    EndFiscalYear = rep(2024, 2),
    UniqueIndividual_ID = c("a", "b"),
    NumberConsecutiveDays_Segregation = c(10, 20),
    Number_Of_Placements = c(1, 1))
  totals <- data.frame(EndFiscalYear = 2024,
                       NumberIndividuals_Segregation = 2)
  out <- mrm_otis_stock_flow(person, placement, totals)
  expect_true(out$strata_agree)
  # with one spell each, spell length IS the person's days
  expect_equal(out$reconciliation$consecutive_over_person_days, 1)
  expect_equal(out$reconciliation$placements_per_person, 1)
})

test_that("the decomposition is exact, and opposite signs are possible", {
  # Fewer people held longer: the flow falls while the stock rises. The
  # published Ontario segregation figures, as one row per person would
  # give them.
  person <- rbind(
    data.frame(EndFiscalYear = 2023,
               UniqueIndividual_ID = paste0("a", seq_len(12647)),
               TotalAggregatedDays_Segregation = 115674 / 12647),
    data.frame(EndFiscalYear = 2025,
               UniqueIndividual_ID = paste0("b", seq_len(9608)),
               TotalAggregatedDays_Segregation = 126121 / 9608))
  out <- mrm_otis_stock_flow(person,
                             exposure = c(15495050, 16256538))
  sf <- out$stock_flow
  expect_true(out$decomposition_exact)
  expect_equal(round(sf$days, 0), c(115674, 126121))
  expect_equal(round(sf$adp[2], 1), round(126121 / 365, 1))
  # people down, stay up, days up
  expect_lt(sf$people_change[2], 0)
  expect_gt(sf$alos_change[2], 0)
  expect_gt(sf$days_change[2], 0)
  # and the two rates disagree on the sign, which is the finding
  expect_lt(sf$flow_rate_change[2], 0)
  expect_gt(sf$stock_rate_change[2], 0)
})

test_that("the period length is a parameter, not an assumption", {
  person <- data.frame(
    EndFiscalYear = rep(2024, 2),
    UniqueIndividual_ID = c("a", "b"),
    TotalAggregatedDays_Segregation = c(15, 15))
  expect_equal(mrm_otis_stock_flow(person, t = 30)$stock_flow$adp, 1)
  expect_equal(mrm_otis_stock_flow(person, t = 366)$stock_flow$adp, 30 / 366)
})

test_that("missing columns are named rather than guessed at", {
  person <- data.frame(EndFiscalYear = 2024, UniqueIndividual_ID = "a",
                       TotalAggregatedDays_Segregation = 5)
  expect_error(mrm_otis_stock_flow(person[, 1:2]), "missing column")
  expect_error(mrm_otis_stock_flow(person, days_col = "nope"),
               "missing column")
  expect_error(mrm_otis_stock_flow("not a frame"), "must be a data frame")
  expect_error(
    mrm_otis_stock_flow(person, placements = data.frame(x = 1)),
    "missing column")
})

test_that("column names are configurable for another release's schema", {
  # A different jurisdiction naming the same quantities differently.
  d <- data.frame(fy = c(2024, 2024), pid = c("x", "y"), served = c(7, 14))
  out <- mrm_otis_stock_flow(d, year_col = "fy", id_col = "pid",
                             days_col = "served", t = 7)
  expect_equal(out$stock_flow$days, 21)
  expect_equal(out$stock_flow$adp, 3)
  expect_equal(out$stock_flow$alos, 10.5)
})
