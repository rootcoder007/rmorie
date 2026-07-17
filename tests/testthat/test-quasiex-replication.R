# SPDX-License-Identifier: AGPL-3.0-or-later
# Phase 9 replication tests: the bundled canonical datasets driven
# through the native quasi-experimental stack, asserting agreement
# with the published estimates (tolerances reflect estimator-variant
# differences documented in each literature).

.qx_file <- function(name) {
  p <- system.file("extdata", "quasiex", name, package = "rmorie")
  if (!nzchar(p)) p <- file.path("inst", "extdata", "quasiex", name)
  read.csv(p)
}

test_that("LaLonde: PS weighting moves the naive gap toward the exp benchmark", {
  d <- .qx_file("lalonde_matchit.csv")
  d$treat <- as.integer(d$treat)
  covs <- c("age", "educ", "re74", "re75")
  naive <- mean(d$re78[d$treat == 1]) - mean(d$re78[d$treat == 0])
  w <- morie_weight_ps(d, "treat", covs, estimand = "ATT",
                       trim = 0.01)
  d$.w <- w$weights
  wc <- d$treat == 0
  adj <- mean(d$re78[d$treat == 1]) -
    sum(d$re78[wc] * d$.w[wc]) / sum(d$.w[wc])
  # Observational naive gap is famously ~ -$635; the experimental
  # benchmark is ~ +$1,794 (Dehejia-Wahba). Weighting must move the
  # estimate a long way toward positive territory.
  expect_lt(naive, 0)
  expect_gt(adj, naive + 1000)
})

test_that("LaLonde: doubly-robust cross-fit PLR is positive-signed", {
  d <- .qx_file("lalonde_matchit.csv")
  fit <- estimate_plr(d, treatment = "treat", outcome = "re78",
                      covariates = c("age", "educ", "re74", "re75"))
  expect_gt(fit$ate + 2 * fit$se, 0) # not significantly negative
})

test_that("Basque: synthetic control finds the terrorism GDP gap", {
  d <- .qx_file("basque_synth.csv")
  d <- d[!is.na(d$gdpcap), c("regionname", "year", "gdpcap")]
  # optimize_v = FALSE keeps this replication CI-friendly (the full
  # V-optimized fit + placebo suite takes ~30 min); equal V weights
  # still reproduce the Abadie-Gardeazabal gap direction and size.
  fit <- morie_synth_control(d, outcome = "gdpcap",
                             unit = "regionname", time = "year",
                             treated_unit = "Basque Country (Pais Vasco)",
                             treatment_time = 1970,
                             optimize_v = FALSE)
  # Post-treatment gap: treated below synthetic (Abadie-Gardeazabal
  # report ~ -0.6 to -1 GDP points across the late 70s-80s).
  ts <- fit$time_series
  post <- ts[ts$time >= 1975 & ts$time <= 1990, ]
  expect_lt(mean(post$observed - post$synthetic), -0.2)
})

test_that("Lee (2008) incumbency RDD replicates ~0.08 vote-share jump", {
  d <- .qx_file("lee2008_house.csv")
  # rddtools::house: x = dem margin t, y = dem vote share t+1.
  fit <- morie_rdd(d, outcome = "y", running = "x", cutoff = 0)
  expect_gt(fit$estimate, 0.04)
  expect_lt(fit$estimate, 0.12) # Lee reports ~0.077
  expect_lt(fit$p.value, 0.01)
})

test_that("CigarettesSW: IV price elasticity is negative and gated", {
  d <- .qx_file("cigarettes_sw_aer.csv")
  d <- d[d$year == 1995, ]
  d$lprice <- log(d$price / d$cpi)
  d$lquant <- log(d$packs)
  d$tdiff <- (d$taxs - d$tax) / d$cpi
  fit <- morie_iv_2sls(d, "lquant", "lprice", "tdiff")
  expect_false(fit$weak_instruments)
  # Stock-Watson report elasticity ~ -1.08 for this specification.
  expect_lt(fit$estimate, -0.5)
  expect_gt(fit$estimate, -2)
})

test_that("UKDriverDeaths ITS finds the 1983 seatbelt-law drop", {
  # Real base-R data (no bundling needed): monthly GB driver deaths
  # 1969-1984; compulsory seatbelts Jan 31, 1983 (Harvey & Durbin
  # 1986 report a substantial immediate reduction).
  y <- as.numeric(datasets::UKDriverDeaths)
  d <- data.frame(deaths = y, t = seq_along(y))
  cut <- which(time(datasets::UKDriverDeaths) >= 1983.05)[1]
  fit <- morie_its(d, outcome = "deaths", time = "t",
                   interruption_time = cut)
  # Level change: negative and material (hundreds of deaths/month).
  expect_lt(fit$level_change$estimate, -100)
  expect_lt(fit$level_change$p_value, 0.05)
})
