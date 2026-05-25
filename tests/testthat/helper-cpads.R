# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared CPADS-shaped synthetic-data fixtures for the coverage tests.
# testthat auto-sources helper-*.R before the test files, so any test
# file may call make_canonical_cpads() / make_raw_cpads().
#
# Marginals are anchored to published CPADS national prevalence:
# alcohol past-12m ~75% (female 78 / male 71); cannabis ~39%,
# age-graded (17-19: 32, 20-22: 45, 23-25: 42).

make_canonical_cpads <- function(n = 1200L, seed = 101L) {
  set.seed(seed)
  age_group <- sample(1:4, n, replace = TRUE)
  gender <- sample(1:3, n, replace = TRUE, prob = c(0.48, 0.49, 0.03))
  province_region <- sample(1:4, n, replace = TRUE)
  mental_health <- sample(1:5, n, replace = TRUE)
  physical_health <- sample(1:5, n, replace = TRUE)
  weight <- round(stats::rgamma(n, shape = 2.4, scale = 45), 1)
  cannabis_any_use <- stats::rbinom(
    n, 1L,
    c(0.32, 0.45, 0.42, 0.40)[age_group]
  )
  alcohol_past12m <- stats::rbinom(
    n, 1L, ifelse(gender == 2, 0.78, ifelse(gender == 1, 0.71, 0.75))
  )
  lp_hd <- -1.0 + 0.7 * cannabis_any_use + 0.15 * (mental_health >= 4) +
    0.10 * (gender == 2)
  heavy_drinking_30d <- stats::rbinom(n, 1L, 1 / (1 + exp(-lp_hd)))
  ebac_linear <- 0.04 + 0.03 * heavy_drinking_30d + 0.01 * cannabis_any_use +
    stats::rnorm(n, 0, 0.02)
  ebac_tot <- round(pmax(0, pmin(0.35, ebac_linear)), 3)
  ebac_legal <- as.integer(ebac_tot > 0.08)
  observed <- alcohol_past12m == 1L & stats::runif(n) < 0.70
  ebac_tot[!observed] <- NA_real_
  ebac_legal[!observed] <- NA_integer_
  data.frame(
    weight = weight, alcohol_past12m = alcohol_past12m,
    heavy_drinking_30d = heavy_drinking_30d, ebac_tot = ebac_tot,
    ebac_legal = ebac_legal, cannabis_any_use = cannabis_any_use,
    age_group = age_group, gender = gender,
    province_region = province_region, mental_health = mental_health,
    physical_health = physical_health,
    alc06 = sample(c(1:6, 97, 98, 99), n,
      replace = TRUE,
      prob = c(rep(0.155, 6), 0.03, 0.02, 0.05)
    ),
    stringsAsFactors = FALSE
  )
}

make_raw_cpads <- function(n = 900L, seed = 202L) {
  set.seed(seed)
  cannabis <- stats::rbinom(n, 1L, 0.39)
  hd <- stats::rbinom(n, 1L, 0.30)
  ebac <- round(pmax(0, pmin(0.35, 0.04 + 0.03 * hd +
    stats::rnorm(n, 0, 0.02))), 3)
  data.frame(
    wtpumf = round(stats::rgamma(n, 2.4, scale = 45), 1),
    alc05 = sample(c(1L, 2L), n, replace = TRUE, prob = c(0.75, 0.25)),
    alc12_30d_prev_total = sample(c(0L, 1L), n, replace = TRUE),
    alc12_30d_prev = sample(c(0L, 1L), n, replace = TRUE),
    can05 = ifelse(cannabis == 1L, 1L, 2L),
    age_groups = sample(c(1:4, 98L), n,
      replace = TRUE,
      prob = c(0.27, 0.34, 0.23, 0.14, 0.02)
    ),
    dvdemq01 = sample(c(1L, 2L, 3L, 99L), n,
      replace = TRUE,
      prob = c(0.48, 0.47, 0.03, 0.02)
    ),
    region = sample(c(1:4, 98L), n,
      replace = TRUE,
      prob = c(0.11, 0.23, 0.39, 0.25, 0.02)
    ),
    hwbq01 = sample(c(1:5, 98L), n,
      replace = TRUE,
      prob = c(0.14, 0.25, 0.33, 0.18, 0.08, 0.02)
    ),
    hwbq02 = sample(c(1:5, 99L), n,
      replace = TRUE,
      prob = c(0.10, 0.22, 0.34, 0.21, 0.11, 0.02)
    ),
    ebac_tot = ebac, ebac_legal = as.integer(ebac > 0.08),
    alc06 = sample(1:6, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
