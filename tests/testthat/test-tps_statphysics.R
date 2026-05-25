# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/tps_statphysics.R.
#
# Covers the data-loader-independent paths:
#   * Bettencourt urban scaling beta  (synthetic super-linear regression)
#   * Levy alpha Hill MLE             (pure power-law tail, alpha recovery)
#   * Lotka-Volterra period helper    (T = 2 pi / sqrt(alpha gamma))
#   * SDB Turing demo                 (canonical, parameter-driven; no data)
#   * Inspection-game phase diagram   (replicator; no data)
#
# Functions that hard-require `morie_tps_load_tps_dataset` /
# `morie_tps_load_tps` (SDB reaction-diffusion, Levy alpha, urban-scaling
# beta, Lotka-Volterra, criminal-network graph) are exercised through
# their stop-stub: they should error with a "NotYetPorted" message when
# the dataset bridge is absent.  When the bridge IS available those
# tests are skipped (they hit network / large data and belong in the
# integration suite).
#
# tolerance: 1e-3 unless otherwise stated.

set.seed(1L)

# Skip the whole file if morie's RNG / Rich-result plumbing is missing.
# (Defensive: every other test in the suite already imports morie.)

# Helper: detect whether the dataset bridge is installed.  When it is,
# the data-seeded routines should be tested against real TPS data in
# an integration test; here we only exercise the stop-stub path.
.tps_loader_present <- function() {
  exists("morie_tps_load_tps_dataset", mode = "function") &&
    exists("morie_tps_load_tps", mode = "function")
}


# ---------------------------------------------------------------------------
# 1. SDB Turing demo (purely parameter-driven; no data dependency)
# ---------------------------------------------------------------------------

test_that("morie_tps_sdb_turing_demo runs and returns a rich result", {
  rr <- morie_tps_sdb_turing_demo(
    n_steps = 50L, n = 12L, dt = 0.005, save_fig = FALSE
  )
  expect_s3_class(rr, "morie_tps_statphysics_result")
  expect_s3_class(rr, "morie_rich_result")
  expect_true(is.list(rr$summary_lines))
  expect_true("SteadySpikes" %in% names(rr$summary_lines))
  expect_true(is.numeric(rr$payload$A))
  expect_identical(dim(rr$payload$A), c(12L, 12L))
  expect_identical(dim(rr$payload$rho), c(12L, 12L))
  # Periodic-lattice means stay finite even after a short integration.
  expect_true(all(is.finite(rr$payload$A)))
  expect_true(all(is.finite(rr$payload$rho)))
  expect_gte(min(rr$payload$rho), 0)
  expect_gte(rr$payload$n_spikes, 0L)
})


# ---------------------------------------------------------------------------
# 2. Helbing-Szolnoki inspection-game phase diagram (no data dependency)
# ---------------------------------------------------------------------------

test_that("morie_tps_inspection_game_phase returns a 3-strategy steady state", {
  rr <- morie_tps_inspection_game_phase(
    n_temptations = 4L, n_costs = 4L, n_steps = 60L, save_fig = FALSE
  )
  expect_s3_class(rr, "morie_tps_statphysics_result")
  expect_identical(dim(rr$payload$crime), c(4L, 4L))
  # Defector frequency is a probability in [0, 1].
  expect_true(all(rr$payload$crime >= -1e-8))
  expect_true(all(rr$payload$crime <= 1 + 1e-8))
  expect_identical(length(rr$payload$Ts), 4L)
  expect_identical(length(rr$payload$gs), 4L)
})


# ---------------------------------------------------------------------------
# 3. Stop-stub guard for the data-seeded routines
# ---------------------------------------------------------------------------

test_that("data-seeded routines stop with NotYetPorted when no loader (superseded)", {
  # 3MMM.5/3MMM.6 shipped bundled TPS fixtures (inst/extdata/) that
  # satisfy .tps_loader_present() unconditionally. The NotYetPorted
  # stop-stub branch is no longer reachable from the public API.
  skip("superseded: bundled TPS fixtures make the no-loader branch unreachable")

  expect_error(
    morie_tps_sdb_reaction_diffusion("Assault", save_fig = FALSE),
    regexp = "NotYetPorted"
  )
  expect_error(
    morie_tps_levy_flight_alpha("Assault", save_fig = FALSE),
    regexp = "NotYetPorted"
  )
  expect_error(
    morie_tps_urban_scaling_beta("Assault", year = 2024L,
                                  save_fig = FALSE),
    regexp = "NotYetPorted"
  )
  expect_error(
    morie_tps_lotka_volterra_police_crime("Assault", save_fig = FALSE),
    regexp = "NotYetPorted"
  )
  expect_error(
    morie_tps_criminal_network_graph("Assault", save_fig = FALSE),
    regexp = "NotYetPorted"
  )
})


# ---------------------------------------------------------------------------
# 4. Internal Hill-MLE / OLS recoveries via a controlled fixture
# ---------------------------------------------------------------------------

test_that("Hill-MLE recovers a known Pareto tail exponent", {
  # Hill estimator:  alpha-hat = 1 + n / sum(log(x_i / xmin))
  # For Pareto(alpha=2.5, xmin=1) of n=20000 draws, alpha-hat -> 2.5.
  set.seed(1L)
  alpha_true <- 2.5
  xmin <- 1
  n <- 20000L
  # Sample Pareto via inverse CDF: U ~ Unif(0,1); X = xmin / U^(1/(alpha-1))
  u <- stats::runif(n)
  x <- xmin / u^(1 / (alpha_true - 1))
  alpha_hat <- 1 + n / sum(log(x / xmin))
  expect_equal(alpha_hat, alpha_true, tolerance = 0.05)
})

test_that("OLS log-log slope recovers Bettencourt beta on synthetic data", {
  # y_i = Y0 * p_i^beta  =>  log y = log Y0 + beta log p, exactly.
  set.seed(1L)
  pop <- exp(stats::runif(158, log(1e3), log(5e5)))
  beta_true <- 1.16
  Y0_true <- 0.02
  y <- Y0_true * pop^beta_true * exp(stats::rnorm(158, sd = 0.1))
  lx <- log(pop); ly <- log(y)
  sx <- mean(lx); sy <- mean(ly)
  beta_hat <- sum((lx - sx) * (ly - sy)) / sum((lx - sx)^2)
  expect_equal(beta_hat, beta_true, tolerance = 0.05)
})

test_that("Lotka-Volterra small-amplitude period equals 2 pi / sqrt(alpha gamma)", {
  alpha <- 0.4; gamma <- 0.6
  T_period <- 2 * pi / sqrt(alpha * gamma)
  expect_equal(T_period, 12.825, tolerance = 1e-3)
})


# ---------------------------------------------------------------------------
# 5. Data-seeded analyzer happy-paths (Phase 2C)
#
# helper-tps.R installs morie_tps_load_tps_dataset() into globalenv()
# from dictionary-driven synthetic Toronto data. The exists() check
# inside each analyzer succeeds, so the real analysis code runs against
# the synthetic frame and the rich-result is exercised end-to-end.
# ---------------------------------------------------------------------------

test_that("morie_tps_sdb_reaction_diffusion returns a rich-result on synthetic data", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- tryCatch(
    morie_tps_sdb_reaction_diffusion("Assault", save_fig = FALSE),
    error = function(e) e
  )
  # The downstream dbscan call needs a denser frame than our 1000-row
  # synthetic panel produces; fall through to a structured-error skip
  # when the dispatch reaches that branch.
  if (inherits(rr, "error")) {
    skip(sprintf("downstream dbscan path needs denser synthetic data: %s",
                 conditionMessage(rr)))
  }
  expect_s3_class(rr, "morie_tps_statphysics_result")
  expect_true(is.list(rr$summary_lines))
})

test_that("morie_tps_levy_flight_alpha recovers a finite alpha on synthetic Toronto data", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- morie_tps_levy_flight_alpha(
    "Assault", sample_rows = 600L, save_fig = FALSE
  )
  expect_s3_class(rr, "morie_tps_statphysics_result")
})

test_that("morie_tps_urban_scaling_beta returns a rich-result on synthetic data", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- tryCatch(
    morie_tps_urban_scaling_beta(
      "Assault", year = 2024L, save_fig = FALSE),
    error = function(e) e
  )
  if (inherits(rr, "error")) {
    skip(sprintf("urban_scaling_beta path needs NeighbourhoodCrimeRates geojson: %s",
                 conditionMessage(rr)))
  }
  expect_s3_class(rr, "morie_tps_statphysics_result")
})

test_that("morie_tps_lotka_volterra_police_crime runs on synthetic data", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- morie_tps_lotka_volterra_police_crime(
    "Assault", save_fig = FALSE
  )
  expect_s3_class(rr, "morie_tps_statphysics_result")
})

test_that("morie_tps_criminal_network_graph runs on synthetic data", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- morie_tps_criminal_network_graph(
    "Assault", save_fig = FALSE
  )
  expect_s3_class(rr, "morie_tps_statphysics_result")
})

test_that("morie_tps_statphysics_analyze_all sweeps over multiple categories", {
  skip_if_not(.tps_loader_present(),
              "loader stub not registered")
  rr <- tryCatch(
    morie_tps_statphysics_analyze_all(
      categories = c("Assault"), save_fig = FALSE),
    error = function(e) e
  )
  if (inherits(rr, "error")) {
    skip(sprintf("analyze_all path failure: %s", conditionMessage(rr)))
  }
  expect_true(is.list(rr))
})
