# Environmental-health composition: pollution exposure -> disease burden.
# Mirrors morie.envhealth (Python) function for function; every formula cites
# the paper it implements, and the parity test compares both arms on fixed
# inputs. Scalar inputs give scalar fields; a vector of exposures gives the
# mean RR / log-RR with the per-unit vector under extra$rr_per_unit, exactly
# as the Python arm does.

.envhealth_crf <- function(rr, log_rr, z, scalar, reference_conc, pollutant,
                           citation, extra) {
  structure(list(
    rr = if (scalar) rr[[1L]] else mean(rr),
    log_rr = if (scalar) log_rr[[1L]] else mean(log_rr),
    reference_conc = reference_conc,
    exposure_conc = if (scalar) z[[1L]] else mean(z),
    pollutant = pollutant,
    citation = citation,
    extra = c(extra, list(rr_per_unit = if (scalar) NULL else rr))
  ), class = "morie_envhealth_crf")
}

#' Concentration-response functions for PM2.5 and NO2
#'
#' \code{morie_envhealth_crf_pm25()} is log-linear for all-cause mortality with
#' the pooled cohort estimate of the WHO 2021 guideline review (Chen and Hoek
#' 2020: RR 1.08, 95\% CI 1.06-1.09, per 10 micrograms per cubic metre),
#' \deqn{RR(z) = \exp(\ln(1.08) (z - z_{cf}) / 10)}
#' for \eqn{z > z_{cf}} and 1 otherwise; for the cause-specific outcomes (IHD,
#' stroke) it is the Integrated Exposure-Response curve of Burnett et al.
#' (2014, Eq. 1),
#' \deqn{RR(z) = 1 + \alpha (1 - e^{-\gamma (z - z_{cf})^{\delta}}),}
#' with the GBD 2013 triples; the IER was fit per cause and has no all-cause
#' form. \code{morie_envhealth_crf_no2()} is the log-linear function of the
#' WHO 2021 review (Huangfu and Atkinson 2020: RR 1.02, 95\% CI 1.01-1.04, per
#' 10 micrograms per cubic metre for all-cause mortality),
#' \deqn{RR(z) = \exp(\beta (z - z_{cf}) / 10).}
#'
#' @param exposure Ambient concentration in micrograms per cubic metre
#'   (scalar or vector).
#' @param outcome PM2.5: \code{"all_cause_mortality"}, \code{"ihd"} or
#'   \code{"stroke"}. NO2: \code{"all_cause_mortality"},
#'   \code{"respiratory"} or \code{"childhood_asthma"}.
#' @param reference_conc Counterfactual concentration below which no excess
#'   risk is assumed (PM2.5 default 5.8, NO2 default 10).
#' @param beta_per_10 Optional log-RR per 10 units overriding the NO2 outcome
#'   table.
#' @return A list of class \code{morie_envhealth_crf} with \code{rr},
#'   \code{log_rr}, \code{reference_conc}, \code{exposure_conc},
#'   \code{pollutant}, \code{citation} and \code{extra}.
#' @references Chen, J. and Hoek, G. (2020). Environment International, 143,
#'   105974. Huangfu, P. and Atkinson, R. (2020). Environment International,
#'   144, 105998. Burnett, R. T. et al. (2014). Environmental Health
#'   Perspectives, 122(4), 397-403. WHO (2021). Global Air Quality Guidelines.
#' @examples
#' morie_envhealth_crf_pm25(12)$rr
#' morie_envhealth_crf_no2(25, outcome = "respiratory")$rr
#' @export
morie_envhealth_crf_pm25 <- function(exposure, outcome = "all_cause_mortality",
                                     reference_conc = 5.8) {
  # all-cause: RR 1.08 (95% CI 1.06-1.09) per 10 ug/m3, Chen and Hoek (2020), the WHO 2021 review
  loglinear <- c(all_cause_mortality = log(1.08))
  # GBD 2013 cause-specific IER triples (Burnett et al. 2014)
  ier <- list(ihd = c(1.91, 0.14, 0.49),
              stroke = c(1.46, 0.13, 0.61))
  if (!outcome %in% c(names(loglinear), names(ier))) {
    stop(sprintf("Unknown outcome '%s'. Available: %s", outcome,
                 paste(c(names(loglinear), names(ier)), collapse = ", ")), call. = FALSE)
  }
  z <- as.numeric(exposure)
  scalar <- length(z) == 1L
  excess <- pmax(z - reference_conc, 0)
  if (outcome %in% names(loglinear)) {
    beta <- loglinear[[outcome]]
    log_rr <- beta * excess / 10
    return(.envhealth_crf(exp(log_rr), log_rr, z, scalar, reference_conc, "PM2.5",
                          "Chen & Hoek (2020) Environ Int 143:105974; WHO (2021) Global AQ Guidelines",
                          list(beta_per_10 = beta, form = "log-linear", outcome = outcome)))
  }
  p <- ier[[outcome]]
  rr <- 1 + p[1L] * (1 - exp(-p[2L] * excess^p[3L]))
  .envhealth_crf(rr, log(rr), z, scalar, reference_conc, "PM2.5",
                 "Burnett et al. (2014) EHP 122(4):397-403",
                 list(alpha = p[1L], gamma = p[2L], delta = p[3L], form = "IER", outcome = outcome))
}

#' @rdname morie_envhealth_crf_pm25
#' @export
morie_envhealth_crf_no2 <- function(exposure, outcome = "all_cause_mortality",
                                    reference_conc = 10, beta_per_10 = NULL) {
  # all-cause: RR 1.02 (95% CI 1.01-1.04) per 10 ug/m3, Huangfu and Atkinson (2020), the WHO 2021 review
  betas <- c(all_cause_mortality = log(1.02), respiratory = 0.029, childhood_asthma = 0.039)
  if (is.null(beta_per_10)) {
    if (!outcome %in% names(betas)) {
      stop(sprintf("Unknown outcome '%s'. Available: %s or pass beta_per_10 explicitly.",
                   outcome, paste(names(betas), collapse = ", ")), call. = FALSE)
    }
    beta <- betas[[outcome]]
  } else {
    beta <- as.numeric(beta_per_10)
  }
  z <- as.numeric(exposure)
  scalar <- length(z) == 1L
  log_rr <- beta * pmax(z - reference_conc, 0) / 10
  .envhealth_crf(exp(log_rr), log_rr, z, scalar, reference_conc, "NO2",
                 "Huangfu & Atkinson (2020) Environ Int 144:105998; WHO (2021) Global AQ Guidelines",
                 list(beta_per_10 = beta, outcome = outcome))
}

#' Population attributable fraction
#'
#' Levin's formula (Rothman, Greenland and Lash 2008, chapter 5):
#' \deqn{PAF = p (RR - 1) / (1 + p (RR - 1)).}
#' @param rr Relative risk at the observed exposure level.
#' @param exposure_prevalence Proportion of the population exposed, in
#'   \code{[0, 1]}.
#' @return The fraction of cases that would be avoided if the exposure were
#'   removed.
#' @examples
#' morie_envhealth_attributable_fraction(1.5, 0.4)
#' @export
morie_envhealth_attributable_fraction <- function(rr, exposure_prevalence) {
  rr <- as.numeric(rr)
  p <- as.numeric(exposure_prevalence)
  if (is.na(p) || p < 0 || p > 1) {
    stop(sprintf("exposure_prevalence must be in [0,1]; got %s", format(p)), call. = FALSE)
  }
  num <- p * (rr - 1)
  den <- 1 + p * (rr - 1)
  if (den == 0) return(0)
  num / den
}

#' Deaths avoided under a counterfactual exposure reduction
#'
#' BenMAP-CE health-impact function (US EPA 2018; Anenberg et al. 2010):
#' \deqn{\Delta Y = y_0 N (1 - e^{-\beta \Delta x}).}
#' @param exposure_delta Counterfactual reduction in exposure (positive means
#'   the exposure goes down).
#' @param population At-risk population.
#' @param baseline_rate Baseline rate in cases per person-year.
#' @param beta_per_unit Log-RR per unit of exposure (per unit, not per 10).
#' @return Expected avoided cases.
#' @examples
#' morie_envhealth_mortality_displaced(10, 1e6, 0.008, 0.0039)
#' @export
morie_envhealth_mortality_displaced <- function(exposure_delta, population,
                                                baseline_rate, beta_per_unit) {
  if (population < 0) stop("population must be non-negative", call. = FALSE)
  if (baseline_rate < 0) stop("baseline_rate must be non-negative", call. = FALSE)
  baseline_rate * population * (1 - exp(-beta_per_unit * exposure_delta))
}

#' End-to-end pollution burden
#'
#' Chains the concentration-response function, the attributable fraction and
#' the baseline case count into the annual attributable cases (GBD 2019 Risk
#' Factors Collaborators 2020).
#' @param exposure_mean Population-mean exposure.
#' @param exposure_prevalence Proportion of the population at that level
#'   (1 for ambient air).
#' @param baseline_rate Cases per person-year in the unexposed scenario.
#' @param population At-risk population.
#' @param pollutant \code{"PM2.5"} or \code{"NO2"}.
#' @param outcome Outcome key passed to the concentration-response function.
#' @param reference_conc Counterfactual concentration; defaults to the
#'   pollutant's WHO guideline.
#' @return A list of class \code{morie_envhealth_burden} with \code{paf},
#'   \code{attributable_cases}, \code{baseline_cases}, \code{population},
#'   \code{baseline_rate}, \code{exposure_mean}, \code{reference_conc},
#'   \code{pollutant}, \code{citation} and \code{extra}.
#' @examples
#' morie_envhealth_burden(25, 1, 0.008, 1e6, pollutant = "NO2")$attributable_cases
#' @export
morie_envhealth_burden <- function(exposure_mean, exposure_prevalence, baseline_rate,
                                   population, pollutant = "NO2",
                                   outcome = "all_cause_mortality",
                                   reference_conc = NULL) {
  crf <- if (identical(pollutant, "PM2.5")) {
    morie_envhealth_crf_pm25(exposure_mean, outcome = outcome,
                             reference_conc = reference_conc %||% 5.8)
  } else if (identical(pollutant, "NO2")) {
    morie_envhealth_crf_no2(exposure_mean, outcome = outcome,
                            reference_conc = reference_conc %||% 10)
  } else {
    stop(sprintf("Unknown pollutant '%s'. Use 'PM2.5' or 'NO2'.", pollutant), call. = FALSE)
  }
  paf <- morie_envhealth_attributable_fraction(crf$rr, exposure_prevalence)
  baseline_cases <- baseline_rate * population
  structure(list(
    paf = paf,
    attributable_cases = paf * baseline_cases,
    baseline_cases = baseline_cases,
    population = as.integer(population),
    baseline_rate = baseline_rate,
    exposure_mean = as.numeric(exposure_mean),
    reference_conc = crf$reference_conc,
    pollutant = pollutant,
    citation = paste0(crf$citation, "; Rothman et al. (2008) \u00a75"),
    extra = list(rr = crf$rr, log_rr = crf$log_rr, outcome = outcome)
  ), class = "morie_envhealth_burden")
}

#' Exposure-response double machine learning with bootstrap sensitivity
#'
#' Fits the partially linear model of Chernozhukov et al. (2018, section
#' 4.2) with \code{\link{morie_estimate_double_ml}} (continuous exposure as
#' the treatment), then re-fits it on \code{n_bootstrap} non-parametric
#' resamples for a percentile interval (Efron and Tibshirani 1993).
#' @param data A data frame.
#' @param outcome,exposure,confounders Column names.
#' @param n_bootstrap Number of resamples (default 100).
#' @param random_state Seed for the resampling.
#' @return A list with \code{ate}, \code{se_analytic}, \code{se_bootstrap},
#'   \code{ci_lower_bs}, \code{ci_upper_bs}, \code{n_bootstrap} and
#'   \code{method}.
#' @examples
#' \donttest{
#' set.seed(1)
#' d <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
#' d$exposure <- 20 + 2 * d$x1 + rnorm(200)
#' d$asthma <- 5 + 0.3 * d$exposure + d$x2 + rnorm(200)
#' morie_envhealth_sensitivity(d, outcome = "asthma", exposure = "exposure",
#'                             confounders = c("x1", "x2"), n_bootstrap = 20)$ate
#' }
#' @export
morie_envhealth_sensitivity <- function(data, outcome, exposure, confounders,
                                        n_bootstrap = 100L, random_state = 42L) {
  base <- morie_estimate_double_ml(data, outcome = outcome, treatment = exposure,
                                   covariates = confounders)
  ate <- as.numeric(base$ate)
  se_analytic <- as.numeric(base$se)
  .rmorie_local_seed(random_state)
  n <- nrow(data)
  boot <- numeric(0)
  for (b in seq_len(n_bootstrap)) {
    idx <- sample.int(n, n, replace = TRUE)
    r <- tryCatch(morie_estimate_double_ml(data[idx, , drop = FALSE], outcome = outcome,
                                           treatment = exposure, covariates = confounders),
                  error = function(e) NULL)
    if (!is.null(r)) boot <- c(boot, as.numeric(r$ate))
  }
  if (length(boot) < 10L) {
    stop(sprintf("Only %d successful bootstrap fits; need >=10. Inspect the PLR convergence on your resamples.",
                 length(boot)), call. = FALSE)
  }
  list(
    ate = ate,
    se_analytic = se_analytic,
    se_bootstrap = stats::sd(boot),
    ci_lower_bs = unname(stats::quantile(boot, 0.025, type = 7)),
    ci_upper_bs = unname(stats::quantile(boot, 0.975, type = 7)),
    n_bootstrap = length(boot),
    method = "PLR (DoubleML) + percentile bootstrap"
  )
}

#' Concentration index of exposure by income
#'
#' Wagstaff, Paci and van Doorslaer (1991):
#' \deqn{CI = (2 / \mu) \, \mathrm{cov}(h_i, R_i)}
#' with \eqn{R_i} the fractional income rank \eqn{(rank_i - 0.5) / n}
#' (ties kept in input order, as the Python arm's stable sort does) and the
#' population covariance. Negative values mean lower-income units bear more
#' exposure.
#' @param data A data frame.
#' @param exposure,income Column names.
#' @return A list of class \code{morie_envhealth_equity} with
#'   \code{concentration_index}, \code{interpretation}, \code{n_quintiles},
#'   \code{exposure_mean}, \code{citation} and \code{extra}.
#' @examples
#' d <- data.frame(exposure = c(30, 25, 20, 15, 10), income = 1:5)
#' morie_envhealth_equity(d, "exposure", "income")$concentration_index
#' @export
morie_envhealth_equity <- function(data, exposure, income) {
  df <- data[, c(exposure, income), drop = FALSE]
  df <- df[stats::complete.cases(df), , drop = FALSE]
  n <- nrow(df)
  if (n < 2L) stop("Need at least 2 rows to compute concentration index.", call. = FALSE)
  h <- as.numeric(df[[exposure]])
  inc <- as.numeric(df[[income]])
  mu <- mean(h)
  if (mu == 0) stop("Mean exposure is zero; concentration index undefined.", call. = FALSE)
  ord <- order(inc, method = "radix")
  ranks <- numeric(n)
  ranks[ord] <- seq_len(n)
  R <- (ranks - 0.5) / n
  cov_hR <- sum((h - mean(h)) * (R - mean(R))) / n
  ci <- 2 * cov_hR / mu
  interp <- if (ci < -0.01) {
    sprintf("CI = %.4f. Pro-poor exposure burden: lower-income individuals bear disproportionately higher pollution.", ci)
  } else if (ci > 0.01) {
    sprintf("CI = %.4f. Pro-rich exposure burden: higher-income individuals bear disproportionately higher pollution.", ci)
  } else {
    sprintf("CI = %.4f. Exposure distributed approximately evenly across income.", ci)
  }
  structure(list(
    concentration_index = ci,
    interpretation = interp,
    n_quintiles = 5L,
    exposure_mean = mu,
    citation = "Wagstaff, Paci & van Doorslaer (1991) Soc Sci Med",
    extra = list(n = n, cov_h_R = cov_hR)
  ), class = "morie_envhealth_equity")
}

#' Per-area pollution burden
#'
#' Applies \code{\link{morie_envhealth_burden}} to every row of a table with
#' one row per area (forward sortation area, tract, ...) and returns the rows
#' sorted by attributable cases, worst first.
#' @param fsa_table A data frame with one row per area.
#' @param fsa_col,exposure_col,population_col,baseline_rate_col Column names.
#' @param pollutant,outcome Passed to \code{\link{morie_envhealth_burden}}.
#' @return The input columns plus \code{rr}, \code{paf},
#'   \code{attributable_cases} and \code{baseline_cases}.
#' @examples
#' t <- data.frame(fsa = c("M6H", "M5V"), exposure = c(28, 18),
#'                 population = c(40000, 60000), baseline_rate = 0.008)
#' morie_envhealth_burden_by_fsa(t)
#' @export
morie_envhealth_burden_by_fsa <- function(fsa_table, fsa_col = "fsa",
                                          exposure_col = "exposure",
                                          population_col = "population",
                                          baseline_rate_col = "baseline_rate",
                                          pollutant = "NO2",
                                          outcome = "all_cause_mortality") {
  required <- c(fsa_col, exposure_col, population_col, baseline_rate_col)
  missing <- setdiff(required, names(fsa_table))
  if (length(missing)) {
    stop(sprintf("fsa_table missing columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(fsa_table)), function(i) {
    r <- morie_envhealth_burden(
      exposure_mean = as.numeric(fsa_table[[exposure_col]][i]),
      exposure_prevalence = 1,
      baseline_rate = as.numeric(fsa_table[[baseline_rate_col]][i]),
      population = as.integer(fsa_table[[population_col]][i]),
      pollutant = pollutant, outcome = outcome)
    out <- data.frame(a = fsa_table[[fsa_col]][i], stringsAsFactors = FALSE)
    names(out) <- fsa_col
    out[[exposure_col]] <- as.numeric(fsa_table[[exposure_col]][i])
    out[[population_col]] <- as.integer(fsa_table[[population_col]][i])
    out[[baseline_rate_col]] <- as.numeric(fsa_table[[baseline_rate_col]][i])
    out$rr <- r$extra$rr
    out$paf <- r$paf
    out$attributable_cases <- r$attributable_cases
    out$baseline_cases <- r$baseline_cases
    out
  })
  res <- do.call(rbind, rows)
  res <- res[order(-res$attributable_cases), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' @rdname morie_envhealth_crf_pm25
#' @export
morie_envhealth_cheatsheet <- function() {
  paste0("morie envhealth: morie_envhealth_crf_pm25/no2, morie_envhealth_attributable_fraction, ",
         "morie_envhealth_mortality_displaced, morie_envhealth_burden, morie_envhealth_sensitivity, ",
         "morie_envhealth_equity, morie_envhealth_burden_by_fsa, morie_verify_pollution.")
}

# ---- verify-pollution ------------------------------------------------------

.envhealth_demo_data <- function(pollutant) {
  # Calibrated like the Python demo (Toronto FSA-level NO2 and CIHI asthma
  # orders of magnitude); R's own RNG, so the draws differ from numpy's.
  .rmorie_local_seed(20260417L)
  n <- 1000L
  meanlog <- switch(pollutant, no2 = log(22), pm25 = log(9), log(15))
  sdlog <- if (pollutant == "pm25") 0.30 else 0.35
  exposure <- stats::rlnorm(n, meanlog, sdlog)
  income <- sample(1:5, n, replace = TRUE, prob = c(0.18, 0.22, 0.22, 0.20, 0.18))
  mult <- c(1.25, 1.12, 1.00, 0.93, 0.85)[income]
  data.frame(exposure = exposure * mult, income = income)
}

.envhealth_assumptions <- function(pollutant, exposure_mean, exposure_prevalence,
                                   baseline_rate, population, reference) {
  row <- function(name, ok, note) list(assumption = name, ok = isTRUE(ok), note = note)
  list(
    row("exposure > reference", exposure_mean > reference,
        sprintf("mean %s vs ref %s -- CRF is monotonic only when exposure exceeds the counterfactual floor.",
                format(exposure_mean, digits = 10), format(reference))),
    row("prevalence in [0,1]", exposure_prevalence >= 0 && exposure_prevalence <= 1,
        sprintf("exposure_prevalence=%s", format(exposure_prevalence))),
    row("baseline_rate non-negative", baseline_rate >= 0,
        sprintf("baseline_rate=%s per 100k per year", format(baseline_rate))),
    row("population positive", population > 0, sprintf("population=%s", format(population))),
    row("pollutant supported by envhealth CRF", tolower(pollutant) %in% c("no2", "pm25"),
        "Current CRFs: NO2 (log-linear), PM2.5 (log-linear all-cause; Burnett IER for IHD and stroke). Other pollutants reject.")
  )
}

#' Run the pollution-to-health causal pipeline and report it
#'
#' The R twin of \code{morie verify-pollution}: resolves the exposure (demo
#' data, a CSV with an \code{exposure} and optional \code{income} column, or
#' scalar arguments), logs the assumptions, and when they all hold runs the
#' concentration-response, attributable-fraction, mortality-displaced, burden
#' and equity stages.
#' @param pollutant \code{"no2"} or \code{"pm25"}.
#' @param outcome Outcome key for the concentration-response function.
#' @param region,years Labels for the report.
#' @param demo Use synthetic demo data.
#' @param exposure_csv Path of a CSV with an \code{exposure} column.
#' @param exposure_mean,exposure_prevalence Scalar inputs used when neither
#'   \code{demo} nor \code{exposure_csv} is given.
#' @param reference Counterfactual reference concentration.
#' @param baseline_rate Baseline outcome rate per 100,000 per year.
#' @param population Population at risk.
#' @return The report as a list; its \code{status} is \code{"ok"},
#'   \code{"assumption_failure"} or \code{"error"}, and the attribute
#'   \code{exit_status} carries the command-line exit code (0, 1 or 2).
#' @examples
#' r <- morie_verify_pollution("no2", demo = TRUE)
#' r$status
#' r$pipeline$paf
#' @export
morie_verify_pollution <- function(pollutant, outcome = "all_cause_mortality",
                                   region = NULL, years = NULL, demo = FALSE,
                                   exposure_csv = NULL, exposure_mean = 0,
                                   exposure_prevalence = 0, reference = 5.8,
                                   baseline_rate = 500, population = 1e6) {
  pollutant <- tolower(pollutant)
  equity_df <- NULL
  fail <- function(msg) structure(list(status = "error", error = msg), exit_status = 2L)
  if (isTRUE(demo)) {
    df <- .envhealth_demo_data(pollutant)
    exposure_mean <- mean(df$exposure)
    exposure_prevalence <- mean(df$exposure > reference)
    equity_df <- df
    data_source <- "demo (synthetic)"
  } else if (!is.null(exposure_csv)) {
    if (!file.exists(exposure_csv)) return(fail(sprintf("exposure CSV not found: %s", exposure_csv)))
    df <- utils::read.csv(exposure_csv, stringsAsFactors = FALSE)
    if (!"exposure" %in% names(df) && "value" %in% names(df)) {
      # a NAPS pull (rmorie pull naps-...): hourly `value` in `unit`; NO2 is reported in ppb
      vals <- suppressWarnings(as.numeric(df$value))
      units <- if ("unit" %in% names(df)) unique(tolower(trimws(stats::na.omit(df$unit)))) else character()
      if (pollutant == "no2" && any(units %in% c("ppb", "ppbv"))) {
        vals <- vals * 1.88  # ug/m3 per ppb of NO2 at 25 C and 1 atm (WHO 2021 conversion)
        message("note: NO2 in ppb converted to ug/m3 (x 1.88)")
      } else if (length(setdiff(units, c("ug/m3", "\u00b5g/m3", "\u00b5g/m\u00b3", "ug/m\u00b3")))) {
        return(fail(sprintf("exposure unit %s is not ug/m3 for %s", paste(sort(units), collapse = ", "), pollutant)))
      }
      df$exposure <- vals
    }
    if (!"exposure" %in% names(df)) return(fail("CSV missing 'exposure' column (ug/m3); a NAPS pull's 'value' column also works."))
    exposure_mean <- mean(df$exposure, na.rm = TRUE)
    exposure_prevalence <- mean(df$exposure > reference, na.rm = TRUE)
    if ("income" %in% names(df)) equity_df <- df
    data_source <- exposure_csv
  } else {
    exposure_mean <- as.numeric(exposure_mean)
    exposure_prevalence <- as.numeric(exposure_prevalence)
    data_source <- "CLI scalar args"
  }
  baseline_rate <- as.numeric(baseline_rate)
  population <- as.integer(population)
  assumptions <- .envhealth_assumptions(pollutant, exposure_mean, exposure_prevalence,
                                        baseline_rate, population, reference)
  report <- list(
    command = "morie verify-pollution", pollutant = pollutant, outcome = outcome,
    region = region, years = years, data_source = data_source,
    inputs = list(exposure_mean = exposure_mean, exposure_prevalence = exposure_prevalence,
                  baseline_rate_per_100k = baseline_rate, population = population,
                  reference_conc = reference),
    assumptions = assumptions)
  if (!all(vapply(assumptions, function(a) a$ok, logical(1)))) {
    report$status <- "assumption_failure"
    report$pipeline <- list(skipped = TRUE, reason = "assumptions")
    return(structure(report, exit_status = 1L))
  }
  crf <- if (pollutant == "no2") {
    morie_envhealth_crf_no2(exposure_mean, outcome = outcome, reference_conc = reference)
  } else {
    morie_envhealth_crf_pm25(exposure_mean, outcome = outcome, reference_conc = reference)
  }
  paf <- morie_envhealth_attributable_fraction(crf$rr, exposure_prevalence)
  baseline_per_person <- baseline_rate / 1e5
  exposure_delta <- max(0, exposure_mean - reference)
  beta_per_unit <- log(crf$rr) / max(exposure_delta, 1e-9)
  # the deaths displaced by removing the excess exposure are counted over the exposed share of the
  # population, and the burden uses the same reference concentration as the RR printed above it
  displaced <- morie_envhealth_mortality_displaced(exposure_delta, population * exposure_prevalence,
                                                   baseline_per_person, beta_per_unit)
  burden <- morie_envhealth_burden(exposure_mean, exposure_prevalence, baseline_per_person,
                                   population, pollutant = if (pollutant == "pm25") "PM2.5" else "NO2",
                                   outcome = outcome,  # the same outcome as the CRF above (IHD/stroke were burdened as all-cause)
                                   reference_conc = reference)
  equity <- if (!is.null(equity_df) && "income" %in% names(equity_df)) {
    morie_envhealth_equity(equity_df, "exposure", "income")
  }
  report$status <- "ok"
  report$pipeline <- list(
    crf = unclass(crf), paf = paf,
    displaced = list(deaths_displaced = displaced, exposure_delta = exposure_delta,
                     beta_per_unit = beta_per_unit),
    burden = unclass(burden),
    equity = if (is.null(equity)) NULL else unclass(equity))
  structure(report, exit_status = 0L)
}

.envhealth_report_text <- function(report) {
  bar <- strrep("=", 66)
  if (identical(report$status, "error")) return(paste0("ERROR: ", report$error, "\n"))
  lines <- c(bar, sprintf("  morie verify-pollution -- %s -> %s", toupper(report$pollutant), report$outcome))
  if (!is.null(report$region)) lines <- c(lines, sprintf("  region: %s", report$region))
  if (!is.null(report$years)) lines <- c(lines, sprintf("  years:  %s", report$years))
  inp <- report$inputs
  lines <- c(lines, sprintf("  data:   %s", report$data_source), bar, "", "Inputs",
             sprintf("  exposure mean:      %.3f", inp$exposure_mean),
             sprintf("  exposure prevalence:%.3f", inp$exposure_prevalence),
             sprintf("  baseline rate/100k: %.2f", inp$baseline_rate_per_100k),
             sprintf("  population:         %s", format(inp$population, big.mark = ",")),
             sprintf("  reference conc:     %s", format(inp$reference_conc)),
             "", "Assumption log")
  for (a in report$assumptions) {
    lines <- c(lines, sprintf("  [%s] %s -- %s", if (a$ok) "PASS" else "FAIL", a$assumption, a$note))
  }
  if (identical(report$status, "assumption_failure")) {
    return(paste0(paste(lines, collapse = "\n"), "\n\nSTATUS: assumption_failure (pipeline skipped)\n"))
  }
  p <- report$pipeline
  lines <- c(lines, "", "Concentration-response", sprintf("  RR:       %.4f", p$crf$rr),
             sprintf("  source:   %s", p$crf$citation),
             "", sprintf("Attributable fraction (PAF): %.4f", p$paf),
             "", "Mortality displaced",
             sprintf("  expected avoided deaths: %.1f", p$displaced$deaths_displaced),
             "", "Burden of pollution",
             sprintf("  attributable deaths:   %.1f", p$burden$attributable_cases))
  if (!is.null(p$equity)) {
    lines <- c(lines, "", "Equity analysis",
               sprintf("  concentration index (Gini): %.4f", p$equity$concentration_index))
  }
  paste0(paste(lines, collapse = "\n"), "\n\nSTATUS: ok\n")
}
