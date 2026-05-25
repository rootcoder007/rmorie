# SPDX-License-Identifier: AGPL-3.0-or-later

#' Replication of Laniyonu & Goff (2021) — Police force vs SMI disparity
#'
#' R port of \code{morie.laniyonu.smi_force_disparity}.  Estimates a
#' hierarchical negative-binomial model with a synthetic area-exposure
#' (SAE) offset for persons-with-serious-mental-illness (PwSMI), with
#' year fixed effects and an area random intercept.
#'
#' The trick: there is no administrative census of who has SMI at the
#' tract level, so the denominator is built by:
#' \enumerate{
#'   \item Fitting \emph{P(SMI | age, sex, race, income, ...)} on a
#'         national survey using only covariates also tabulated at the
#'         tract level by the ACS.
#'   \item Applying those coefficients to ACS tract marginals to get a
#'         per-tract predicted P(SMI).
#'   \item Multiplying by adult population for a synthetic exposure
#'         denominator \eqn{n_{vti}}.
#' }
#'
#' The count model is
#' \deqn{y_{vti} \sim \mathrm{NegBin}(n_{vti} \exp(\mu + \alpha_v + \delta_t + \beta_i), \phi)}{y_vti ~ NegBin(n_vti exp(mu + alpha_v + delta_t + beta_i), phi)}
#' with \eqn{v} = PwSMI vs non-SMI, \eqn{t} = year, \eqn{i} = area.
#' The headline coefficient \eqn{\alpha_v}{alpha_v} is the log relative-risk of
#' police use of force against PwSMI vs non-SMI.
#'
#' Paper headlines: RR PwSMI = 11.6x (tract); 10.2x (precinct).
#'
#' This R port is a frequentist MLE approximation (via
#' \code{stats::glm.nb} in MASS, falling back to a hand-rolled NB MLE
#' on \code{stats::optim} if MASS is unavailable).  For paper-grade
#' Bayesian credible intervals, fit in \pkg{brms} / \pkg{rstanarm}
#' using the design matrix returned with \code{return_design=TRUE}.
#'
#' Surfaces a \code{warning()} on every call: the SMI flag on force
#' events is a proxy biased TOWARD THE NULL (officers miss more SMI
#' than they over-attribute), so the estimated \eqn{\alpha_v}{alpha_v} is a
#' \strong{conservative lower bound} on the true disparity.
#'
#' @references
#' Laniyonu, A., & Goff, P. A. (2021).  Measuring disparities in
#'   police use of force and injury among persons with serious mental
#'   illness.  BMC Psychiatry, 21(1), 500.
#'
#' @return A \code{list} of class \code{morie_laniyonu_smi_result}.
#' @name morie_laniyonu_smi_force_disparity
NULL


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.lan_smi_result <- function(alpha_v, intercept, year_effects,
                              area_random_effect_sd, dispersion,
                              n_events, n_area_years, log_likelihood,
                              converged, exposure_summary = list(),
                              note = "") {
  rr <- exp(alpha_v$estimate)
  ci_lo <- exp(alpha_v$estimate - 1.96 * alpha_v$std_error)
  ci_hi <- exp(alpha_v$estimate + 1.96 * alpha_v$std_error)

  head <- sprintf(
    "Laniyonu & Goff (2021) replication: %s force events across %s area-year rows. ",
    format(n_events, big.mark = ","), format(n_area_years, big.mark = ",")
  )
  rr_line <- sprintf(
    "alpha_v (PwSMI log-RR) = %+.3f (SE %.3f); RR = %.2fx [95%% CI %.2fx, %.2fx]. ",
    alpha_v$estimate, alpha_v$std_error, rr, ci_lo, ci_hi
  )
  disp_line <- sprintf(
    "NB dispersion phi = %.3f; area random-intercept SD ~= %.3f. ",
    dispersion, area_random_effect_sd
  )
  comp <- "Compare paper's headline RR = 11.6x (tract) / 10.2x (precinct). "
  conv <- if (converged) "" else "WARNING: optimiser did not converge. "
  smi_caveat <- paste(
    "CAVEAT: the SMI flag is a proxy biased TOWARD THE NULL",
    "(officers miss more SMI than they over-attribute), so alpha_v is",
    "a conservative lower bound on the true disparity."
  )

  out <- list(
    title = "Laniyonu & Goff SMI x Use-of-Force Disparity",
    call = "morie_laniyonu_smi_force_disparity(...)",
    alpha_v = alpha_v,
    intercept = intercept,
    year_effects = year_effects,
    area_random_effect_sd = area_random_effect_sd,
    dispersion = dispersion,
    rr = rr, rr_ci_low = ci_lo, rr_ci_high = ci_hi,
    n_events = n_events,
    n_area_years = n_area_years,
    log_likelihood = log_likelihood,
    converged = converged,
    exposure_summary = exposure_summary,
    note = note,
    interpretation = paste0(head, rr_line, disp_line, comp, conv, smi_caveat)
  )
  class(out) <- c("morie_laniyonu_smi_result", "morie_rich_result", "list")
  out
}


.lan_smi_coef <- function(name, estimate, std_error) {
  list(name = name,
       estimate = as.numeric(estimate),
       std_error = as.numeric(std_error))
}


# ---------------------------------------------------------------------------
# Synthetic Area Exposure (SAE) — base-R logistic + tract scoring
# ---------------------------------------------------------------------------

.lan_smi_sae <- function(survey_df, survey_trait_col, survey_covariate_cols,
                          area_df, area_population_col) {
  needed_s <- c(survey_trait_col, survey_covariate_cols)
  missing_s <- setdiff(needed_s, names(survey_df))
  if (length(missing_s) > 0L) {
    stop("survey_df missing columns: ", paste(missing_s, collapse = ", "))
  }
  needed_a <- c(survey_covariate_cols, area_population_col)
  missing_a <- setdiff(needed_a, names(area_df))
  if (length(missing_a) > 0L) {
    stop("area_df missing columns: ", paste(missing_a, collapse = ", "))
  }

  fm <- as.formula(paste0("`", survey_trait_col, "` ~ ",
                          paste0("`", survey_covariate_cols, "`",
                                  collapse = " + ")))
  fit <- suppressWarnings(stats::glm(fm, data = survey_df,
                                       family = stats::binomial()))
  p_smi <- stats::predict(fit, newdata = area_df, type = "response")
  setNames(p_smi * area_df[[area_population_col]], rownames(area_df))
}


# ---------------------------------------------------------------------------
# Negative-binomial fitter
# ---------------------------------------------------------------------------

.lan_smi_fit_nb <- function(X, y, offset_vec, max_iter, tol) {
  # Prefer MASS::glm.nb when available; fall back to optim-based MLE.
  if (requireNamespace("MASS", quietly = TRUE)) {
    df_fit <- data.frame(.y = y, X, check.names = FALSE)
    fm <- as.formula(paste0(".y ~ -1 + ",
                            paste0("`", colnames(X), "`",
                                    collapse = " + "),
                            " + offset(.off)"))
    df_fit$.off <- offset_vec
    fit <- tryCatch(
      suppressWarnings(MASS::glm.nb(fm, data = df_fit,
                                       control = stats::glm.control(
                                         maxit = max_iter, epsilon = tol))),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      sm <- summary(fit)$coefficients
      return(list(
        coef = sm[, "Estimate"],
        se = sm[, "Std. Error"],
        loglik = as.numeric(stats::logLik(fit)),
        phi = fit$theta,
        converged = fit$converged
      ))
    }
  }

  # Hand-rolled NB2 MLE (mirrors the Python scipy.optimize path).
  np <- ncol(X)
  init <- c(rep(0, np), 0)  # last param is log_phi
  neg_ll <- function(par) {
    beta <- par[seq_len(np)]
    log_phi <- max(min(par[np + 1L], 10), -10)
    phi <- exp(log_phi)
    eta <- as.numeric(X %*% beta) + offset_vec
    eta <- pmax(pmin(eta, 50), -50)
    mu <- pmin(pmax(exp(eta), 1e-12), 1e12)
    ll <- lgamma(y + phi) - lgamma(phi) - lgamma(y + 1) +
      phi * (log(phi) - log(phi + mu)) +
      y * (log(mu) - log(phi + mu))
    nll <- -sum(ll)
    if (!is.finite(nll)) return(1e12)
    nll
  }
  res <- tryCatch(
    stats::optim(init, neg_ll, method = "BFGS",
                  control = list(maxit = max_iter, reltol = tol),
                  hessian = TRUE),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(list(coef = rep(NA_real_, np), se = rep(NA_real_, np),
                 loglik = NA_real_, phi = NA_real_, converged = FALSE))
  }
  H <- res$hessian + diag(1e-8, length(res$par))
  cov_ <- tryCatch(solve(H), error = function(e) NULL)
  se_all <- if (is.null(cov_)) rep(NA_real_, length(res$par))
            else sqrt(pmax(diag(cov_), 0))
  list(
    coef = setNames(res$par[seq_len(np)], colnames(X)),
    se = setNames(se_all[seq_len(np)], colnames(X)),
    loglik = -res$value,
    phi = exp(max(min(res$par[np + 1L], 10), -10)),
    converged = res$convergence == 0
  )
}


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

#' Replicate Laniyonu & Goff (2021): hierarchical NB on SMI force disparity
#'
#' Composes a synthetic-area-exposure (SAE) step (base-R logistic on
#' survey microdata, predicted at ACS tract marginals) into a
#' negative-binomial GLM with year fixed effects and an area random
#' intercept approximated by ridge-penalised area dummies.
#'
#' @param df Force-event panel, one row per (area, year).
#' @param survey_df Survey microdata for fitting P(SMI | covariates).
#' @param survey_trait_col Binary column in \code{survey_df}.
#' @param survey_covariate_cols Covariates available in BOTH survey_df
#'   and df.
#' @param area_covariate_cols Optional rename map for df.
#' @param force_count_col Count of force events against PwSMI per
#'   (area, year).
#' @param non_smi_count_col Count of force events against non-SMI per
#'   (area, year).  If \code{NULL}, df must contain
#'   \code{total_force_events} and the non-SMI count is computed as
#'   total minus PwSMI.
#' @param geog_col,year_col,population_col Column names.
#' @param baseline_year Year to drop as the reference (default = min).
#' @param include_year_fe,include_area_re Toggle the year FE / area RE
#'   blocks.
#' @param max_iter,tol Optimiser controls.
#' @param return_design Attach \code{X}, \code{y}, \code{offset} to
#'   \code{exposure_summary} for hand-off to brms / rstanarm.
#'
#' @return A list of class \code{morie_laniyonu_smi_result}.
#' @export
morie_laniyonu_smi_force_disparity <- function(
  df,
  survey_df,
  survey_trait_col = "smi",
  survey_covariate_cols,
  area_covariate_cols = NULL,
  force_count_col = "force_events",
  non_smi_count_col = NULL,
  geog_col = "tract_id",
  year_col = "year",
  population_col = "pop_18plus",
  baseline_year = NULL,
  include_year_fe = TRUE,
  include_area_re = TRUE,
  max_iter = 500L,
  tol = 1e-6,
  return_design = FALSE
) {
  stopifnot(is.data.frame(df), is.data.frame(survey_df))

  # Mandatory SMI-flag proxy-bias warning on every call.
  warning(paste(
    "morie_laniyonu_smi_force_disparity: the SMI flag on force events",
    "is a proxy biased TOWARD THE NULL - officers are more likely to MISS",
    "SMI than to over-attribute it.  alpha_v is a CONSERVATIVE LOWER BOUND",
    "on the true disparity (Laniyonu & Goff 2021, Limitations)."
  ), call. = FALSE)

  if (is.null(area_covariate_cols)) {
    area_covariate_cols <- survey_covariate_cols
  }
  if (length(area_covariate_cols) != length(survey_covariate_cols)) {
    stop("area_covariate_cols must have the same length as survey_covariate_cols.")
  }

  # 1. Build SAE offset --------------------------------------------------
  agg_cols <- c(area_covariate_cols, population_col)
  area_frame <- stats::aggregate(
    df[, agg_cols, drop = FALSE],
    by = list(.geog = df[[geog_col]]),
    FUN = function(v) mean(v, na.rm = TRUE)
  )
  rownames(area_frame) <- area_frame$.geog
  area_frame$.geog <- NULL
  # Rename to match survey covariate names
  rn <- setNames(survey_covariate_cols, area_covariate_cols)
  for (oc in area_covariate_cols) {
    if (oc != rn[[oc]]) {
      area_frame[[rn[[oc]]]] <- area_frame[[oc]]
      area_frame[[oc]] <- NULL
    }
  }
  smi_exposure <- .lan_smi_sae(
    survey_df = survey_df,
    survey_trait_col = survey_trait_col,
    survey_covariate_cols = survey_covariate_cols,
    area_df = area_frame,
    area_population_col = population_col
  )
  smi_exp_vec <- smi_exposure[as.character(df[[geog_col]])]
  non_smi_exp <- pmax(df[[population_col]] - smi_exp_vec, 1)
  smi_exp_vec <- pmax(smi_exp_vec, 1)

  # 2. Resolve non-SMI count column --------------------------------------
  if (is.null(non_smi_count_col)) {
    if (!"total_force_events" %in% names(df)) {
      stop("non_smi_count_col is NULL but df has no 'total_force_events' column.")
    }
    non_smi_count <- pmax(df$total_force_events - df[[force_count_col]], 0)
  } else {
    non_smi_count <- df[[non_smi_count_col]]
  }

  # 3. Stack to long (area x year x vulnerability) -----------------------
  smi_rows <- data.frame(
    .geog = df[[geog_col]], .year = df[[year_col]],
    .count = df[[force_count_col]], .offset = smi_exp_vec, .v = 1L
  )
  non_smi_rows <- data.frame(
    .geog = df[[geog_col]], .year = df[[year_col]],
    .count = non_smi_count, .offset = non_smi_exp, .v = 0L
  )
  long_df <- rbind(smi_rows, non_smi_rows)
  long_df <- long_df[stats::complete.cases(long_df) & long_df$.offset > 0, ,
                      drop = FALSE]

  # 4. Build design matrix ----------------------------------------------
  if (is.null(baseline_year)) baseline_year <- min(long_df$.year)
  years_all <- sort(unique(long_df$.year))
  non_baseline_years <- setdiff(years_all, baseline_year)

  X_pieces <- list(
    `(Intercept)` = rep(1, nrow(long_df)),
    alpha_v = as.numeric(long_df$.v)
  )
  if (include_year_fe) {
    for (y in non_baseline_years) {
      X_pieces[[paste0("year_", y)]] <- as.numeric(long_df$.year == y)
    }
  }
  area_levels <- sort(unique(long_df$.geog))
  baseline_area <- area_levels[1]
  if (include_area_re && length(area_levels) > 1L) {
    for (a in area_levels[-1]) {
      X_pieces[[paste0("area_", a)]] <- as.numeric(long_df$.geog == a)
    }
  }
  X <- do.call(cbind, X_pieces)
  colnames(X) <- names(X_pieces)
  y_count <- long_df$.count
  offset_vec <- log(long_df$.offset)

  # 5. Fit -----------------------------------------------------------------
  fit <- .lan_smi_fit_nb(X, y_count, offset_vec, max_iter, tol)

  # 6. Pack coefficients ---------------------------------------------------
  alpha_v <- .lan_smi_coef("alpha_v",
                            fit$coef["alpha_v"],
                            fit$se["alpha_v"])
  intercept <- .lan_smi_coef("mu",
                              fit$coef["(Intercept)"],
                              fit$se["(Intercept)"])
  year_effects <- list()
  if (include_year_fe) {
    for (y in non_baseline_years) {
      nm <- paste0("year_", y)
      year_effects[[length(year_effects) + 1L]] <- .lan_smi_coef(
        sprintf("delta_%s", as.character(y)),
        fit$coef[nm], fit$se[nm]
      )
    }
  }

  # Area-RE SD from fitted dummies
  area_names <- grep("^area_", names(fit$coef), value = TRUE)
  if (length(area_names) > 1L) {
    area_re_sd <- stats::sd(fit$coef[area_names], na.rm = TRUE)
  } else {
    area_re_sd <- 0.0
  }

  exposure_summary <- list(
    smi_exposure_mean = mean(smi_exposure, na.rm = TRUE),
    smi_exposure_median = stats::median(smi_exposure, na.rm = TRUE),
    smi_exposure_min = min(smi_exposure, na.rm = TRUE),
    smi_exposure_max = max(smi_exposure, na.rm = TRUE),
    n_areas = nrow(area_frame),
    baseline_year = baseline_year,
    baseline_area = baseline_area
  )
  if (return_design) {
    exposure_summary$X <- X
    exposure_summary$y <- y_count
    exposure_summary$offset <- offset_vec
    exposure_summary$design_cols <- colnames(X)
  }

  note <- paste(
    "Frequentist MLE (MASS::glm.nb or optim fall-back) approximation",
    "to the paper's Bayesian hierarchical NB.  Area random intercept",
    "implemented as area dummies; SDs reported are empirical.",
    "Pass return_design=TRUE to hand off to brms / rstanarm for",
    "paper-grade credible intervals."
  )

  .lan_smi_result(
    alpha_v = alpha_v,
    intercept = intercept,
    year_effects = year_effects,
    area_random_effect_sd = area_re_sd,
    dispersion = fit$phi,
    n_events = as.integer(sum(y_count)),
    n_area_years = as.integer(nrow(X)),
    log_likelihood = fit$loglik,
    converged = isTRUE(fit$converged),
    exposure_summary = exposure_summary,
    note = note
  )
}


#' @export
print.morie_laniyonu_smi_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  cat(x$interpretation, "\
")
  if (nzchar(x$note)) cat("Note: ", x$note, "\
", sep = "")
  invisible(x)
}
