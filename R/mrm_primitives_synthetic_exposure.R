# SPDX-License-Identifier: AGPL-3.0-or-later
#' Synthetic small-area-estimated exposure offset (MRM primitive)
#'
#' Mirrors the Python module
#' \code{morie.mrm_primitives.synthetic_exposure}, adapted from
#' Laniyonu & Goff (2021) BMC Psychiatry 21(1):500.
#'
#' The trick: when you need a rate-per-hidden-subpopulation (force-
#' per-PwSMI, contact-per-undocumented, contact-per-homeless) and no
#' administrative census of that subpopulation exists, you can:
#'
#' \enumerate{
#'   \item Fit \eqn{P(\text{trait} | \text{covariates})}{P(trait | covariates)} on a national
#'     probability sample (NCS-R for SMI; ACS-style survey for other
#'     traits) using ONLY covariates also available at the area level.
#'   \item Apply the fitted coefficients to area-level marginals from
#'     ACS / census to predict \eqn{P(\text{trait})}{P(trait)} per area.
#'   \item Multiply by area-level adult population to get a synthetic
#'     "population at risk" denominator.
#' }
#'
#' Generalises far beyond Laniyonu & Goff's SMI application:
#' homelessness rates of police force, LGBTQ stop-and-frisk rates,
#' undocumented-immigrant ICE-contact rates -- any "rate per hidden
#' subpopulation" estimand.
#'
#' The returned offset is suitable for use as the \code{offset=
#' log(exposure)} argument in a Poisson / negative-binomial GLM that
#' counts trait-specific events.
#'
#' @name mrm_synthetic_exposure
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# .mrm_result is defined in mrm_primitives_gentrification.R and is shared
# across the three MRM primitives files. Same for print.morie_mrm_result.

#' @noRd
.se_logistic_fit <- function(X, y, max_iter = 200L, tol = 1e-6) {
  # Minimal-dependency logistic regression via Newton-IRLS. Returns a
  # coefficient vector of length p+1 (intercept first). Mirrors the
  # Python `_logistic_fit` line-for-line: stable sigmoid, weight
  # clipping to >=1e-10, and singular-Hessian fallback to the last
  # iterate.
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  X_int <- cbind(1, X)
  beta <- numeric(p + 1L)
  for (iter in seq_len(max_iter)) {
    eta <- as.numeric(X_int %*% beta)
    # Stable sigmoid (branch on sign of eta, like the Python np.where)
    pos <- eta >= 0
    mu <- numeric(n)
    mu[pos] <- 1.0 / (1.0 + exp(-eta[pos]))
    mu[!pos] <- exp(eta[!pos]) / (1.0 + exp(eta[!pos]))
    # IRLS weights, clipped to avoid divide-by-zero
    w <- mu * (1 - mu)
    w <- pmax(w, 1e-10)
    XW <- X_int * w           # row-wise scale; recycling along columns
    XWX <- crossprod(X_int, XW)
    XWz <- crossprod(XW, X_int %*% beta + (y - mu) / w)
    new_beta <- tryCatch(
      as.numeric(solve(XWX, XWz)),
      error = function(e) NULL
    )
    if (is.null(new_beta)) {
      return(beta)  # singular Hessian -- return last good iterate
    }
    if (max(abs(new_beta - beta)) < tol) {
      return(new_beta)
    }
    beta <- new_beta
  }
  beta
}


# ---------------------------------------------------------------------------
# mrm_synthetic_area_exposure
# ---------------------------------------------------------------------------

#' Compute the synthetic exposure offset for each area
#'
#' \enumerate{
#'   \item Fit logistic \eqn{P(\text{trait} | \text{covariates})}{P(trait | covariates)} on
#'     the survey microdata.
#'   \item Apply fitted coefficients to area-level marginals from
#'     \code{area_df}.
#'   \item Multiply predicted rate by area population to obtain the
#'     synthetic "population at risk" exposure offset.
#' }
#'
#' @param survey_df A \code{data.frame} of survey microdata (one row
#'   per respondent), carrying \code{survey_trait_col} (0/1 or logical)
#'   and \code{survey_covariate_cols}.
#' @param survey_trait_col Character. Name of the binary trait column.
#' @param survey_covariate_cols Character vector of covariates that are
#'   present in BOTH the survey and the area dataset.
#' @param area_df A \code{data.frame} with one row per area (tract,
#'   precinct, etc.); must carry the same covariate columns as
#'   area-level proportions / means, plus \code{area_population_col}.
#' @param area_population_col Character. Adult-population column in
#'   \code{area_df}.
#' @param fit_callable Optional function with signature
#'   \code{function(X, y) -> coef}, returning a coefficient vector of
#'   length \code{length(survey_covariate_cols) + 1L} (intercept
#'   first). Defaults to a base-R Newton-IRLS logistic fit.
#' @param return_per_area_rate Logical; default \code{FALSE}. If
#'   \code{TRUE} the result list also carries \code{predicted_rate}.
#' @return A named list with classes \code{morie_mrm_result},
#'   \code{morie_rich_result}, \code{list}. Carries
#'   \code{exposure} (named numeric vector, one entry per area row),
#'   \code{predicted_rate} (when requested), \code{coef} (the fitted
#'   logistic coefficient vector), plus \code{interpretation} +
#'   \code{warnings}.
#' @examples
#' set.seed(2)
#' n_survey <- 500
#' x1 <- rnorm(n_survey); x2 <- rnorm(n_survey)
#' p  <- 1 / (1 + exp(-(-2 + 0.6 * x1 - 0.4 * x2)))
#' y  <- rbinom(n_survey, 1, p)
#' survey <- data.frame(trait = y, x1 = x1, x2 = x2)
#'
#' area <- data.frame(
#'   x1 = rnorm(20), x2 = rnorm(20),
#'   pop = sample(800:1500, 20, replace = TRUE)
#' )
#' rownames(area) <- paste0("area_", seq_len(20))
#' res <- mrm_synthetic_area_exposure(
#'   survey_df = survey,
#'   survey_trait_col = "trait",
#'   survey_covariate_cols = c("x1", "x2"),
#'   area_df = area,
#'   area_population_col = "pop"
#' )
#' head(res$exposure)
#' @export
mrm_synthetic_area_exposure <- function(survey_df,
                                        survey_trait_col,
                                        survey_covariate_cols,
                                        area_df,
                                        area_population_col,
                                        fit_callable = NULL,
                                        return_per_area_rate = FALSE) {
  stopifnot(is.data.frame(survey_df), is.data.frame(area_df),
            is.character(survey_trait_col),
            length(survey_trait_col) == 1L,
            is.character(survey_covariate_cols),
            length(survey_covariate_cols) >= 1L,
            is.character(area_population_col),
            length(area_population_col) == 1L)

  call_str <- sprintf(
    "mrm_synthetic_area_exposure(survey=<%dr>, area=<%dr>, covariates=[%s])",
    nrow(survey_df), nrow(area_df),
    paste(survey_covariate_cols, collapse = ",")
  )

  warnings <- character(0)

  miss_survey <- setdiff(c(survey_trait_col, survey_covariate_cols),
                         names(survey_df))
  if (length(miss_survey) > 0L) {
    return(.mrm_result(
      "MRM Synthetic Area Exposure",
      call_str,
      warnings = sprintf("survey_df missing column(s): %s",
                         paste(miss_survey, collapse = ", ")),
      interpretation = sprintf(
        "No analysis: survey_df is missing required column(s): %s.",
        paste(miss_survey, collapse = ", ")
      ),
      n = 0L
    ))
  }
  miss_area <- setdiff(c(survey_covariate_cols, area_population_col),
                       names(area_df))
  if (length(miss_area) > 0L) {
    return(.mrm_result(
      "MRM Synthetic Area Exposure",
      call_str,
      warnings = sprintf("area_df missing column(s): %s",
                         paste(miss_area, collapse = ", ")),
      interpretation = sprintf(
        "No analysis: area_df is missing required column(s): %s.",
        paste(miss_area, collapse = ", ")
      ),
      n = 0L
    ))
  }

  X_survey <- as.matrix(survey_df[, survey_covariate_cols, drop = FALSE])
  storage.mode(X_survey) <- "double"
  y_raw <- survey_df[[survey_trait_col]]
  if (is.logical(y_raw)) {
    y <- as.numeric(y_raw)
  } else if (is.numeric(y_raw)) {
    y <- as.numeric(y_raw != 0)
  } else {
    y_str <- tolower(as.character(y_raw))
    map_pos <- c("yes", "true", "1", "y", "t")
    map_neg <- c("no", "false", "0", "n", "f")
    y <- ifelse(y_str %in% map_pos, 1,
                ifelse(y_str %in% map_neg, 0, NA_real_))
  }

  bad_rows <- !is.finite(y) |
    apply(X_survey, 1L, function(r) any(!is.finite(r)))
  n_bad <- sum(bad_rows)
  if (n_bad > 0L) {
    warnings <- c(warnings, sprintf(
      "%d survey row(s) dropped (non-finite trait/covariates).", n_bad
    ))
    X_survey <- X_survey[!bad_rows, , drop = FALSE]
    y <- y[!bad_rows]
  }
  if (nrow(X_survey) < length(survey_covariate_cols) + 1L) {
    return(.mrm_result(
      "MRM Synthetic Area Exposure",
      call_str,
      warnings = c(warnings, sprintf(
        "Only %d usable survey row(s) for %d covariate(s) plus intercept; fit not identified.",
        nrow(X_survey), length(survey_covariate_cols)
      )),
      interpretation = "No analysis: too few usable survey rows to fit the logistic.",
      n = 0L
    ))
  }
  if (length(unique(y)) < 2L) {
    return(.mrm_result(
      "MRM Synthetic Area Exposure",
      call_str,
      warnings = c(warnings, "Survey trait is constant; cannot fit logistic."),
      interpretation = "No analysis: the survey trait is constant after dropping non-finite rows.",
      n = 0L
    ))
  }

  if (is.null(fit_callable)) {
    coef <- .se_logistic_fit(X_survey, y)
  } else {
    coef <- as.numeric(fit_callable(X_survey, y))
  }
  if (length(coef) != length(survey_covariate_cols) + 1L) {
    stop(sprintf(
      "fit_callable returned coef of length %d; expected %d (intercept + %d covariates).",
      length(coef), length(survey_covariate_cols) + 1L,
      length(survey_covariate_cols)
    ))
  }

  X_area <- as.matrix(area_df[, survey_covariate_cols, drop = FALSE])
  storage.mode(X_area) <- "double"
  X_area_with_int <- cbind(1, X_area)
  eta <- as.numeric(X_area_with_int %*% coef)
  pred_rate <- 1.0 / (1.0 + exp(-eta))

  pop <- as.numeric(area_df[[area_population_col]])
  if (any(!is.finite(pop) | pop < 0)) {
    warnings <- c(warnings,
      "Non-finite or negative area populations detected; exposure values will be propagated as NA / negative.")
  }
  exposure <- pred_rate * pop
  area_names <- rownames(area_df)
  if (is.null(area_names)) area_names <- as.character(seq_len(nrow(area_df)))
  names(exposure) <- area_names
  names(pred_rate) <- area_names

  rate_summary <- if (any(is.finite(pred_rate))) {
    sprintf("min=%.4f, median=%.4f, max=%.4f",
            min(pred_rate, na.rm = TRUE),
            stats::median(pred_rate, na.rm = TRUE),
            max(pred_rate, na.rm = TRUE))
  } else {
    "no finite predicted rates"
  }
  expo_summary <- if (any(is.finite(exposure))) {
    sprintf("min=%.2f, median=%.2f, max=%.2f, sum=%.0f",
            min(exposure, na.rm = TRUE),
            stats::median(exposure, na.rm = TRUE),
            max(exposure, na.rm = TRUE),
            sum(exposure, na.rm = TRUE))
  } else {
    "no finite exposures"
  }

  interp <- paste(
    sprintf(
      "Fitted logistic of %s on %d covariate(s) (%s) across %d usable survey row(s).",
      survey_trait_col, length(survey_covariate_cols),
      paste(survey_covariate_cols, collapse = ", "),
      nrow(X_survey)
    ),
    sprintf(
      "Per-area predicted trait rate: %s. Per-area synthetic exposure (rate x population): %s.",
      rate_summary, expo_summary
    ),
    "Use the exposure vector as offset = log(exposure) in a Poisson / negative-binomial GLM that counts trait-specific events. Cite the underlying survey explicitly when reporting.",
    sep = " "
  )

  extras <- list()
  if (isTRUE(return_per_area_rate)) {
    extras$predicted_rate <- pred_rate
  }

  do.call(.mrm_result, c(
    list(
      "MRM Synthetic Area Exposure",
      call_str,
      summary_lines = list(
        `Survey rows (usable)` = nrow(X_survey),
        `Areas`                = nrow(area_df),
        `Covariates`           = length(survey_covariate_cols),
        `Intercept (beta_0)`   = coef[1],
        `Mean predicted rate`  = if (any(is.finite(pred_rate)))
                                    mean(pred_rate, na.rm = TRUE) else NA_real_,
        `Sum exposure`         = if (any(is.finite(exposure)))
                                    sum(exposure, na.rm = TRUE) else NA_real_
      ),
      warnings = warnings,
      interpretation = interp,
      n = nrow(area_df),
      n_survey = nrow(X_survey),
      n_areas  = nrow(area_df),
      coef = setNames(coef, c("(Intercept)", survey_covariate_cols)),
      exposure = exposure,
      value = if (any(is.finite(exposure))) sum(exposure, na.rm = TRUE) else NA_real_
    ),
    extras
  ))
}
