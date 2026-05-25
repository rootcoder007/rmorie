# SPDX-License-Identifier: AGPL-3.0-or-later

#' Replication of O'Connell & Laniyonu (2025) — CSC actuarial-risk disparity
#'
#' R port of \code{morie.laniyonu.actuarial_risk_disparity}.  Audits
#' the Correctional Service of Canada's four ordinal risk instruments
#' (Static, DFIA-R Dynamic, Offender Security Level, Reintegration
#' Potential) and the two binary downstream outcomes (parole granted;
#' institutional housing level) for race x gender bias.
#'
#' Two empirical stages match the paper:
#' \itemize{
#'   \item Stage 1 (ordinal scores): \emph{threshold-specific}
#'     cumulative-logit, fit as two separate binary logits at the
#'     low->medium and medium->high cutoffs, plus a proportional-odds
#'     LR test.  The headline pattern is much larger |beta| at the
#'     low->medium cut than at medium->high.
#'   \item Stage 2 (binary outcomes): the score-net-residual audit -
#'     logistic regression of outcome on actuarial score + race
#'     indicators (+ controls).  A non-zero residual race coefficient
#'     is the disparate-treatment signal.
#' }
#'
#' Caveat surfaced via \code{warning()} on every call (Goel et al. 2021):
#' a non-zero residual race coefficient is evidence of OUTPUT disparity,
#' not PREDICTIVE-VALIDITY disparity.  The two are conceptually
#' distinct; the paper's disparate-treatment claim rests on the former.
#'
#' @references
#' O'Connell, C., & Laniyonu, A. (2025).  Race, gender, and risk
#'   assessments in Canadian federal prison.  Race & Justice, 15(3),
#'   428-453.
#'
#' Goel, S., Shroff, R., Skeem, J., & Slobogin, C. (2021).  The
#'   accuracy, equity, and jurisprudence of criminal risk assessment.
#'   In Research Handbook on Big Data Law (pp. 9-28).
#'
#' @return A named \code{list} of class \code{morie_laniyonu_ard_result}
#'   carrying the per-stratum coefficients and a multi-paragraph
#'   \code{interpretation} string.
#' @name morie_laniyonu_actuarial_risk_disparity
NULL


# ---------------------------------------------------------------------------
# Outcome registry
# ---------------------------------------------------------------------------

.lan_ard_ordinal <- list(
  static        = "static_score",
  dynamic       = "dynamic_score",
  osl           = "offender_security_level",
  reintegration = "reintegration_potential"
)
.lan_ard_binary <- list(
  parole  = "parole_granted",
  housing = "housing_level"
)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.lan_ard_result <- function(title, call, interpretation = "",
                             warnings_ = character(0), ...) {
  out <- list(
    title = title,
    call = call,
    warnings = warnings_,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_laniyonu_ard_result", "morie_rich_result", "list")
  out
}


.lan_threshold_logit <- function(yk, X) {
  # Binary logit at a single ordinal threshold (P(Y > k)).
  # Returns coefficients (with intercept), SEs, log-lik.
  if (length(unique(yk)) < 2L) {
    return(list(coef = rep(NA_real_, ncol(X) + 1L),
                 se = rep(NA_real_, ncol(X) + 1L),
                 loglik = NA_real_, n = length(yk),
                 converged = FALSE))
  }
  df <- data.frame(.y = yk, X, check.names = FALSE)
  fm <- as.formula(paste(".y ~", paste0("`", colnames(X), "`",
                                          collapse = " + ")))
  fit <- tryCatch(
    suppressWarnings(stats::glm(fm, data = df, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit) || !fit$converged) {
    return(list(coef = rep(NA_real_, ncol(X) + 1L),
                 se = rep(NA_real_, ncol(X) + 1L),
                 loglik = NA_real_, n = length(yk),
                 converged = FALSE))
  }
  sm <- summary(fit)$coefficients
  list(
    coef = sm[, "Estimate"],
    se = sm[, "Std. Error"],
    loglik = as.numeric(stats::logLik(fit)),
    n = length(yk),
    converged = TRUE
  )
}


.lan_ord_levels_to_int <- function(y, levels_) {
  m <- match(as.character(y), levels_)
  if (any(is.na(m))) {
    bad <- unique(as.character(y)[is.na(m)])
    stop("ordinal level(s) not in `ordinal_levels`: ",
         paste(bad, collapse = ", "))
  }
  m
}


# ---------------------------------------------------------------------------
# Stage 1 — threshold-specific ordinal logit
# ---------------------------------------------------------------------------

.lan_run_ordinal <- function(df, outcome_col, race_cols, gender_col,
                              control_cols, ordinal_levels,
                              split_by_gender) {
  results <- list()
  per_thresh <- list()
  worst_p <- NA_real_
  worst_stat <- NA_real_
  worst_df_v <- NA_integer_

  fit_one <- function(sub, stratum_label) {
    y_int <- .lan_ord_levels_to_int(sub[[outcome_col]], ordinal_levels)
    # Two thresholds for 3-level (low/medium/high) => P(Y > 1), P(Y > 2)
    thresholds <- seq_len(length(ordinal_levels) - 1L)
    thresh_labels <- vapply(thresholds, function(k) {
      sprintf("%s|%s", ordinal_levels[k], ordinal_levels[k + 1L])
    }, character(1))
    cov_cols <- c(race_cols, control_cols)
    X <- as.matrix(sub[, cov_cols, drop = FALSE])
    coef_mat <- matrix(NA_real_, nrow = length(thresholds),
                       ncol = ncol(X) + 1L,
                       dimnames = list(thresh_labels,
                                       c("(Intercept)", cov_cols)))
    se_mat <- coef_mat
    pofits <- vector("list", length(thresholds))
    for (k in thresholds) {
      fit <- .lan_threshold_logit(as.integer(y_int > k), X)
      coef_mat[k, ] <- fit$coef
      se_mat[k, ] <- fit$se
      pofits[[k]] <- fit
    }
    # Proportional-odds LR test: pooled (PO) vs threshold-specific
    yk_long <- unlist(lapply(thresholds, function(k) as.integer(y_int > k)))
    thresh_long <- factor(rep(thresh_labels, each = length(y_int)))
    X_pool_long <- do.call(rbind, replicate(length(thresholds), X,
                                              simplify = FALSE))
    df_pool <- data.frame(.y = yk_long, .thresh = thresh_long,
                          X_pool_long, check.names = FALSE)
    pofit <- tryCatch(
      suppressWarnings(stats::glm(
        as.formula(paste(".y ~ .thresh +",
                          paste0("`", cov_cols, "`", collapse = " + "))),
        data = df_pool, family = stats::binomial()
      )),
      error = function(e) NULL
    )
    tsfit <- tryCatch(
      suppressWarnings(stats::glm(
        as.formula(paste(".y ~ .thresh *",
                          paste0("(`", paste(cov_cols, collapse = "` + `"),
                                  "`)"))),
        data = df_pool, family = stats::binomial()
      )),
      error = function(e) NULL
    )
    lr_stat <- NA_real_
    lr_df <- NA_integer_
    lr_p <- NA_real_
    if (!is.null(pofit) && !is.null(tsfit)) {
      lr_stat <- 2 * (as.numeric(stats::logLik(tsfit))
                       - as.numeric(stats::logLik(pofit)))
      lr_df <- tsfit$df.null - tsfit$df.residual -
        (pofit$df.null - pofit$df.residual)
      if (is.finite(lr_stat) && lr_df > 0L) {
        lr_p <- stats::pchisq(lr_stat, lr_df, lower.tail = FALSE)
      }
    }
    list(
      stratum = stratum_label,
      thresh_labels = thresh_labels,
      coef = coef_mat,
      se = se_mat,
      n = length(y_int),
      converged = all(vapply(pofits, function(f) f$converged, logical(1))),
      lr_stat = lr_stat, lr_df = lr_df, lr_p = lr_p
    )
  }

  if (split_by_gender) {
    strata <- sort(unique(stats::na.omit(df[[gender_col]])))
    for (g in strata) {
      sub <- df[!is.na(df[[gender_col]]) & df[[gender_col]] == g, , drop = FALSE]
      if (nrow(sub) < 30L) {
        warning(sprintf(
          "gender stratum '%s' has only %d rows; skipping (need n>=30).",
          g, nrow(sub)))
        next
      }
      r <- fit_one(sub, as.character(g))
      results[[as.character(g)]] <- r
      for (k in seq_along(r$thresh_labels)) {
        for (rc in race_cols) {
          per_thresh[[paste(as.character(g), r$thresh_labels[k], rc,
                             sep = "::")]] <- r$coef[k, rc]
        }
      }
      if (is.finite(r$lr_p) && (is.na(worst_p) || r$lr_p < worst_p)) {
        worst_p <- r$lr_p
        worst_stat <- r$lr_stat
        worst_df_v <- r$lr_df
      }
    }
  } else {
    # One-hot gender, fold into covariates
    g_fac <- factor(df[[gender_col]])
    g_dum <- stats::model.matrix(~ g_fac)[, -1, drop = FALSE]
    colnames(g_dum) <- paste0(gender_col, "_", levels(g_fac)[-1])
    df_aug <- cbind(df, g_dum)
    r <- fit_one(df_aug, "pooled")
    # NB: fit_one in pooled mode pulls only race_cols + control_cols,
    # so widen control_cols first
    control_cols2 <- c(control_cols, colnames(g_dum))
    r2 <- (function() {
      saved_control_cols <- control_cols
      control_cols <<- control_cols2
      on.exit(control_cols <<- saved_control_cols, add = TRUE)
      fit_one(df_aug, "pooled")
    })()
    results[["pooled"]] <- r2
    for (k in seq_along(r2$thresh_labels)) {
      for (rc in race_cols) {
        per_thresh[[paste("pooled", r2$thresh_labels[k], rc,
                           sep = "::")]] <- r2$coef[k, rc]
      }
    }
    worst_p <- r2$lr_p
    worst_stat <- r2$lr_stat
    worst_df_v <- r2$lr_df
  }

  # Build interpretation
  lines <- character(0)
  for (s in names(results)) {
    r <- results[[s]]
    for (rc in race_cols) {
      b1 <- r$coef[1, rc]
      b2 <- r$coef[2, rc]
      if (is.finite(b1) && is.finite(b2) && abs(b2) > 1e-6) {
        ratio <- abs(b1) / abs(b2)
        if (ratio >= 1.5) {
          lines <- c(lines, sprintf(
            "[%s] %s: bias concentrated at low->medium cutoff (beta1=%+.3f vs beta2=%+.3f; ratio=%.2f).",
            s, rc, b1, b2, ratio))
        }
      }
    }
  }
  if (length(lines) == 0L) {
    lines <- "No stratum/race combination showed |beta_low->med| / |beta_med->high| >= 1.5."
  }
  po_text <- if (is.finite(worst_p)) {
    sprintf("Proportional-odds LR test (worst stratum): chi2=%.3f on %d df, p=%.4g.",
            worst_stat, worst_df_v, worst_p)
  } else {
    "Proportional-odds LR test not computable."
  }
  caveat <- paste(
    "CAVEAT: this reports OUTPUT disparity, not predictive-validity",
    "disparity (Goel et al. 2021)."
  )

  n_obs <- sum(vapply(results, function(r) r$n, integer(1)))
  .lan_ard_result(
    "Laniyonu Actuarial Risk Disparity (Stage 1, ordinal)",
    sprintf("morie_laniyonu_actuarial_risk_disparity(outcome=%s, kind=ordinal)",
            outcome_col),
    interpretation = paste(c(lines, po_text, caveat), collapse = " "),
    outcome_col = outcome_col,
    outcome_kind = "ordinal",
    n_obs = n_obs,
    race_cols = race_cols,
    gender_col = gender_col,
    ordinal_result = results,
    per_threshold_logodds = per_thresh,
    proportional_odds_lr_stat = worst_stat,
    proportional_odds_lr_df = worst_df_v,
    proportional_odds_p = worst_p
  )
}


# ---------------------------------------------------------------------------
# Stage 2 — score-net-residual logit
# ---------------------------------------------------------------------------

.lan_score_net_residual <- function(sub, score_col, outcome_col,
                                     race_cols, control_cols,
                                     bootstrap_replicates,
                                     random_state) {
  cov_cols <- c(score_col, race_cols, control_cols)
  df_fit <- sub[, c(outcome_col, cov_cols), drop = FALSE]
  df_fit <- df_fit[stats::complete.cases(df_fit), , drop = FALSE]
  if (nrow(df_fit) < 30L) {
    return(NULL)
  }
  fm <- as.formula(paste0("`", outcome_col, "` ~ ",
                          paste0("`", cov_cols, "`", collapse = " + ")))
  fit <- tryCatch(
    suppressWarnings(stats::glm(fm, data = df_fit,
                                  family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit) || !fit$converged) {
    return(NULL)
  }
  cf <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  # Bootstrap residual race SEs (optional)
  boot_se <- rep(NA_real_, length(race_cols))
  names(boot_se) <- race_cols
  if (bootstrap_replicates > 0L) {
    set.seed(random_state)
    n <- nrow(df_fit)
    draws <- matrix(NA_real_, nrow = bootstrap_replicates,
                    ncol = length(race_cols),
                    dimnames = list(NULL, race_cols))
    for (b in seq_len(bootstrap_replicates)) {
      idx <- sample.int(n, n, replace = TRUE)
      fit_b <- tryCatch(
        suppressWarnings(stats::glm(fm, data = df_fit[idx, , drop = FALSE],
                                      family = stats::binomial())),
        error = function(e) NULL
      )
      if (!is.null(fit_b) && fit_b$converged) {
        cb <- stats::coef(fit_b)
        for (rc in race_cols) {
          if (rc %in% names(cb)) draws[b, rc] <- cb[rc]
        }
      }
    }
    boot_se <- apply(draws, 2L, function(v) stats::sd(v, na.rm = TRUE))
  }
  list(
    coefficients = cf,
    std_errors = se,
    bootstrap_se = boot_se,
    score_coefficient = unname(cf[score_col]),
    n_obs = nrow(df_fit)
  )
}


.lan_run_residual <- function(df, outcome, outcome_col, score_col,
                               race_cols, gender_col, control_cols,
                               split_by_gender, bootstrap_replicates,
                               random_state) {
  combined_coef <- list()
  combined_se <- list()
  score_coefs <- numeric(0)
  n_total <- 0L

  if (split_by_gender) {
    strata <- sort(unique(stats::na.omit(df[[gender_col]])))
    for (g in strata) {
      sub <- df[!is.na(df[[gender_col]]) & df[[gender_col]] == g, , drop = FALSE]
      if (nrow(sub) < 30L) {
        warning(sprintf(
          "gender stratum '%s' has only %d rows; skipping.", g, nrow(sub)))
        next
      }
      r <- .lan_score_net_residual(sub, score_col, outcome_col, race_cols,
                                    control_cols, bootstrap_replicates,
                                    random_state)
      if (is.null(r)) next
      for (rc in race_cols) {
        key <- paste(rc, g, sep = "::")
        combined_coef[[key]] <- unname(r$coefficients[rc])
        combined_se[[key]] <- unname(r$std_errors[rc])
      }
      score_coefs <- c(score_coefs, r$score_coefficient)
      n_total <- n_total + r$n_obs
    }
  } else {
    g_fac <- factor(df[[gender_col]])
    g_dum <- stats::model.matrix(~ g_fac)[, -1, drop = FALSE]
    colnames(g_dum) <- paste0(gender_col, "_", levels(g_fac)[-1])
    df_aug <- cbind(df, g_dum)
    r <- .lan_score_net_residual(df_aug, score_col, outcome_col,
                                  c(race_cols, colnames(g_dum)),
                                  control_cols, bootstrap_replicates,
                                  random_state)
    if (!is.null(r)) {
      for (rc in c(race_cols, colnames(g_dum))) {
        combined_coef[[rc]] <- unname(r$coefficients[rc])
        combined_se[[rc]] <- unname(r$std_errors[rc])
      }
      score_coefs <- r$score_coefficient
      n_total <- r$n_obs
    }
  }

  parts <- vapply(names(combined_coef), function(k) {
    sprintf("%s=%+.4f", k, combined_coef[[k]])
  }, character(1))
  interp <- paste(
    "Stage 2 (score-net-residual): coefficient on each race indicator",
    "NET of actuarial score is the disparate-treatment signal.",
    "Per-stratum residual race coefficients:",
    paste(parts, collapse = ", "),
    sprintf("Mean score coefficient across strata: %+.4f.",
            mean(score_coefs, na.rm = TRUE)),
    "Paper headline: Black & Indigenous men with the best reintegration",
    "score are still -11pp on parole vs. White; Black women -22pp.",
    "CAVEAT: OUTPUT disparity, not predictive-validity disparity",
    "(Goel et al. 2021)."
  )

  .lan_ard_result(
    "Laniyonu Actuarial Risk Disparity (Stage 2, binary)",
    sprintf("morie_laniyonu_actuarial_risk_disparity(outcome=%s, kind=binary)",
            outcome_col),
    interpretation = interp,
    outcome = outcome,
    outcome_col = outcome_col,
    outcome_kind = "binary",
    n_obs = n_total,
    race_cols = race_cols,
    gender_col = gender_col,
    residual_coefficients = combined_coef,
    residual_std_errors = combined_se,
    score_coefficient_mean = mean(score_coefs, na.rm = TRUE),
    bootstrap_replicates = bootstrap_replicates
  )
}


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

#' Audit CSC actuarial risk and downstream outcomes for race x gender bias
#'
#' Pass one of the four ordinal risk scores (\code{"static"},
#' \code{"dynamic"}, \code{"osl"}, \code{"reintegration"}) to run the
#' Stage 1 threshold-specific ordinal logit; pass \code{"parole"} or
#' \code{"housing"} to run the Stage 2 score-net-residual binary logit.
#'
#' @param df Sentence-level (one row per sentence) CSC microdata.
#' @param outcome One of \code{"static"}, \code{"dynamic"}, \code{"osl"},
#'   \code{"reintegration"}, \code{"parole"}, \code{"housing"}.
#' @param race_cols Character vector of 0/1 race indicator columns
#'   (White is the implicit reference; pass non-reference levels).
#' @param gender_col Categorical gender column.
#' @param score_col Required when \code{outcome} is \code{"parole"} or
#'   \code{"housing"}; the actuarial score column name (e.g.
#'   \code{"reintegration_score_numeric"} for parole).
#' @param control_cols Optional additional control columns (age, priors,
#'   sentence length, etc.).  Pre-dummy any categoricals.
#' @param ordinal_levels Level ordering for ordinal outcomes
#'   (default \code{c("low", "medium", "high")}).
#' @param outcome_col Optional override for the default column name.
#' @param split_by_gender If \code{TRUE} (default), stratifies by
#'   \code{gender_col}; if \code{FALSE}, folds one-hot gender into the
#'   design.
#' @param bootstrap_replicates Stage 2 only; bootstrap reps for residual
#'   race-coefficient SEs.
#' @param random_state Seed for bootstrap.
#'
#' @return A named \code{list} of class \code{morie_laniyonu_ard_result}.
#' @export
morie_laniyonu_actuarial_risk_disparity <- function(
  df,
  outcome,
  race_cols,
  gender_col = "gender",
  score_col = NULL,
  control_cols = NULL,
  ordinal_levels = c("low", "medium", "high"),
  outcome_col = NULL,
  split_by_gender = TRUE,
  bootstrap_replicates = 200L,
  random_state = 20260513L
) {
  stopifnot(is.data.frame(df), is.character(outcome), length(outcome) == 1L)
  if (is.null(control_cols)) control_cols <- character(0)

  # Mandatory output-vs-predictive caveat on every call.
  warning(paste(
    "morie_laniyonu_actuarial_risk_disparity reports OUTPUT disparity,",
    "not predictive-validity disparity.  A non-zero residual race",
    "coefficient could reflect disparate treatment OR a valid race",
    "difference in the unobserved outcome - see Goel et al. (2021)."
  ), call. = FALSE)

  if (outcome %in% names(.lan_ard_ordinal)) {
    kind <- "ordinal"
    col <- if (is.null(outcome_col)) .lan_ard_ordinal[[outcome]] else outcome_col
  } else if (outcome %in% names(.lan_ard_binary)) {
    kind <- "binary"
    col <- if (is.null(outcome_col)) .lan_ard_binary[[outcome]] else outcome_col
  } else {
    stop("unknown outcome '", outcome, "'; expected one of ",
         paste(c(names(.lan_ard_ordinal), names(.lan_ard_binary)),
               collapse = ", "))
  }

  needed <- c(col, gender_col, race_cols, control_cols)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0L) {
    stop("columns missing from df: ", paste(missing_cols, collapse = ", "))
  }

  if (kind == "ordinal") {
    if (!is.null(score_col)) {
      warning("score_col is ignored for ordinal outcomes; the score IS the outcome.",
              call. = FALSE)
    }
    return(.lan_run_ordinal(df, col, race_cols, gender_col, control_cols,
                             ordinal_levels, split_by_gender))
  }

  if (is.null(score_col)) {
    stop("score_col is required for outcome='", outcome, "' ",
         "(pass e.g. 'reintegration_score_numeric' for parole, ",
         "'osl_score_numeric' for housing).")
  }
  .lan_run_residual(df, outcome, col, score_col, race_cols, gender_col,
                     control_cols, split_by_gender, bootstrap_replicates,
                     random_state)
}


#' @export
print.morie_laniyonu_ard_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  cat("Call:", x$call, "\
\
", sep = " ")
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  cat(x$interpretation, "\
")
  invisible(x)
}
