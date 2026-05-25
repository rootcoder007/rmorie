# SPDX-License-Identifier: AGPL-3.0-or-later

#' Replication of Laniyonu (2018) — Coffee Shops and Street Stops
#'
#' R port of \code{morie.laniyonu.gentrification_policing}.  Estimates
#' the direct, indirect (spatial spillover), and total effect of
#' gentrification on NYPD stop-and-frisk rates at the census-tract x
#' year level via a Spatial Durbin Model (SDM) decomposition.
#'
#' The paper's headline finding: gentrification has roughly zero
#' \emph{direct} effect on stops/capita inside the gentrifying tract,
#' but a +51\% to +90\% \emph{indirect} (spillover) effect on
#' stops/capita in neighbouring tracts.
#'
#' Two modes are supported:
#' \itemize{
#'   \item \strong{Pre-fitted mode (preferred)}: pass \code{fitted_rho},
#'     \code{fitted_beta_direct}, \code{fitted_beta_spatial} from your
#'     own SDM fit (e.g.\ \pkg{spatialreg::lagsarlm} with Durbin terms).
#'     This wrapper handles the diagnostic ladder + Kelejian-Prucha
#'     spillover decomposition.
#'   \item \strong{Lite mode (fall-back)}: OLS + Moran's I on
#'     residuals, decomposition with rho=0.  Useful for sanity-checks
#'     only.
#' }
#'
#' @references
#' Laniyonu, A. (2018).  Coffee shops and street stops: Policing
#'   practices in gentrifying neighborhoods.  Urban Affairs Review,
#'   54(5), 898-930.
#'
#' LeSage, J. P., & Pace, R. K. (2009).  Introduction to Spatial
#'   Econometrics.  CRC Press.
#'
#' @return A \code{list} of class \code{morie_laniyonu_gp_result}, one
#'   per year analysed.
#' @name morie_laniyonu_gentrification_policing
NULL


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.lan_gp_result <- function(year, n_tracts, rho, moran_i_ols,
                            decompositions, gent_distribution,
                            sensitivity_thresholds = list(),
                            note = "") {
  gent_idx <- which(vapply(decompositions, function(d) {
    grepl("^gent", d$coefficient %||% "")
  }, logical(1)))
  gent_d <- if (length(gent_idx) > 0L) decompositions[[gent_idx[1]]] else NULL

  head <- sprintf(
    "Year %s, N=%d tracts, rho=%+.4f, Moran's I (OLS residuals)=%+.4f. ",
    as.character(year), n_tracts, rho, moran_i_ols
  )
  gent_line <- if (!is.null(gent_d)) {
    sprintf("Gentrification effect: direct=%+.4f, indirect=%+.4f, total=%+.4f.",
            gent_d$direct, gent_d$indirect, gent_d$total)
  } else {
    "Gentrification coefficient not found in decomposition."
  }

  out <- list(
    title = sprintf("Laniyonu Gentrification x Policing SDM (year %s)",
                     as.character(year)),
    call = sprintf("morie_laniyonu_gentrification_policing(year=%s)",
                    as.character(year)),
    year = year,
    n_tracts = n_tracts,
    rho = rho,
    moran_i_ols = moran_i_ols,
    decompositions = decompositions,
    gentrification_distribution = gent_distribution,
    sensitivity_thresholds = sensitivity_thresholds,
    note = note,
    interpretation = paste(head, gent_line)
  )
  class(out) <- c("morie_laniyonu_gp_result", "morie_rich_result", "list")
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a


.lan_gp_placeholder_W <- function(crime_arr, k = 4L) {
  n <- length(crime_arr)
  if (n < 2L) return(matrix(0, n, n))
  k <- min(as.integer(k), n - 1L)
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    d <- abs(crime_arr - crime_arr[i])
    d[i] <- Inf
    nn <- order(d)[seq_len(k)]
    W[i, nn] <- 1.0
  }
  rs <- rowSums(W)
  rs[rs == 0] <- 1
  W / rs
}


.lan_gp_morans_i <- function(resid, W) {
  n <- length(resid)
  if (n < 2L || !all(dim(W) == n)) return(NA_real_)
  z <- resid - mean(resid)
  s0 <- sum(W)
  if (s0 == 0 || sum(z * z) == 0) return(NA_real_)
  (n / s0) * (sum(z * (W %*% z))) / sum(z * z)
}


# ---------------------------------------------------------------------------
# Gentrification panel — baseline-conditional 3-level factor
# ---------------------------------------------------------------------------

.lan_gent_panel <- function(baseline_frame, baseline_income_col,
                             baseline_rent_col, growth_college_col,
                             growth_rent_col,
                             income_quantile = 0.40,
                             rent_quantile = 0.40,
                             growth_quantile = 0.66) {
  inc <- baseline_frame[[baseline_income_col]]
  rent <- baseline_frame[[baseline_rent_col]]
  gc <- baseline_frame[[growth_college_col]]
  gr <- baseline_frame[[growth_rent_col]]

  inc_q <- stats::quantile(inc, income_quantile, na.rm = TRUE)
  rent_q <- stats::quantile(rent, rent_quantile, na.rm = TRUE)
  gc_q <- stats::quantile(gc, growth_quantile, na.rm = TRUE)
  gr_q <- stats::quantile(gr, growth_quantile, na.rm = TRUE)

  eligible <- !is.na(inc) & !is.na(rent) & inc < inc_q & rent < rent_q
  gentrified <- eligible &
    !is.na(gc) & !is.na(gr) & gc > gc_q & gr > gr_q

  flag <- rep("ineligible", nrow(baseline_frame))
  flag[eligible] <- "eligible_no_change"
  flag[gentrified] <- "gentrified"
  flag <- factor(flag,
                  levels = c("ineligible", "eligible_no_change", "gentrified"))
  list(
    flag = flag,
    thresholds = list(
      income_baseline_q40 = unname(inc_q),
      rent_baseline_q40 = unname(rent_q),
      college_growth_q66 = unname(gc_q),
      rent_growth_q66 = unname(gr_q)
    )
  )
}


# ---------------------------------------------------------------------------
# SDM spillover decomposition
# ---------------------------------------------------------------------------
# Reuses (and forward-declares) the spatial-spillover primitive at
# mrm_primitives_spatial_spillover.R when available; falls back to an
# inline implementation otherwise.  For each coefficient k:
#   M = (I - rho * W)^{-1} * (I * beta_d[k] + W * beta_s[k])
#   direct   = mean(diag(M))
#   indirect = mean(rowSums(M) - diag(M))
#   total    = mean(rowSums(M))

.lan_sdm_decompose <- function(rho, beta_direct, beta_spatial, W,
                                coefficient_names) {
  if (exists("morie_spatial_spillover_decomposition",
             envir = asNamespace("morie"), inherits = FALSE)) {
    return(morie_spatial_spillover_decomposition(
      rho = rho, beta_direct = beta_direct, beta_spatial = beta_spatial,
      W = W, coefficient_names = coefficient_names
    ))
  }
  n <- nrow(W)
  inv <- tryCatch(solve(diag(n) - rho * W), error = function(e) NULL)
  if (is.null(inv)) {
    return(lapply(seq_along(coefficient_names), function(k) {
      list(coefficient = coefficient_names[k],
           direct = NA_real_, indirect = NA_real_, total = NA_real_)
    }))
  }
  lapply(seq_along(coefficient_names), function(k) {
    M <- inv %*% (diag(n) * beta_direct[k] + W * beta_spatial[k])
    direct <- mean(diag(M))
    total <- mean(rowSums(M))
    list(coefficient = coefficient_names[k],
         direct = direct,
         indirect = total - direct,
         total = total)
  })
}


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

#' Replicate Laniyonu (2018) SDM by year and return per-year decompositions
#'
#' @param df Tract-year panel.  One row per tract per year.
#' @param year_col,tract_id_col,stops_col,population_col,crime_col,demand_col
#'   Column names; defaults match the morie toy bundle schema.
#' @param baseline_income_col,baseline_rent_col Baseline-period income
#'   and rent (2000 in the paper).
#' @param growth_college_col,growth_rent_col Growth columns.  If
#'   \code{NULL}, computed from follow-minus-baseline.
#' @param follow_income_col,follow_rent_col,baseline_college_col,follow_college_col
#'   Used when growth columns are not pre-computed.
#' @param additional_controls Extra tract-year controls (pct_black, etc.).
#' @param weight_matrix Pre-computed (N, N) row-standardised spatial
#'   weights.  Required when \code{fitted_*} mode is in use.
#' @param weight_matrix_kind Provenance label only.
#' @param fitted_rho,fitted_beta_direct,fitted_beta_spatial Pre-fitted
#'   SDM outputs.  Pass these to bypass lite-mode.
#' @param years Subset of years to analyse.
#' @param log_outcome If \code{TRUE} (default), outcome is
#'   \code{log(stops / population)}.
#'
#' @return A \code{list} of \code{morie_laniyonu_gp_result}, one per
#'   year analysed.
#' @export
morie_laniyonu_gentrification_policing <- function(
  df,
  year_col = "year",
  tract_id_col = "tract_id",
  stops_col = "stops",
  population_col = "population",
  crime_col = "felony_count",
  demand_col = "calls_311_omp",
  baseline_income_col = "median_inc_2000",
  baseline_rent_col = "median_rent_2000",
  growth_college_col = NULL,
  growth_rent_col = NULL,
  follow_income_col = "median_inc_2014",
  follow_rent_col = "median_rent_2014",
  baseline_college_col = "pct_ba_2000",
  follow_college_col = "pct_ba_2014",
  additional_controls = NULL,
  weight_matrix = NULL,
  weight_matrix_kind = c("queen", "knn"),
  fitted_rho = NULL,
  fitted_beta_direct = NULL,
  fitted_beta_spatial = NULL,
  years = NULL,
  log_outcome = TRUE
) {
  stopifnot(is.data.frame(df))
  weight_matrix_kind <- match.arg(weight_matrix_kind)

  df <- as.data.frame(df)
  if (is.null(growth_college_col)) {
    df$.growth_college <- df[[follow_college_col]] - df[[baseline_college_col]]
    growth_college_col <- ".growth_college"
  }
  if (is.null(growth_rent_col)) {
    df$.growth_rent <- df[[follow_rent_col]] - df[[baseline_rent_col]]
    growth_rent_col <- ".growth_rent"
  }

  # Build baseline frame (one row per tract) for the gentrification flag.
  baseline_frame <- df[!duplicated(df[[tract_id_col]]), , drop = FALSE]
  rownames(baseline_frame) <- baseline_frame[[tract_id_col]]
  gp <- .lan_gent_panel(
    baseline_frame,
    baseline_income_col = baseline_income_col,
    baseline_rent_col = baseline_rent_col,
    growth_college_col = growth_college_col,
    growth_rent_col = growth_rent_col
  )
  gent_flag <- setNames(as.character(gp$flag), baseline_frame[[tract_id_col]])
  df$gentrification <- factor(
    gent_flag[as.character(df[[tract_id_col]])],
    levels = c("ineligible", "eligible_no_change", "gentrified")
  )

  if (is.null(years)) {
    years <- sort(unique(stats::na.omit(df[[year_col]])))
  }

  out <- list()
  for (yr in years) {
    yr_df <- df[df[[year_col]] == yr, , drop = FALSE]
    n <- nrow(yr_df)
    if (n < 10L) {
      warning(sprintf(
        "morie_laniyonu_gentrification_policing: year %s has only %d tracts; skipping.",
        as.character(yr), n))
      next
    }

    # Design matrix
    gent_dum <- stats::model.matrix(~ gentrification, data = yr_df)[, -1, drop = FALSE]
    colnames(gent_dum) <- paste0("gent_", levels(yr_df$gentrification)[-1])
    extras <- if (!is.null(additional_controls)) {
      as.matrix(yr_df[, additional_controls, drop = FALSE])
    } else {
      matrix(0, nrow = n, ncol = 0L)
    }
    X <- cbind(gent_dum,
                stats::setNames(as.matrix(yr_df[, c(crime_col, demand_col), drop = FALSE]),
                                 c(crime_col, demand_col)),
                extras)
    X_cols <- colnames(X)

    rate <- yr_df[[stops_col]] / pmax(yr_df[[population_col]], 1)
    y <- if (log_outcome) log(pmax(rate, 1e-10)) else rate

    # OLS for diagnostic
    X_int <- cbind(Intercept = 1, X)
    beta_ols <- tryCatch(
      drop(solve(crossprod(X_int), crossprod(X_int, y))),
      error = function(e) rep(0, ncol(X_int))
    )
    residuals <- y - drop(X_int %*% beta_ols)

    # Weight matrix
    if (is.null(weight_matrix)) {
      W <- .lan_gp_placeholder_W(yr_df[[crime_col]])
      note <- paste("Using placeholder W from felony-count proximity;",
                    "pass weight_matrix= for paper-grade results.")
    } else {
      W <- weight_matrix
      note <- sprintf("Using user-supplied W (kind=%s).", weight_matrix_kind)
    }

    mi <- tryCatch(.lan_gp_morans_i(residuals, W),
                    error = function(e) NA_real_)

    # SDM decomposition: pre-fitted or lite-mode
    if (!is.null(fitted_rho) && !is.null(fitted_beta_direct)
        && !is.null(fitted_beta_spatial)) {
      rho_yr <- if (is.list(fitted_rho)) fitted_rho[[as.character(yr)]] else as.numeric(fitted_rho)
      beta_d <- if (is.list(fitted_beta_direct))
        fitted_beta_direct[[as.character(yr)]] else fitted_beta_direct
      beta_s <- if (is.list(fitted_beta_spatial))
        fitted_beta_spatial[[as.character(yr)]] else fitted_beta_spatial
    } else {
      rho_yr <- 0.0
      beta_d <- beta_ols[-1]  # drop intercept
      beta_s <- rep(0, length(beta_d))
      note <- paste(note,
                    "(lite mode: rho=0; pass fitted_* for SDM decomposition.)")
    }

    decomps <- .lan_sdm_decompose(rho_yr, beta_d, beta_s, W, X_cols)

    gent_dist_yr <- as.list(table(yr_df$gentrification))

    out[[length(out) + 1L]] <- .lan_gp_result(
      year = yr,
      n_tracts = n,
      rho = as.numeric(rho_yr),
      moran_i_ols = as.numeric(mi),
      decompositions = decomps,
      gent_distribution = gent_dist_yr,
      sensitivity_thresholds = gp$thresholds,
      note = note
    )
  }
  out
}


#' @export
print.morie_laniyonu_gp_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  cat(x$interpretation, "\
")
  if (nzchar(x$note)) cat("Note: ", x$note, "\
", sep = "")
  invisible(x)
}
