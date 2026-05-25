# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-city temporal disparity audit
#'
#' R port of \code{morie.fairness.temporal}. Reimplements Barman &
#' Barman, \emph{Unmasking Algorithmic Bias in Predictive Policing}
#' (arXiv:2603.18987): the four disparity metrics - Disparate Impact
#' Ratio, Demographic Parity Gap, Gini coefficient, and Bias
#' Amplification Score - are computed for every (\code{city},
#' \code{period}) cell and assembled into a time series so that
#' \emph{temporal instability} and \emph{cross-city divergence}
#' become visible.
#'
#' Builds on the metrics in \code{\link{fairness_metrics}}.
#'
#' @name fairness_temporal
NULL


.morie_fairness_mean_finite <- function(xs) {
  finite <- xs[is.finite(xs)]
  if (length(finite) == 0L) NA_real_ else mean(finite)
}


#' Audit how disparity metrics move over time and across cities
#'
#' For every (\code{city}, \code{period}) cell the audit computes the
#' four disparity metrics, then aggregates per city - reporting the
#' mean of each metric, the count of periods with DIR > 1
#' (over-prediction periods), and the DIR temporal range (max - min)
#' which quantifies how unstable the metric is across the audited
#' window.
#'
#' The reference (privileged) group is inferred \strong{globally} from
#' the pooled data when not supplied, so every cell uses the same
#' reference.
#'
#' @param period Time-period label per record (e.g. \code{"2019-03"}).
#'   Sorted lexically for display, so ISO-style labels order correctly.
#' @param city City label per record.
#' @param y_pred Decision / assignment per record.
#' @param group Protected attribute per record.
#' @param privileged Reference group. If \code{NULL}, inferred
#'   globally from pooled data.
#' @param favorable Value of \code{y_pred} counted as favourable
#'   (default \code{1}).
#' @return A \code{morie_fairness_result}; headline value is the
#'   largest per-city DIR temporal range - the worst temporal
#'   instability found in the audited window.
#' @examples
#' period <- c(rep("p1", 10), rep("p2", 10))
#' city   <- rep("A", 20)
#' pred   <- rep(c(1,1,1,1,1,1,1,1,0,0), 2)
#' grp    <- rep(c(rep("X",5), rep("Y",5)), 2)
#' res <- morie_fairness_predpol_temporal_audit(
#'   period, city, pred, grp, privileged = "X"
#' )
#' res$payload$per_city$A$dir_range
#' @export
morie_fairness_predpol_temporal_audit <- function(period, city, y_pred, group,
                                                  privileged = NULL,
                                                  favorable = 1) {
  period <- as.vector(period)
  city <- as.vector(city)
  y_pred <- as.vector(y_pred)
  group <- as.vector(group)

  n <- length(period)
  if (!(n == length(city) && n == length(y_pred) && n == length(group))) {
    stop("period, city, y_pred and group must all align", call. = FALSE)
  }
  if (n == 0L) {
    stop("inputs are empty", call. = FALSE)
  }

  warnings <- character(0)

  if (is.null(privileged)) {
    groups_pooled <- .morie_fairness_ordered_unique(group)
    pooled_rates <- vapply(groups_pooled, function(g) {
      mean(y_pred[group == g] == favorable)
    }, numeric(1))
    privileged <- as.character(groups_pooled[which.max(pooled_rates)])
    warnings <- c(warnings, sprintf(
      "`privileged` not given; inferred globally as '%s' (highest pooled favourable-outcome rate) so every cell uses the same reference group.",
      privileged
    ))
  } else {
    privileged <- as.character(privileged)
  }

  cells <- list()
  skipped <- 0L
  for (c in .morie_fairness_ordered_unique(city)) {
    city_mask <- city == c
    periods_here <- sort(as.character(
      .morie_fairness_ordered_unique(period[city_mask])
    ))
    for (p in periods_here) {
      mask <- city_mask & (period == p)
      cg <- group[mask]
      cy <- y_pred[mask]
      cell_groups <- as.character(.morie_fairness_ordered_unique(cg))
      if (length(cell_groups) < 2L || !(privileged %in% cell_groups)) {
        skipped <- skipped + 1L
        next
      }
      di_res <- morie_fairness_disparate_impact(
        cy, cg, privileged = privileged, favorable = favorable
      )
      pg_res <- morie_fairness_demographic_parity(
        cy, cg, privileged = privileged, favorable = favorable
      )
      rate_vec <- vapply(cell_groups, function(g) {
        mean(cy[cg == g] == favorable)
      }, numeric(1))
      gini_res <- morie_fairness_gini(rate_vec)
      bas_res <- morie_fairness_bias_amplification(
        cy, cg, privileged = privileged, favorable = favorable
      )
      cells[[length(cells) + 1L]] <- list(
        city = as.character(c),
        period = as.character(p),
        n = as.integer(sum(mask)),
        dir = as.numeric(di_res$value),
        parity_gap = as.numeric(pg_res$value),
        gini = as.numeric(gini_res$value),
        bas = as.numeric(bas_res$value)
      )
    }
  }

  if (skipped > 0L) {
    warnings <- c(warnings, sprintf(
      "%d (city, period) cell(s) were skipped - fewer than two groups present, or the privileged group absent.",
      skipped
    ))
  }
  if (length(cells) == 0L) {
    stop("no (city, period) cell had enough groups to audit", call. = FALSE)
  }

  cities_seen <- unique(vapply(cells, function(x) x$city, character(1)))
  per_city <- list()
  for (c in cities_seen) {
    cc <- Filter(function(x) x$city == c, cells)
    dirs <- vapply(cc, function(x) x$dir, numeric(1))
    dirs <- dirs[is.finite(dirs)]
    per_city[[c]] <- list(
      n_periods = length(cc),
      mean_dir = .morie_fairness_mean_finite(vapply(cc, function(x) x$dir, numeric(1))),
      mean_parity_gap = .morie_fairness_mean_finite(
        vapply(cc, function(x) x$parity_gap, numeric(1))),
      mean_gini = .morie_fairness_mean_finite(
        vapply(cc, function(x) x$gini, numeric(1))),
      mean_bas = .morie_fairness_mean_finite(
        vapply(cc, function(x) x$bas, numeric(1))),
      dir_min = if (length(dirs) > 0L) min(dirs) else NA_real_,
      dir_max = if (length(dirs) > 0L) max(dirs) else NA_real_,
      dir_range = if (length(dirs) > 0L) max(dirs) - min(dirs) else NA_real_,
      periods_dir_gt1 = sum(dirs > 1.0)
    )
  }

  ranges <- vapply(per_city, function(v) v$dir_range, numeric(1))
  ranges <- ranges[is.finite(ranges)]
  worst_range <- if (length(ranges) > 0L) max(ranges) else NA_real_

  mean_dirs <- vapply(per_city, function(v) v$mean_dir, numeric(1))
  mean_dirs <- mean_dirs[is.finite(mean_dirs)]
  cross_city_spread <- if (length(mean_dirs) >= 2L) {
    max(mean_dirs) - min(mean_dirs)
  } else 0.0

  # Tables
  cell_rows <- lapply(cells, function(x) list(
    city = x$city, period = x$period, n = x$n,
    dir = round(x$dir, 4), parity_gap = round(x$parity_gap, 4),
    gini = round(x$gini, 4), bas = round(x$bas, 4)
  ))
  city_rows <- lapply(names(per_city), function(c) {
    v <- per_city[[c]]
    list(
      city = c, periods = v$n_periods,
      mean_dir = round(v$mean_dir, 4),
      mean_pg = round(v$mean_parity_gap, 4),
      mean_gini = round(v$mean_gini, 4),
      mean_bas = round(v$mean_bas, 4),
      dir_range = round(v$dir_range, 4),
      periods_dir_gt1 = v$periods_dir_gt1
    )
  })

  stab <- if (is.finite(worst_range) && worst_range >= 0.5) {
    sprintf(
      "Bias is temporally unstable: the Disparate Impact Ratio swings by up to %.3f across periods within a single city. A one-off audit at deployment time would not have caught this - the metric must be recomputed every period.",
      worst_range
    )
  } else {
    "The Disparate Impact Ratio is reasonably stable across periods within each city over the audited window."
  }
  div <- if (length(per_city) >= 2L && cross_city_spread >= 0.3) {
    sprintf(
      " Bias also diverges across cities: mean annual DIR spans %.3f between cities - the direction and size of bias is city-specific, not a fixed property of the system.",
      cross_city_spread
    )
  } else {
    ""
  }

  .morie_fairness_result(
    title = "Multi-City Temporal Disparity Audit",
    summary_lines = list(
      `Cells audited` = length(cells),
      `Cities` = length(per_city),
      `Worst DIR temporal range` = worst_range,
      `Cross-city mean-DIR spread` = cross_city_spread,
      `Reference group` = privileged
    ),
    sections = list(list(
      title = "Per-city aggregates:",
      headers = c("city", "periods", "mean DIR", "mean PG",
                  "mean Gini", "mean BAS", "DIR range", "M>1"),
      table = city_rows
    )),
    tables = list(list(
      title = "Per-(city, period) metrics:",
      headers = c("city", "period", "n", "DIR", "parity gap",
                  "Gini", "BAS"),
      rows = cell_rows
    )),
    warnings = warnings,
    interpretation = paste0(stab, div),
    payload = list(
      value = worst_range,
      worst_dir_range = worst_range,
      cross_city_dir_spread = cross_city_spread,
      per_city = per_city,
      cells = cells,
      privileged = privileged
    )
  )
}
