# SPDX-License-Identifier: AGPL-3.0-or-later
#' TPS Use-of-Force rate + type distribution
#'
#' Compact callable mirroring \code{morie.fn.tpsuof.tps_use_of_force}.
#' Computes a use-of-force rate over a known encounter denominator and
#' returns a per-type count distribution, packaged as a rich-result
#' list compatible with the \code{morie_mrm_uof_result} family in
#' \code{R/mrm_uof.R}.
#'
#' Formula
#' -------
#' \itemize{
#'   \item \code{rate = length(force_types) / n_encounters}
#'   \item \code{type_counts = table(force_types)}
#' }
#'
#' @param force_types Character vector of use-of-force-type labels
#'   (one row per use-of-force incident).
#' @param n_encounters Positive integer total number of police-public
#'   encounters in the denominator.
#' @return A named \code{list} with classes
#'   \code{morie_tps_use_of_force_result}, \code{morie_mrm_uof_result},
#'   \code{morie_rich_result}, \code{list}.  Slots: \code{rate},
#'   \code{n}, \code{population}, \code{type_counts}, \code{n_types},
#'   \code{interpretation}.
#' @examples
#' force_types <- c("Physical Control", "Physical Control", "CEW",
#'                  "Firearm", "OC Spray")
#' morie_tps_use_of_force(force_types, n_encounters = 1000L)
#' @export
morie_tps_use_of_force <- function(force_types, n_encounters) {
  if (!is.numeric(n_encounters) || length(n_encounters) != 1L ||
      n_encounters <= 0) {
    stop("n_encounters must be a positive scalar")
  }
  ft <- as.character(force_types)
  ft <- ft[!is.na(ft)]
  n_uof <- length(ft)
  rate <- n_uof / as.numeric(n_encounters)
  counts <- as.list(table(ft))
  n_types <- length(counts)

  rate_text <- if (n_encounters >= 100L) {
    sprintf("%.3f use-of-force incident(s) per encounter (%.2f per 100 encounters)",
            rate, 100 * rate)
  } else {
    sprintf("%.3f use-of-force incident(s) per encounter (small denominator: n_encounters = %d)",
            rate, as.integer(n_encounters))
  }

  warnings <- character(0)
  if (n_encounters < 30L) {
    warnings <- c(warnings,
      "n_encounters < 30: rate estimate is descriptive only; bootstrap a CI for inference.")
  }
  if (n_uof == 0L) {
    warnings <- c(warnings,
      "No use-of-force incidents supplied; type distribution is empty.")
  }

  top_type <- if (n_types > 0L) {
    sort_idx <- order(-unlist(counts))
    names(counts)[sort_idx[1]]
  } else "-"

  out <- list(
    title = "TPS use-of-force rate + type distribution",
    call = sprintf("morie_tps_use_of_force(<%d force_types>, n_encounters=%d)",
                   n_uof, as.integer(n_encounters)),
    summary_lines = list(
      `Use-of-force incidents` = n_uof,
      `Encounters (denominator)` = as.integer(n_encounters),
      `Rate per encounter` = round(rate, 6),
      `Distinct force types` = n_types,
      `Most common type` = top_type
    ),
    warnings = warnings,
    interpretation = sprintf(
      "Across %d encounter(s), %d use-of-force incident(s) were recorded (%s) spanning %d distinct force-type categor%s.",
      as.integer(n_encounters), n_uof, rate_text, n_types,
      if (n_types == 1L) "y" else "ies"
    ),
    name = "use_of_force_rate",
    rate = rate,
    n = n_uof,
    population = as.integer(n_encounters),
    type_counts = counts,
    n_types = n_types
  )
  class(out) <- c("morie_tps_use_of_force_result",
                  "morie_mrm_uof_result",
                  "morie_rich_result",
                  "list")
  out
}


#' @rdname morie_tps_use_of_force
#' @export
morie_tpsuof <- morie_tps_use_of_force
