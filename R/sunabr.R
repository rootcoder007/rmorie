# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sun-Abraham interaction-weighted difference in differences
#'
#' Formula: nu_g = (1/|g|) sum_\{l in g\} sum_e CATT(e,l) Pr\{E=e | E in \[-l, T-l\]\}
#'
#' @param y Outcome, one row per unit-period.
#' @param unit Unit identifier.
#' @param time Calendar period.
#' @param cohort First treated period for the row's unit.
#' @param never Value of ``cohort`` marking never-treated units.

#' @param y See Usage.
#' @param unit See Usage.
#' @param time See Usage.
#' @param cohort See Usage.
#' @param never See Usage.
#' @return List with ``event_time``, ``att``, ``overall``, ``cohorts``, ``n``.
#' @references Sun and Abraham (2021), Estimating dynamic treatment effects in event studies with heterogeneous treatment effects, Journal of Econometrics 225(3):175-199. Equations (26) and (28) for the estimand and the IW estimator, Section 4.2 for the DID choice of pre-period and control cohort. Verified against the paper.
#' @export
#' @examples
#' Iwdid(y = c(1, 2, 3, 4, 5, 6, 7, 8), unit = c(1, 2, 3, 4, 5, 6, 7, 8), time = c(1, 2, 3, 4, 5, 6, 7, 8), cohort = c(1, 2, 3, 4, 5, 6, 7, 8))
Iwdid <- function(y, unit, time, cohort, never = 0) {
  y <- .t1_vec(y); time <- as.numeric(time); cohort <- as.numeric(cohort)
  n <- length(y); nev <- as.numeric(never)
  cellmean <- tapply(y, paste(cohort, time, sep = "|"), mean)
  gm <- function(g, t) {
    k <- paste(g, t, sep = "|")
    if (k %in% names(cellmean)) cellmean[[k]] else NA_real_
  }
  treated <- sort(unique(cohort[cohort != nev]))
  size <- vapply(treated, function(g) length(unique(unit[cohort == g])), numeric(1))
  evs <- sort(unique(as.numeric(outer(sort(unique(time)), treated, "-"))))
  ev_out <- numeric(0); att_out <- numeric(0)
  for (e in evs) {
    num <- 0; den <- 0
    for (j in seq_along(treated)) {
      g <- treated[j]
      v <- c(gm(g, g + e), gm(g, g - 1), gm(nev, g + e), gm(nev, g - 1))
      if (!any(is.na(v))) {
        num <- num + size[j] * ((v[1] - v[2]) - (v[3] - v[4]))
        den <- den + size[j]
      }
    }
    if (den > 0) { ev_out <- c(ev_out, e); att_out <- c(att_out, num / den) }
  }
  post <- att_out[ev_out >= 0]
  .t1_result(event_time = ev_out, att = att_out,
             overall = if (length(post)) mean(post) else NA_real_,
             cohorts = treated, n = n,
             method = "Sun-Abraham interaction-weighted DID")
}
