# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Goodman-Bacon three-way composition of the TWFE DiD (Gbtcom).
# Mirror of morie.fn.gbtcom: collapse the full Bacon decomposition to
# its three comparison types. Anchored on the test-did-parity.R hand
# fixture: type weights 0.7 / 0.1 / 0.2, mean 2x2 estimates
# 1.375/0.7, 1.25, 0.25, contributions summing to the TWFE 1.55.

#' Goodman-Bacon three-way composition of the TWFE DiD coefficient
#'
#' Collapses the Goodman-Bacon (2021, Theorem 1, eqs. 7-9)
#' decomposition to its three comparison types: a timing cohort
#' against never-treated units, an earlier cohort against a later one
#' before the later adopts, and a later cohort against an
#' already-treated earlier one -- the forbidden comparison whose
#' weight is how a TWFE coefficient can land below every clean
#' comparison when effects grow over time. Reports total weight,
#' weighted-average 2x2 estimate and contribution per type; the three
#' contributions sum back to the TWFE coefficient and the weights sum
#' to 1, both computed and reported rather than assumed.
#'
#' Classification rule (identical to the Python arm): a component with
#' an infinite control cohort is treated-vs-never; otherwise a treated
#' cohort adopting before its control cohort is early-vs-late, and one
#' adopting after its (already treated) control cohort is
#' late-vs-early.
#'
#' @param data Long-format balanced panel data frame.
#' @param outcome Name of the outcome column.
#' @param treatment Name of the absorbing 0/1 treatment column.
#' @param unit Name of the unit identifier column.
#' @param time Name of the period column.
#' @return List with \code{estimate} (TWFE coefficient),
#'   \code{weight}, \code{mean_beta} and \code{contribution} (named
#'   vectors over the three types), \code{weight_sum},
#'   \code{identity_residual}, \code{forbidden_weight} and
#'   \code{n_components_by_type}.
#' @references Goodman-Bacon, A. (2021). Difference-in-differences
#'   with variation in treatment timing. Journal of Econometrics,
#'   225(2), 254-277. doi:10.1016/j.jeconom.2021.03.014. Implemented
#'   from Theorem 1 and eqs. (7)-(9) of NBER Working Paper 25018;
#'   local source: WD_BLACK library/pdf/fetched-wave3/
#'   goodman-bacon-2021-did-variation-treatment-timing.pdf.
#' @examples
#' u <- rep(1:9, each = 8L)
#' tt <- rep(1:8, times = 9L)
#' g <- rep(c(3, 3, 3, 5, 5, 5, Inf, Inf, Inf), each = 8L)
#' rel <- ifelse(is.finite(g), pmax(0, tt - g), 0)
#' y <- u * 0.3 + tt * 0.2 + ifelse(tt >= g, 1 + 0.5 * rel, 0)
#' d <- data.frame(y = y, d = as.numeric(tt >= g), unit = u, time = tt)
#' out <- Gbtcom(d, "y", "d", "unit", "time")
#' round(out$weight[["treated vs never-treated"]], 10)
#' @export
Gbtcom <- function(data, outcome, treatment, unit, time) {
  dec <- morie_did_bacon_decomposition(data, outcome, treatment,
                                       unit, time)
  cmp <- dec$components
  types <- c("treated vs never-treated",
             "early vs late (before late adopts)",
             "late vs early (early already treated)")
  cls <- ifelse(!is.finite(cmp$untreated), types[1L],
                ifelse(cmp$treated < cmp$untreated, types[2L], types[3L]))
  w <- vapply(types, function(tp) sum(cmp$weight[cls == tp]), 0)
  wb <- vapply(types, function(tp)
    sum(cmp$weight[cls == tp] * cmp$estimate[cls == tp]), 0)
  mean_beta <- ifelse(w > 0, wb / w, NA_real_)
  names(w) <- names(wb) <- names(mean_beta) <- types
  n_by <- vapply(types, function(tp) sum(cls == tp), 0L)
  # independently computed TWFE coefficient, so identity_residual is a
  # real check (dec$overall_estimate is already the recomposition)
  beta <- as.numeric(morie_did_panel_fe(data, outcome, treatment,
                                        unit, time)$estimate)
  list(
    estimate = beta,
    weight = as.list(w),
    mean_beta = as.list(mean_beta),
    contribution = as.list(wb),
    weight_sum = sum(w),
    identity_residual = beta - sum(wb),
    forbidden_weight = unname(w[[types[3L]]]),
    n_components_by_type = as.list(n_by),
    method = paste("Goodman-Bacon (2021) three-way composition of the",
                   "TWFE DiD coefficient")
  )
}

#' Goodman-Bacon decomposition, vector interface
#'
#' The signature-compatible mirror of Python \code{morie.fn.gbtcom},
#' which takes the four columns as vectors. \code{Gbtcom} keeps the
#' data-frame form for R callers who already hold a panel.
#'
#' @param y Outcome, one entry per unit-period.
#' @param D Treatment indicator, same length.
#' @param unit Unit identifier, same length.
#' @param time Period identifier, same length.
#' @return As \code{Gbtcom}.
#' @export
morie_gbtcom <- function(y, D, unit, time) {
  n <- length(y)
  if (length(D) != n || length(unit) != n || length(time) != n)
    stop("gbtcom: y, D, unit and time must have the same length")
  df <- data.frame(y = as.numeric(y), D = as.numeric(D),
                   unit = unit, time = time, stringsAsFactors = FALSE)
  Gbtcom(df, "y", "D", "unit", "time")
}
