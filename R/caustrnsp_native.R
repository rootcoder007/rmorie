# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Weighted generalization/transport of a trial effect (Caustrnsp).
# Bit-identical mirror of src/morie/fn/caustrnsp.py.

#' Generalize or transport a randomized-trial effect by weighting
#'
#' Weighting estimator of the population average treatment effect from
#' trial data with estimated sampling scores s(x): the weighted
#' difference of arm means with weights normalised within each arm
#' (Tipton and Hartman 2023, Eq. 3.10). For generalizability the
#' weights are inverse sampling probabilities, w = 1/s (Eq. 3.11); for
#' transport to a disjoint target population they are the odds
#' w = (1 - s)/s divided by Pr(W = 0) (Eq. 3.12; the constant cancels
#' in the ratio form). Identification of transported effects is due to
#' Pearl and Bareinboim (2014).
#'
#' @param y Outcomes of the trial units.
#' @param z Randomized binary treatment, 0/1.
#' @param s Estimated sampling scores, strictly inside (0, 1).
#' @param mode Either "transport" (odds weights) or "generalize"
#'   (inverse probability weights).
#' @param pr_w0 Optional Pr(W = 0) for the unnormalised odds weights;
#'   affects only the reported weights, not the estimate.
#' @return List with \code{estimate}, \code{mean_treated},
#'   \code{mean_control}, \code{weights}, \code{n}, \code{n_treat},
#'   \code{n_control}, \code{mode}, \code{method}.
#' @references Tipton, E. and Hartman, E. (2023), Generalizability and
#'   transportability, Ch. 3 Eqs. 3.10-3.12 in Zubizarreta, Stuart,
#'   Small and Rosenbaum (eds), Handbook of Matching and Weighting
#'   Adjustments for Causal Inference, Chapman and Hall/CRC,
#'   doi:10.1201/9781003102670 (local PDF, WD_BLACK library).
#'   Pearl, J. and Bareinboim, E. (2014), External validity: From
#'   do-calculus to transportability across populations, Statistical
#'   Science 29(4), 579-595, doi:10.1214/14-STS486; local copy
#'   fetched-wave3/pearl-bareinboim-2014-external-validity-transportability-StatSci29.pdf.
#' @export
Caustrnsp <- function(y, z, s, mode = "transport", pr_w0 = NULL) {
  yv <- as.numeric(y); zv <- as.numeric(z); sv <- as.numeric(s)
  n <- length(yv)
  if (length(zv) != n || length(sv) != n) {
    stop("y, z, s must have equal length", call. = FALSE)
  }
  if (!all(zv %in% c(0, 1))) stop("z must be binary 0/1", call. = FALSE)
  if (any(sv <= 0) || any(sv >= 1)) {
    stop("sampling scores must lie strictly in (0, 1)", call. = FALSE)
  }
  if (!mode %in% c("transport", "generalize")) {
    stop("mode must be transport or generalize", call. = FALSE)
  }
  if (mode == "generalize") {
    w <- 1 / sv
  } else {
    cc <- if (is.null(pr_w0)) 1 else as.numeric(pr_w0)
    if (!cc > 0) stop("pr_w0 must be positive", call. = FALSE)
    w <- (1 - sv) / sv / cc
  }
  i1 <- which(zv == 1); i0 <- which(zv == 0)
  if (length(i1) == 0L || length(i0) == 0L) {
    stop("need both treatment arms in the trial sample", call. = FALSE)
  }
  mu1 <- sum(w[i1] * yv[i1]) / sum(w[i1])
  mu0 <- sum(w[i0] * yv[i0]) / sum(w[i0])
  list(estimate = mu1 - mu0, mean_treated = mu1, mean_control = mu0,
       weights = w, n = n, n_treat = length(i1), n_control = length(i0),
       mode = mode,
       method = sprintf(
         "Tipton-Hartman Eq. 3.10 weighted PATE, %s weights", mode))
}
