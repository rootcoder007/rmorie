# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Generalizability SMD of sampling-score logits (Causgsw). Bit-identical
# mirror of src/morie/fn/causgsw.py.

#' Generalizability diagnostics from trial and target sampling scores
#'
#' For estimated trial-participation scores s(x) with logits
#' l(x) = logit(s(x)), returns the standardized mean difference of the
#' logits, \eqn{SMD = (\bar l_{sample} - \bar l_{target}) / sd}, with
#' sd the standard deviation of the logits in the target population
#' (Tipton and Hartman 2023, Eq. 3.7, crediting Stuart et al. 2011,
#' who propose the difference in mean sampling scores as the primary
#' similarity metric; the raw-scale difference is reported as well).
#' Positive values mean the sample sits higher on the
#' participation-score scale than the target population.
#'
#' @param s_sample Sampling scores of the trial sample, strictly in
#'   (0, 1).
#' @param s_target Sampling scores of the target population, strictly
#'   in (0, 1).
#' @return List with \code{estimate} (SMD of logits, sample minus
#'   target over the target logit sd), \code{smd_abs},
#'   \code{diff_means}, \code{mean_sample}, \code{mean_target},
#'   \code{n_sample}, \code{n_target}, \code{method}.
#' @references Tipton, E. and Hartman, E. (2023), Generalizability and
#'   transportability, Ch. 3 Eq. 3.7 in Zubizarreta, Stuart, Small and
#'   Rosenbaum (eds), Handbook of Matching and Weighting Adjustments
#'   for Causal Inference, Chapman and Hall/CRC,
#'   doi:10.1201/9781003102670 (local PDF, WD_BLACK library).
#'   Stuart, E. A., Cole, S. R., Bradshaw, C. P. and Leaf, P. J.
#'   (2011), The use of propensity scores to assess the
#'   generalizability of results from randomized trials, Journal of
#'   the Royal Statistical Society Series A 174(2), 369-386,
#'   doi:10.1111/j.1467-985x.2010.00673.x.
#' @export
Causgsw <- function(s_sample, s_target) {
  ss <- as.numeric(s_sample); st <- as.numeric(s_target)
  if (length(ss) < 1L || length(st) < 2L) {
    stop("need at least 1 sample and 2 target scores", call. = FALSE)
  }
  if (any(ss <= 0) || any(ss >= 1) || any(st <= 0) || any(st >= 1)) {
    stop("sampling scores must lie strictly in (0, 1)", call. = FALSE)
  }
  ls <- log(ss / (1 - ss))
  lt <- log(st / (1 - st))
  sdv <- stats::sd(lt)
  if (sdv == 0) stop("target-population logits are constant", call. = FALSE)
  smd <- (mean(ls) - mean(lt)) / sdv
  list(estimate = smd, smd_abs = abs(smd),
       diff_means = mean(ss) - mean(st),
       mean_sample = mean(ss), mean_target = mean(st),
       n_sample = length(ss), n_target = length(st),
       method = "Tipton-Hartman Eq. 3.7 SMD of sampling-score logits")
}
