# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pseudo coefficients of determination for a logistic regression
#'
#' Source READ FROM THE CORPUS PDF, pages rendered with pdftoppm:
#' Hedderich, Sachs and Reynarowych, Applied Statistics: Methods Using R,
#' section 8.4.6 "Pseudo Coefficients of Determination", printed pages
#' 838-839, equations (8.70), (8.71) and (8.72):
#' \code{R2_McF = 1 - LLmod / LL0},
#' \code{R2_CS = 1 - (L0 / Lmod)^(2/n)} and
#' \code{R2_N = R2_CS / (1 - L0^(2/n))}.
#'
#' Written on the log scale here so that L0 and Lmod never underflow:
#' \code{R2_CS = 1 - exp(2 (LL0 - LLmod) / n)} and
#' \code{R2_N = R2_CS / (1 - exp(2 LL0 / n))}.
#'
#' The book records the ranges: 0 <= R2_McF < 1 (below 0.2 low, above
#' 0.4 a significant improvement) and 0 <= R2_CS < 1, with R2_N
#' normalised so that 1 is attainable (above 0.2 acceptable, above 0.5
#' very good).
#'
#' @param llmod Maximised log-likelihood of the model under assessment.
#' @param llnull Maximised log-likelihood of the intercept-only model;
#'   must be strictly negative.
#' @param n Number of observations the model was fitted on.
#' @return list: mcfadden, coxsnell, nagelkerke, coxsnell_max, llmod,
#'   llnull, n.
#' @examples
#' Pseudor2(-20.31519 / 2, -28.26715 / 2, 23)$mcfadden
#' @export
Pseudor2 <- function(llmod, llnull, n) {
  llmod <- as.numeric(llmod)[1]
  llnull <- as.numeric(llnull)[1]
  n <- as.integer(n)[1]
  if (!is.finite(llmod) || !is.finite(llnull)) stop("log-likelihoods must be finite")
  if (is.na(n) || n < 1L) stop("n must be at least 1")
  if (llnull >= 0) {
    stop("llnull must be strictly negative for (8.70) and (8.72) to be defined")
  }
  mcf <- 1 - llmod / llnull
  cs <- 1 - exp(2 * (llnull - llmod) / n)
  csmax <- 1 - exp(2 * llnull / n)
  list(
    mcfadden = mcf, coxsnell = cs, nagelkerke = cs / csmax,
    coxsnell_max = csmax, llmod = llmod, llnull = llnull, n = n
  )
}
