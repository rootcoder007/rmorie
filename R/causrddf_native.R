# Fuzzy regression discontinuity: the Wald ratio of the outcome jump
# to the treatment-probability jump at the cutoff.
# Sources: Hahn, J., Todd, P. and van der Klaauw, W. (2001),
# Econometrica 69(1), 201-209, Theorem 3 (fuzzy identification as the
# ratio of the two one-sided limits); Imbens, G. and Lemieux, T.
# (2008), Journal of Econometrics 142(2), 615-635, Sec. 3.2 and 4.3.
#
# Native implementation mirroring Python morie.fn.causrddf exactly,
# including the documented simplification of the standard error: the
# delta method is applied treating the outcome and treatment jumps as
# independent, so the Imbens-Lemieux covariance term is omitted.

#' Fuzzy regression discontinuity (Wald ratio)
#'
#' Estimates \eqn{\tau = \tau_y / \tau_w}, the ratio of the outcome
#' discontinuity to the treatment discontinuity at the cutoff, each
#' from a sharp local linear fit (Hahn, Todd and van der Klaauw 2001,
#' Theorem 3).  When treatment switches deterministically at the
#' cutoff the denominator is 1 and the estimate reduces to the sharp
#' one of \code{\link{morie_causrdd}}.
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param treat Treatment received (0/1 or a probability).
#' @param cutoff Threshold, default 0.
#' @param h Bandwidth for the outcome fit; \code{NULL} uses
#'   \code{\link{morie_causrddh}} on the outcome.
#' @param h_treat Bandwidth for the first stage; \code{NULL} uses
#'   \code{\link{morie_causrddh}} on the treatment.
#' @param kernel \code{"triangular"} (default) or \code{"uniform"}.
#' @return A list with \code{estimate}, \code{se}, \code{ci},
#'   \code{jump_outcome}, \code{jump_treatment}, \code{se_outcome},
#'   \code{se_treatment}, \code{h_outcome}, \code{h_treatment},
#'   \code{kernel}, \code{sharp_outcome}, \code{sharp_treatment},
#'   \code{se_note}, \code{method}.
#' @references Imbens, G. and Lemieux, T. (2008). Regression
#'   discontinuity designs: a guide to practice. Journal of
#'   Econometrics, 142(2), 615-635.
#' @export
morie_causrddf <- function(x, y, treat, cutoff = 0, h = NULL,
                           h_treat = NULL, kernel = "triangular") {
  xa <- as.numeric(x); ya <- as.numeric(y); wa <- as.numeric(treat)
  cc <- as.numeric(cutoff)
  if (is.null(h)) h <- morie_causrddh(xa, ya, cutoff = cc)$estimate
  if (is.null(h_treat)) h_treat <- morie_causrddh(xa, wa, cutoff = cc)$estimate
  h <- as.numeric(h); h_treat <- as.numeric(h_treat)
  fy <- morie_causrdd(xa, ya, cutoff = cc, h = h, kernel = kernel)
  fw <- morie_causrdd(xa, wa, cutoff = cc, h = h_treat, kernel = kernel)
  ty <- fy$estimate
  tw <- fw$estimate
  if (abs(tw) < 1e-12)
    stop(paste("no first-stage discontinuity: the treatment jump at",
               "the cutoff is numerically zero"))
  tau <- ty / tw
  se <- sqrt((fy$se^2 + tau^2 * fw$se^2) / tw^2)
  z <- 1.959963984540054
  list(estimate = tau, se = se, ci = c(tau - z * se, tau + z * se),
       jump_outcome = ty, jump_treatment = tw,
       se_outcome = fy$se, se_treatment = fw$se,
       h_outcome = h, h_treatment = h_treat, kernel = kernel,
       sharp_outcome = fy, sharp_treatment = fw,
       se_note = paste("delta method with independent jumps; the",
                       "Imbens-Lemieux covariance term is omitted and",
                       "documented"),
       method = "fuzzy RDD, Wald ratio of local linear jumps")
}
