# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Rate and risk measures -- R mirror of the Python modules incrtio,
# mhrate, riskdf, riskrt.
#
# Sources consulted, not recalled:
#   Rothman, K.J. & Greenland, S., Modern Epidemiology.  The text is
#   not in the corpus and the publisher blocks direct fetch, so the
#   estimators and their variances were taken from the OpenEpi
#   "Comparing Two Person-Time Rates" technical documentation, read in
#   full, which states them in the textbook's own form.
#   Greenland, S. & Robins, J.M. (1985). Estimation of a common effect
#   parameter from sparse follow-up data. Biometrics 41:55-68 -- the
#   Mantel-Haenszel rate ratio standard error.
#
# Everything is closed form.  The normal quantiles are tabulated
# constants rather than a call to qnorm, so both language arms divide
# by bit-identical numbers.
#
# Collision scan: epirate.R and all four exported names were free in
# both R trees and in _lazy_map.json at the time of writing.

#' .s02z
#'
#' A step of the epirate implementation. Called by \code{Incrtio}, \code{Mhrate}, \code{Riskdf} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param confidence See Usage.
#' @return The value of \code{unname}.
#' @export
.s02z <- function(confidence) {
  tab <- c("0.9" = 1.6448536269514722, "0.95" = 1.959963984540054,
           "0.99" = 2.5758293035489004)
  hit <- which(abs(as.numeric(names(tab)) - as.numeric(confidence)) < 1e-12)
  if (!length(hit)) stop("confidence must be one of 0.90, 0.95, 0.99",
                         call. = FALSE)
  unname(tab[hit[1]])
}

#' Incidence rate ratio
#'
#' \deqn{IRR = IR_e/IR_u}{IRR = IR_e / IR_u} with
#' \eqn{Var(\log IRR) = 1/a + 1/b} from the case counts.  The variance
#' needs counts, not rates, so the interval is returned only when the
#' counts are supplied.
#'
#' @param IR_e,IR_u Incidence rates in the exposed and unexposed.
#' @param cases_exposed,cases_unexposed Optional case counts.
#' @param confidence One of 0.90, 0.95, 0.99.
#' @return Named list with `estimate`, `ln_estimate`, `se_ln`,
#'   `ci_lower`, `ci_upper`, `confidence`, `method`.
#' @references Rothman & Greenland, Modern Epidemiology.
#' @examples
#' Incrtio(0.02, 0.01, cases_exposed = 20, cases_unexposed = 10)
#' @export
Incrtio <- function(IR_e, IR_u, cases_exposed = NULL,
                    cases_unexposed = NULL, confidence = 0.95) {
  re <- as.numeric(IR_e); ru <- as.numeric(IR_u)
  if (ru == 0) stop("unexposed incidence rate must be non-zero", call. = FALSE)
  irr <- re / ru
  se <- lo <- hi <- NULL
  if (!is.null(cases_exposed) && !is.null(cases_unexposed)) {
    a <- as.numeric(cases_exposed); b <- as.numeric(cases_unexposed)
    if (a <= 0 || b <= 0) stop("case counts must be positive for a CI",
                               call. = FALSE)
    se <- sqrt(1 / a + 1 / b)
    z <- .s02z(confidence)
    lo <- irr * exp(-z * se); hi <- irr * exp(z * se)
  }
  list(estimate = irr, ln_estimate = log(irr), se_ln = se,
       ci_lower = lo, ci_upper = hi, confidence = as.numeric(confidence),
       method = "incidence rate ratio (Rothman & Greenland)")
}

#' Mantel-Haenszel summary incidence rate ratio
#'
#' \deqn{IRR_{MH} = \sum_i a_i T_{0i}/T_i \big/ \sum_i b_i T_{1i}/T_i}{IRR_MH = sum_i a_i T0_i/T_i / sum_i b_i T1_i/T_i}
#' with the Greenland & Robins (1985) standard error
#' \deqn{SE = \sqrt{\sum_i m_i T_{1i}T_{0i}/T_i^2} \big/ \sqrt{(\sum_i a_i T_{0i}/T_i)(\sum_i b_i T_{1i}/T_i)}}{SE = sqrt(sum m_i T1_i T0_i / T_i^2) / sqrt(num * den)}
#' Person-time weighting lets sparse strata contribute instead of being
#' dropped.
#'
#' @param strata List of length-4 vectors or lists `(a, T1, b, T0)`:
#'   exposed cases, exposed person-time, unexposed cases, unexposed
#'   person-time.
#' @param confidence One of 0.90, 0.95, 0.99.
#' @return Named list with `estimate`, `ln_estimate`, `se_ln`,
#'   `ci_lower`, `ci_upper`, `numerator`, `denominator`, `n_strata`,
#'   `confidence`, `method`.
#' @references Greenland & Robins (1985) Biometrics 41:55-68.
#' @examples
#' Mhrate(list(c(10, 1000, 5, 1000), c(20, 2000, 15, 2500)))
#' @export
Mhrate <- function(strata, confidence = 0.95) {
  if (!length(strata)) stop("need at least one stratum", call. = FALSE)
  num <- 0; den <- 0; vnum <- 0
  for (s in strata) {
    v <- if (!is.null(names(s)) && all(c("a", "T1", "b", "T0") %in% names(s))) {
      c(as.numeric(s[["a"]]), as.numeric(s[["T1"]]),
        as.numeric(s[["b"]]), as.numeric(s[["T0"]]))
    } else as.numeric(unlist(s))
    if (length(v) != 4) stop("each stratum needs (a, T1, b, T0)", call. = FALSE)
    a <- v[1]; T1 <- v[2]; b <- v[3]; T0 <- v[4]
    Tt <- T1 + T0
    if (Tt <= 0) stop("stratum person-time must be positive", call. = FALSE)
    num <- num + a * T0 / Tt
    den <- den + b * T1 / Tt
    vnum <- vnum + (a + b) * T1 * T0 / (Tt * Tt)
  }
  if (den <= 0 || num <= 0) stop("both arms need cases for a rate ratio",
                                 call. = FALSE)
  irr <- num / den
  se <- sqrt(vnum) / sqrt(num * den)
  z <- .s02z(confidence)
  list(estimate = irr, ln_estimate = log(irr), se_ln = se,
       ci_lower = irr * exp(-z * se), ci_upper = irr * exp(z * se),
       numerator = num, denominator = den, n_strata = length(strata),
       confidence = as.numeric(confidence),
       method = "Mantel-Haenszel rate ratio (Greenland & Robins 1985)")
}

#' Risk difference between two proportions
#'
#' \deqn{RD = p_e - p_u}{RD = p_e - p_u} with the binomial variance
#' \eqn{p_e(1-p_e)/n_e + p_u(1-p_u)/n_u}.  The arm sizes are optional;
#' without them only the point estimate is defined.
#'
#' @param p_exposed,p_unexposed Risks in the two arms.
#' @param n_exposed,n_unexposed Optional arm sizes.
#' @param confidence One of 0.90, 0.95, 0.99.
#' @return Named list with `estimate`, `se`, `ci_lower`, `ci_upper`,
#'   `p_exposed`, `p_unexposed`, `confidence`, `method`.
#' @references Rothman & Greenland, Modern Epidemiology.
#' @examples
#' Riskdf(0.3, 0.2, 100, 100)
#' @export
Riskdf <- function(p_exposed, p_unexposed, n_exposed = NULL,
                   n_unexposed = NULL, confidence = 0.95) {
  pe <- as.numeric(p_exposed); pu <- as.numeric(p_unexposed)
  if (pe < 0 || pe > 1 || pu < 0 || pu > 1)
    stop("risks must lie in [0, 1]", call. = FALSE)
  rd <- pe - pu
  se <- lo <- hi <- NULL
  if (!is.null(n_exposed) && !is.null(n_unexposed)) {
    ne <- as.numeric(n_exposed); nu <- as.numeric(n_unexposed)
    if (ne <= 0 || nu <= 0) stop("arm sizes must be positive", call. = FALSE)
    se <- sqrt(pe * (1 - pe) / ne + pu * (1 - pu) / nu)
    z <- .s02z(confidence)
    lo <- rd - z * se; hi <- rd + z * se
  }
  list(estimate = rd, se = se, ci_lower = lo, ci_upper = hi,
       p_exposed = pe, p_unexposed = pu,
       confidence = as.numeric(confidence),
       method = "risk difference (Rothman & Greenland)")
}

#' Risk ratio between two proportions
#'
#' \deqn{RR = p_e/p_u}{RR = p_e / p_u} with
#' \eqn{Var(\log RR) = (1-p_e)/(n_e p_e) + (1-p_u)/(n_u p_u)}.  The
#' interval is symmetric on the log scale, never on the ratio scale.
#'
#' @param p_exposed,p_unexposed Risks in the two arms.
#' @param n_exposed,n_unexposed Optional arm sizes.
#' @param confidence One of 0.90, 0.95, 0.99.
#' @return Named list with `estimate`, `ln_estimate`, `se_ln`,
#'   `ci_lower`, `ci_upper`, `p_exposed`, `p_unexposed`, `confidence`,
#'   `method`.
#' @references Rothman & Greenland, Modern Epidemiology.
#' @examples
#' Riskrt(0.3, 0.2, 100, 100)
#' @export
Riskrt <- function(p_exposed, p_unexposed, n_exposed = NULL,
                   n_unexposed = NULL, confidence = 0.95) {
  pe <- as.numeric(p_exposed); pu <- as.numeric(p_unexposed)
  if (pe < 0 || pe > 1 || pu < 0 || pu > 1)
    stop("risks must lie in [0, 1]", call. = FALSE)
  if (pu == 0) stop("unexposed risk must be non-zero", call. = FALSE)
  rr <- pe / pu
  se <- lo <- hi <- NULL
  if (!is.null(n_exposed) && !is.null(n_unexposed)) {
    ne <- as.numeric(n_exposed); nu <- as.numeric(n_unexposed)
    if (ne <= 0 || nu <= 0) stop("arm sizes must be positive", call. = FALSE)
    if (pe <= 0) stop("exposed risk must be positive for a CI", call. = FALSE)
    se <- sqrt((1 - pe) / (ne * pe) + (1 - pu) / (nu * pu))
    z <- .s02z(confidence)
    lo <- rr * exp(-z * se); hi <- rr * exp(z * se)
  }
  list(estimate = rr, ln_estimate = log(rr), se_ln = se,
       ci_lower = lo, ci_upper = hi, p_exposed = pe, p_unexposed = pu,
       confidence = as.numeric(confidence),
       method = "risk ratio (Rothman & Greenland)")
}
