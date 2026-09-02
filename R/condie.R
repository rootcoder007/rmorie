# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional indirect effect (moderated mediation)
#'
#' For the model in which the a path is moderated by W, the conditional
#' indirect effect is b1 (a1 + a3 W), with the simple-slope standard error of
#' equation (8) and the first-order delta-method standard error of equation
#' (13) applied to this model.  Source consulted: Preacher, Rucker and Hayes
#' (2007), Addressing moderated mediation hypotheses, Multivariate Behavioral
#' Research 42(1), 185-227, p.197 and equations (8), (13), (14).
#'
#' @param a1 coefficient of X in the mediator model.
#' @param a3 coefficient of the X-by-W product in the mediator model.
#' @param b coefficient of M in the outcome model.
#' @param w moderator value(s) at which to condition.
#' @param sa1,sa3,sb standard errors of a1, a3 and b.
#' @param sa1a3 covariance of a1 and a3.
#' @return list: estimate, simple_slope, se, se_slope, z, p_value, w, n, method.
#' @keywords internal
#' @examples
#' condie(0.5, 0, 0.4, 3)
#' @export
condie <- function(a1, a3, b, w, sa1 = NULL, sa3 = NULL, sa1a3 = 0, sb = NULL) {
  wv <- as.numeric(w)
  n <- length(wv)
  slope <- a1 + a3 * wv
  eff <- b * slope
  if (is.null(sa1) || is.null(sa3) || is.null(sb)) {
    se <- rep(NA_real_, n)
    se_slope <- rep(NA_real_, n)
  } else {
    vslope <- sa1^2 + 2 * sa1a3 * wv + sa3^2 * wv^2
    se_slope <- ifelse(vslope >= 0, sqrt(vslope), NA_real_)
    se <- sqrt(slope^2 * sb^2 + b^2 * vslope)
  }
  z <- ifelse(!is.na(se) & se > 0, eff / se, NA_real_)
  pv <- ifelse(!is.na(z), 2 * (1 - stats::pnorm(abs(z))), NA_real_)
  if (n == 1) {
    return(list(estimate = as.numeric(eff), simple_slope = as.numeric(slope),
                se = as.numeric(se), se_slope = as.numeric(se_slope),
                z = as.numeric(z), p_value = as.numeric(pv),
                w = as.numeric(wv), n = 1L,
                method = "Conditional indirect effect (Preacher, Rucker & Hayes 2007)"))
  }
  list(estimate = mean(eff), effect = eff, simple_slope = slope, se = se,
       se_slope = se_slope, z = z, p_value = pv, w = wv, n = as.integer(n),
       method = "Conditional indirect effect (Preacher, Rucker & Hayes 2007)")
}

# CANONICAL TEST
# r <- condie(0.5, 0, 0.4, 3)
# stopifnot(abs(r$estimate - 0.2) < 1e-12, abs(r$simple_slope - 0.5) < 1e-12)

#' @rdname condie
#' @keywords internal
#' @export
morie_conditional_indirect_effect <- condie
