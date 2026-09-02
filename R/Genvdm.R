# SPDX-License-Identifier: AGPL-3.0-or-later
#' D study: project a G study onto a proposed facet size
#'
#' A D study holds the estimated variance components fixed and asks what
#' the coefficients would be for a different number of conditions.  Both
#' coefficients increase monotonically in n and tend to 1, which is what
#' makes the D study a decision tool.
#'
#' Formula: E rho^2(n) = var_p / (var_p + var_pi/n);
#'   Phi(n) = var_p / (var_p + (var_i + var_pi)/n).
#'
#' @param G_components Length-3 vector (var_p, var_i, var_pi).
#' @param n_proposed One or more candidate numbers of conditions.
#' @param target Generalizability coefficient the decision looks for.
#' @return List with \code{estimate}, \code{e_rho2}, \code{phi},
#'   \code{meets_target}, \code{n_required}, \code{n}, \code{method}.
#' @references Brennan (2001), Generalizability Theory, Springer, ch. 3.
#' @export
#' @examples
#' Genvdm(c(0.5, 0.2, 0.3), n_proposed = 20)
Genvdm <- function(G_components, n_proposed, target = 0.8) {
  g <- .s03vec(G_components)
  if (length(g) != 3L) stop("d_study_decision: G_components must hold three variances")
  if (any(g < 0)) stop("d_study_decision: variance components must be non-negative")
  vp <- g[1]
  vi <- g[2]
  vpi <- g[3]
  ns <- as.integer(.s03vec(n_proposed))
  if (length(ns) == 0L) stop("d_study_decision: n_proposed is empty")
  if (any(ns < 1L)) stop("d_study_decision: n_proposed must be positive")
  tg <- as.numeric(target)
  if (!(tg > 0 && tg < 1)) stop("d_study_decision: target must lie in (0, 1)")
  er <- numeric(length(ns))
  ph <- numeric(length(ns))
  for (i in seq_along(ns)) {
    de <- vp + vpi / ns[i]
    er[i] <- if (de != 0) vp / de else NaN
    de2 <- vp + (vi + vpi) / ns[i]
    ph[i] <- if (de2 != 0) vp / de2 else NaN
  }
  meets <- as.integer(!is.na(er) & er >= tg)
  chosen <- 0L
  for (i in seq_along(ns)) if (meets[i] == 1L) { chosen <- ns[i]
  break }
  .t1_result(estimate = er[length(er)], e_rho2 = er, phi = ph,
             meets_target = meets, n_required = chosen, n = length(ns),
             method = "D study projection of fixed variance components, Brennan (2001) ch. 3")
}
