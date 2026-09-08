# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cauchy weight function for iteratively reweighted least squares
#'
#' Holland, P. W. and Welsch, R. E. (1977), "Robust regression using
#' iteratively reweighted least-squares", Communications in Statistics - Theory
#' and Methods 6(9), 813-827, doi:10.1080/03610927708827533. The bibliographic
#' record is verified against Crossref.
#'
#' CITATION LIMIT, stated rather than papered over. The article is closed
#' access: Taylor and Francis returns 403 to every fetch, OpenAlex and Semantic
#' Scholar report no open-access location, and no preprint or technical-report
#' version was found. No page or equation number is therefore attributed to it.
#' The weight function below is the one this module was specified with,
#' \code{w(r) = 1/(1 + (r/c)^2)}, and the companions follow from it by the
#' standard M-estimation relations rather than by quotation:
#' \code{psi(r) = r w(r) = r/(1 + (r/c)^2)} and
#' \code{rho(r) = (c^2/2) log(1 + (r/c)^2)}, with \code{d rho / dr = psi}.
#'
#' The rho is the log-density of a Cauchy up to constants, which is where the
#' name comes from and why the function behaves as it does. Unlike Huber, the
#' weight starts falling at r = 0 and never reaches zero: every observation
#' keeps some influence, however far out it lies, so the estimator redescends
#' but does not reject. psi attains its maximum at r = c and decreases
#' thereafter, so a residual twice as large as c carries LESS weight in absolute
#' terms than one at c -- the property Huber lacks and the bisquare takes to the
#' extreme of exact rejection.
#'
#' The default tuning constant c = 2.3849 is the value conventionally quoted for
#' about 95 percent asymptotic efficiency at the Gaussian; the function does not
#' assert it, and the accompanying anchor derives the efficiency
#' \eqn{(E psi prime)^2 / E psi^2} by quadrature instead of trusting the number.
#'
#' @param y Residuals, already divided by a scale estimate if one is used.
#' @param c Positive tuning constant. Default 2.3849.
#' @return List with \code{estimate} (the weights), \code{weights}, \code{psi},
#'   \code{rho}, \code{objective}, \code{c}, \code{n}, \code{method}.
#' @references Holland, P. W. and Welsch, R. E. (1977), Communications in
#'   Statistics - Theory and Methods 6(9):813-827,
#'   doi:10.1080/03610927708827533.
#' @examples
#' Cauchw(c(0, 1, 2.3849, 10))$weights  # 1, ..., exactly 0.5 at r = c, ...
#' @export
Cauchw <- function(y, c = 2.3849) {
  rv <- .s03vec(y)
  cc <- as.numeric(c)[1L]
  if (is.na(cc) || !(cc > 0) || is.infinite(cc)) {
    stop("cauchy_weight: c must be a positive finite number")
  }
  n <- length(rv)
  if (n == 0L) stop("cauchy_weight: y is empty")
  u <- rv / cc
  d <- 1 + u * u
  w <- 1 / d
  psi <- rv / d
  rho <- 0.5 * cc * cc * log(d)
  list(estimate = w, weights = w, psi = psi, rho = rho,
       objective = sum(rho), c = cc, n = as.integer(n),
       method = "Holland-Welsch (1977) Cauchy weight w(r) = 1/(1 + (r/c)^2)")
}
