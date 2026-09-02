# SPDX-License-Identifier: AGPL-3.0-or-later

#' Convergent validity of a reflective construct
#'
#' Formula: AVE = sum lambda_i^2 / (sum lambda_i^2 + sum theta_i)
#'
#' With standardised loadings the residual variance of an indicator is
#' theta_i = 1 - lambda_i^2, which is what is used when \code{residuals}
#' is not supplied.  Composite reliability follows the same algebra,
#' CR = (sum lambda_i)^2 / ((sum lambda_i)^2 + sum theta_i).  Fornell
#' and Larcker's rule is AVE >= 0.5 with CR >= 0.7.
#'
#' @param loadings Factor loadings of the indicators on their construct.
#' @param residuals Indicator residual variances, or NULL for
#'   1 - lambda^2.
#' @return List with \code{estimate} (AVE), \code{ave}, \code{cr},
#'   \code{adequate}, \code{n_items}, \code{method}.
#' @references Fornell & Larcker (1981), J. Marketing Research
#'   18(1):39-50.
#' @export
#' @examples
#' Convgs(loadings = c(0.7, 0.8, 0.75, 0.6))
Convgs <- function(loadings, residuals = NULL) {
  lam <- .s03vec(loadings)
  p <- length(lam)
  if (p == 0L) stop("empty input: no loadings supplied")
  th <- if (is.null(residuals)) 1 - lam * lam else .s03vec(residuals)
  if (length(th) != p) stop("loadings and residuals must have the same length")
  if (any(th < 0)) stop("residual variances must be non-negative")
  sl2 <- sum(lam * lam)
  sth <- sum(th)
  sl <- sum(lam)
  if (sl2 + sth <= 0) stop("degenerate construct: total variance is zero")
  ave <- sl2 / (sl2 + sth)
  cr <- sl * sl / (sl * sl + sth)
  .t1_result(estimate = ave, ave = ave, cr = cr,
             adequate = as.integer(ave >= 0.5 && cr >= 0.7), n_items = p,
             method = "convergent validity: AVE and composite reliability")
}
