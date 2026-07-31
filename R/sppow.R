# SPDX-License-Identifier: AGPL-3.0-or-later
#' Power semivariogram model (unbounded).
#'
#' gamma(h) = theta h^lambda, theta >= 0 and 0 <= lambda < 2. Not
#' second-order stationary: there is no sill. lambda = 1 gives the linear
#' model. lambda >= 2 violates the intrinsic hypothesis and is an error.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param nugget Nugget effect, added for h > 0.
#' @param c1 Scale theta, non-negative.
#' @param alpha Exponent lambda, in [0, 2).
#' @return Named list: gamma, nugget, theta, lambda, model.
#' @references Schabenberger & Gotway (2005), Sec 4.3.5, eq (4.21), p. 149.
#' @examples
#' sppow(h = c(0, 1, 2, 4), c1 = 1.5, alpha = 1)
#' @export
sppow <- function(h, nugget = 0, c1 = 1, alpha = 1) {
  if (c1 < 0) stop("`c1` (theta) must be >= 0")
  if (nugget < 0) stop("`nugget` must be >= 0")
  if (alpha < 0 || alpha >= 2) {
    stop("`alpha` (lambda) must satisfy 0 <= lambda < 2; lambda >= 2 ",
         "violates the intrinsic hypothesis")
  }
  h <- as.numeric(h)
  if (any(h < 0)) stop("lag distances `h` must be non-negative")
  g <- nugget + c1 * h^alpha
  g[h == 0] <- 0
  list(gamma = g, nugget = as.numeric(nugget), theta = as.numeric(c1),
       lambda = as.numeric(alpha), model = "power")
}
