% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_ukrig}
\alias{morie_ukrig}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
morie_ukrig(x, coords, target, model = "exponential", nugget = 0, sill = 1, range_ = 1, trend_order = 1)
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later ' Universal kriging with polynomial trend. ' ' Z(s) = mu(s) + delta(s), mu(s) = sum_k beta_k f_k(s). ' ' @param x Numeric vector. ' @param coords Numeric coordinate matrix (n by d). ' @param target Numeric matrix (m by d) or vector (d,). ' @param model Covariance: "exponential", "gaussian", "spherical". ' @param nugget,sill,range_ Variogram parameters. ' @param trend_order 0, 1, or 2. ' @return Named list: estimate, se, n, method. ' @examples ' ukrig(x = rnorm(50), coords = matrix(runif(100), 50, 2), target = rnorm(50)) ' @export
}
\value{
A list with components \code{nrow}, \code{ncol}, \code{exponential}, \code{h}, \code{gaussian}, \code{spherical}, \code{trend_order}, \code{error}, \code{estimate}, \code{m}, \code{se}, \code{n}, \code{method}.
}
\references{
' @references Schabenberger & Gotway (2005), Ch 4.
}
\keyword{internal}
