% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_cokrg}
\alias{morie_cokrg}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
morie_cokrg(x, y, coords, target, sill_p = 1, range_p = 1, sill_s = 1, range_s = 1, cross_sill = 0.5, cross_range = 1, nugget = 0)
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later ' Simple morie_cokriging for co-located bivariate spatial prediction. ' ' \\deqn\{\\hat Z_1(s_0) = \\lambda^\\top Z_1 + \\mu^\\top Z_2\}\{hat Z_1(s_0) = lambda^top Z_1 + mu^top Z_2\}, system ' \\deqn\{\\[C_\{pp\} \\; C_\{ps\}; C_\{ps\}^\\top \\; C_\{ss\}\\] \\[\\lambda; \\mu\\] = \\[c_\{0p\}; c_\{0s\}\\]\}\{\\[C_pp C_ps; C_ps^top C_ss\\] \\[lambda; mu\\] = \\[c_0p; c_0s\\]\}. ' ' @param x Primary variable (n,). ' @param y Secondary variable (n,). ' @param coords Co-located coord matrix. ' @param target Target coords (m by d) or (d,). ' @param sill_p,range_p Primary auto-covariance parameters. ' @param sill_s,range_s Secondary auto-covariance parameters. ' @param cross_sill,cross_range Cross-covariance parameters. ' @param nugget Nugget. ' @return Named list: estimate, se, n, method. ' @examples ' cokrg(x = rnorm(50), y = rnorm(50), coords = matrix(runif(100), 50, 2), target = rnorm(50)) ' @export
}
\value{
A list with components \code{nrow}, \code{ncol}, \code{byrow}, \code{D_}, \code{error}, \code{estimate}, \code{m}, \code{se}, \code{n}, \code{method}.
}
\references{
' @references Schabenberger & Gotway (2005), Ch 4.
}
\keyword{internal}
