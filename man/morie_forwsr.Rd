% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_forwsr}
\alias{morie_forwsr}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
morie_forwsr(X, y, start = NULL, n_draw = 500L, seed = 1L, threshold = 3, 
    min_df = 5L) 
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later morie.fn -- function file (rootcoder007/morie) Sources:   Analysis, Springer, ISBN 978-0-387-95017-5,   least-median-of-squares start, the residual ordering that defines   each subset, and the monitoring of the deletion residuals.   unknown number of multivariate outliers", Journal of the Royal   Statistical Society Series B 71(2), 447-454 -- equation (12), the   consistency factor for a scale estimated from a truncated sample, Native implementation mirroring Python morie.fn.forwsr exactly: the same Gauss-Jordan solve with partial pivoting, the same SplitMix64 draws for the LMS start, the same residual ordering, and the same consistency correction. Row indices are 0-based here, as they are in the Python arm; R subscripts add one at the point of use.
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{start}{Argument \code{start}; see Usage.}
\item{n_draw}{Argument \code{n_draw}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
\item{threshold}{Argument \code{threshold}; see Usage.}
\item{min_df}{Argument \code{min_df}; see Usage.}
}
\value{
A list with components \code{m}, \code{entered}, \code{estimate}, \code{coefficients}, \code{steps}, \code{n}, \code{n_flagged}, \code{flagged}, \code{jump_at_m}, \code{threshold}, \code{min_df}, \code{monitored_from_m}, \code{entry_order}, \code{sigma_trajectory}, \code{mdr_trajectory}, \code{method}.
}
\references{
  Atkinson, A. C. and Riani, M. (2000) Robust Diagnostic Regression
  doi:10.1007/978-1-4612-1160-0. The forward search: the
  Riani, M., Atkinson, A. C. and Cerioli, A. (2009) "Finding an
  doi:10.1111/j.1467-9868.2008.00692.x.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_forwsr(V, V)
}
