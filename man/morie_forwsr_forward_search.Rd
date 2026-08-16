% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_forwsr_forward_search}
\alias{morie_forwsr_forward_search}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
morie_forwsr_forward_search(X, y, start = NULL, n_draw = 500L, seed = 1L)
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later morie.fn -- function file (rootcoder007/morie) Sources:   Analysis, Springer, ISBN 978-0-387-95017-5,   least-median-of-squares start, the residual ordering that defines   each subset, and the monitoring of the deletion residuals.   unknown number of multivariate outliers", Journal of the Royal   Statistical Society Series B 71(2), 447-454 -- equation (12), the   consistency factor for a scale estimated from a truncated sample, Native implementation mirroring Python morie.fn.forwsr exactly: the same Gauss-Jordan solve with partial pivoting, the same SplitMix64 draws for the LMS start, the same residual ordering, and the same consistency correction. Row indices are 0-based here, as they are in the Python arm; R subscripts add one at the point of use.
}
\value{
A list with components \code{m}, \code{beta}, \code{sigma}, \code{s2}, \code{consistency_factor}, \code{sigma_corrected}, \code{min_deletion_residual}, \code{subset}.
}
\references{
  Atkinson, A. C. and Riani, M. (2000) Robust Diagnostic Regression
  doi:10.1007/978-1-4612-1160-0. The forward search: the
  Riani, M., Atkinson, A. C. and Cerioli, A. (2009) "Finding an
  doi:10.1111/j.1467-9868.2008.00692.x.
}
\keyword{internal}
