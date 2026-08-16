% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_wls_int}
\alias{.t4_wls_int}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
.t4_wls_int(xs, ys, ws)
}
\arguments{
\item{xs}{Numeric; combined arithmetically in the body.}
\item{ys}{Numeric; combined arithmetically in the body.}
\item{ws}{Numeric; passed to \code{sqrt}.}
}
\description{
weighted least squares of y on (1, x); returns the intercept
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
.t4_wls_int(xs = c(1, 2, 3, 4, 5, 6, 7, 8), ys = c(1, 2, 3, 4, 5, 6, 7, 8), ws = c(1, 2, 3, 4, 5, 6, 7, 8))
}
