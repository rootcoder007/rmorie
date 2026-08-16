% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_jsd}
\alias{.t4_jsd}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
.t4_jsd(p, q, base)
}
\arguments{
\item{p}{A vector; its length is taken and its elements indexed.}
\item{q}{A vector; indexed elementwise.}
\item{base}{Numeric; passed to \code{log}.}
}
\description{
A step of the jsdivg implementation. Called by \code{Jsdiv}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.t4_jsd(p = 0.5, q = 0.5, base = c(1, 2, 3, 4, 5, 6, 7, 8))
}
