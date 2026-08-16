% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gplmmV}
\alias{.gplmmV}
\title{Chapter 5: linear mixed model, eqs (5.1)-(5.2) and REML}
\usage{
.gplmmV(Z, D, R = NULL)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
\item{D}{Passed to \code{.gpmat}.}
\item{R}{Optional; may be \code{NULL}. Passed to \code{.gpmat}.}
}
\description{
--------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.gplmmV(Z = 5L, D = 5L)
}
