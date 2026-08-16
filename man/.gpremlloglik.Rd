% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpremlloglik}
\alias{.gpremlloglik}
\title{.gpremlloglik}
\usage{
.gpremlloglik(X, Z, y, D, R = NULL)
}
\arguments{
\item{X}{Passed to \code{.gpmat}.}
\item{Z}{Passed to \code{.gplmmV}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{D}{Passed to \code{.gplmmV}.}
\item{R}{Passed to \code{.gplmmV}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm011}. See the file header for the source the module follows.
}
\value{
A list with \code{value}, \code{beta}.
}
\keyword{internal}
