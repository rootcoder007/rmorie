% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpblueblup}
\alias{.gpblueblup}
\title{.gpblueblup}
\usage{
.gpblueblup(X, Z, y, Sigma, R = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{t}.}
\item{Z}{A matrix; passed to \code{t}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{Sigma}{Passed to \code{.gplmmV}.}
\item{R}{Passed to \code{.gplmmV}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gpmultitrait}, \code{Msm010}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{u}.
}
\keyword{internal}
