% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_cv_score}
\alias{.gwr_cv_score}
\title{.gwr_cv_score}
\usage{
.gwr_cv_score(y, X, D, bw, kernel, adaptive)
}
\arguments{
\item{y}{A matrix; passed to \code{crossprod}.}
\item{X}{A matrix; indexed by row and column.}
\item{D}{A matrix; indexed by row and column.}
\item{bw}{Passed to \code{.gwr_kernel}.}
\item{kernel}{Passed to \code{.gwr_kernel}.}
\item{adaptive}{A flag; the body branches on it.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{.gwr_criterion}. See the file header for the source the module follows.
}
\value{
The value of \code{acc}, as built in the body.
}
\keyword{internal}
