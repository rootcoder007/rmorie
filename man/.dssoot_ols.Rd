% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dssoot_ols}
\alias{.dssoot_ols}
\title{Least squares of y on an intercept, x1 and optionally x2}
\usage{
.dssoot_ols(y, x1, x2 = NULL)
}
\arguments{
\item{y}{Passed to \code{.s03lstsq}.}
\item{x1}{See Usage.}
\item{x2}{Defaults to \code{NULL}.}
}
\description{
A step of the Dssoot implementation. Called by \code{Dssoot}. See the file header for the source the module follows.
}
\value{
The value of \code{.s03lstsq}.
}
\keyword{internal}
