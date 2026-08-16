% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clpopt_fail}
\alias{.clpopt_fail}
\title{Internal: report a failure. Mirrors clpopt._fail}
\usage{
.clpopt_fail(st, rule, phase)
}
\arguments{
\item{st}{Compared against \code{"cycling"}.}
\item{rule}{Compared against \code{"dantzig"}.}
\item{phase}{See Usage.}
}
\description{
A step of the clpopt_native implementation. Called by \code{simplex}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{status}, \code{x}, \code{fun}, \code{rule}, \code{message}, \code{method}.
}
\keyword{internal}
