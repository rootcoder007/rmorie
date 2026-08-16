% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.opnclp_predict}
\alias{.opnclp_predict}
\title{.opnclp_predict}
\usage{
.opnclp_predict(fit, compute)
}
\arguments{
\item{fit}{A list; the body reads \code{$alpha}, \code{$beta}, \code{$range} from it.}
\item{compute}{See Usage.}
}
\description{
A step of the opnclp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{value}, \code{extrapolation_decades}, \code{interpolated}, \code{note}.
}
\keyword{internal}
