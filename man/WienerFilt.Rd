% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerFilt}
\alias{WienerFilt}
\title{WienerFilt}
\usage{
WienerFilt(x, desired = NULL, order = 8, sd = NULL, seta = NULL, fs = 1)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{desired}{The body requires: give either a desired signal (time-domain route,.}
\item{order}{Passed to \code{Whopf}. Defaults to \code{8}.}
\item{sd}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{seta}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{W}, \code{route}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
