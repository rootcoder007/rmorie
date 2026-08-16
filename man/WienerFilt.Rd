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
\item{x}{See Usage.}
\item{desired}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{8}.}
\item{sd}{Defaults to \code{NULL}.}
\item{seta}{Defaults to \code{NULL}.}
\item{fs}{Defaults to \code{1}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{W}, \code{route}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
