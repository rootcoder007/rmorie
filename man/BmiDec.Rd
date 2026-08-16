% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BmiDec}
\alias{BmiDec}
\title{BmiDec}
\usage{
BmiDec(y, C, a = NULL, procnoise = 1e-04, obsnoise = 0.01, p0 = 0.01)
}
\arguments{
\item{y}{See Usage.}
\item{C}{See Usage.}
\item{a}{Defaults to \code{NULL}.}
\item{procnoise}{Defaults to \code{1e-04}.}
\item{obsnoise}{Defaults to \code{0.01}.}
\item{p0}{Defaults to \code{0.01}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{states}, \code{innovations}, \code{gain}, \code{predicted}, \code{method}.
}
\keyword{internal}
