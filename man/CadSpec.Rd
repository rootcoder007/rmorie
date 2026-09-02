% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CadSpec}
\alias{CadSpec}
\title{CadSpec: frequency-domain feature extraction for CAD}
\usage{
CadSpec(x, fs, bands = NULL) 
}
\arguments{
\item{x}{Passed to \code{.bsapsd}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{bands}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
set.seed(8)
fs <- 2000
x <- sin(2 * pi * 100 * seq(0, 1, by = 1 / fs)) + 0.1 * rnorm(2001)
CadSpec(x, fs = fs)
}
