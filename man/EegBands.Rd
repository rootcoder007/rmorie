% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EegBands}
\alias{EegBands}
\title{Band limits from Section 1.2.6, fractional power by eq (6.44) as used}
\usage{
EegBands(x, fs, bands = NULL)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{bands}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
in Section 10.2.3.  The clinical question -- is there an alpha rhythm -- is answered by the fraction of power in that band, not by the power.
}
\value{
A list with \code{power}, \code{fraction}, \code{dominant}, \code{totalpower}, \code{frequencies}, \code{bands}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
fs <- 100
eeg <- sin(2 * pi * 10 * seq(0, 4, by = 1 / fs)) + 0.3 * rnorm(401)
EegBands(eeg, fs = fs)
}
