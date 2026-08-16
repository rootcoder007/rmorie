% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{noisy_argmax}
\alias{noisy_argmax}
\title{noisy_argmax}
\usage{
noisy_argmax(counts, gamma, seed = 0)
}
\arguments{
\item{counts}{A vector; its length is taken and its elements indexed.}
\item{gamma}{Passed to \code{.pate_lap_draw}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the pate_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{arg}, as built in the body.
}
\keyword{internal}
\examples{
noisy_argmax(counts = c(1, 2, 3, 4, 5, 6, 7, 8), gamma = 0.5)
}
