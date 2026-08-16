% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_threshold}
\alias{morie_threshold}
\title{Single-marker LOD significance threshold}
\usage{
morie_threshold(alpha = 0.05)
}
\arguments{
\item{alpha}{Significance level in \code{(0, 1)}, default 0.05.}
}
\description{
Solves for the upper-tail quantile \code{z} of the standard normal at level \code{alpha} by bisection on the complementary error function, then reports \code{T = (1/2) log10(e) z^2}. The Python arm notes that this threshold applies to a single marker only -- a genome scan needs a permutation threshold.
}
\value{
A named list with keys \code{threshold}, \code{z},   \code{alpha}, \code{note}.
}
\keyword{internal}
\examples{
morie_threshold()
}
