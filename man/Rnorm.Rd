% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rnorm}
\alias{Rnorm}
\title{Inversion of the Philox uniform stream: draw k depends only on}
\usage{
Rnorm(n, mean = 0, sd = 1, seed = 0, stream = 0)
}
\arguments{
\item{n}{Passed to \code{.morie_random_uniform}.}
\item{mean}{Passed to \code{Qnorm}. Defaults to \code{0}.}
\item{sd}{Passed to \code{Qnorm}. Defaults to \code{1}.}
\item{seed}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
}
\description{
uniform k, so the stream is stable when n changes
}
\value{
The value of \code{Qnorm}.
}
\keyword{internal}
\examples{
Rnorm(n = 5L)
}
