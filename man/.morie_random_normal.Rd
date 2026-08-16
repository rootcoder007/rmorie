% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_random_normal}
\alias{.morie_random_normal}
\title{.morie_random_normal}
\usage{
.morie_random_normal(n, seed = 0, stream = 0)
}
\arguments{
\item{n}{Passed to \code{.morie_random_uniform}.}
\item{seed}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_uniform}. Defaults to \code{0}.}
}
\description{
A step of the rng_native implementation. Called by \code{.morie_random_multivariate_normal}, \code{.schab_simulate_unconditional}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_normal_quantile}.
}
\keyword{internal}
\examples{
.morie_random_normal(n = 5L)
}
