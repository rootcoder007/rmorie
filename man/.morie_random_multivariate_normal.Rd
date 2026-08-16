% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_random_multivariate_normal}
\alias{.morie_random_multivariate_normal}
\title{.morie_random_multivariate_normal}
\usage{
.morie_random_multivariate_normal(mean, cov, seed = 0, stream = 0, jitter = 1e-10)
}
\arguments{
\item{mean}{A vector; its length is taken.}
\item{cov}{A matrix; passed to \code{dim}.}
\item{seed}{Passed to \code{.morie_random_normal}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_normal}. Defaults to \code{0}.}
\item{jitter}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the rng_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
