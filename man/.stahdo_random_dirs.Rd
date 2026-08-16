% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stahdo_random_dirs}
\alias{.stahdo_random_dirs}
\title{.stahdo_random_dirs}
\usage{
.stahdo_random_dirs(p, n_dirs, seed)
}
\arguments{
\item{p}{Passed to \code{.ghc_unif}.}
\item{n_dirs}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the stahdo_native implementation. Called by \code{.stahdo_outlyingness}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\references{
----------
Maronna, R. A. & Yohai, V. J. (1995) "The behavior of the
Stahel-Donoho robust multivariate estimator", *Journal of the
American Statistical Association* 90(429), 330-341,
doi:10.1080/01621459.1995.10476517.
}
\keyword{internal}
\examples{
.stahdo_random_dirs(p = 1, n_dirs = 5L, seed = 5L)
}
