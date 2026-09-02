% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{targeted_bootstrap}
\alias{targeted_bootstrap}
\title{targeted_bootstrap}
\usage{
targeted_bootstrap(P_star_sampler, estimator, B = 200L, seed = 0L) 
}
\arguments{
\item{P_star_sampler}{Accepted by the signature and not used anywhere in the body.}
\item{estimator}{Passed to \code{morie_tlboot}.}
\item{B}{Passed to \code{morie_tlboot}. Defaults to \code{200L}.}
\item{seed}{Passed to \code{morie_tlboot}. Defaults to \code{0L}.}
}
\description{
A step of the tlboot_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_tlboot}.
}
\keyword{internal}
\examples{
set.seed(2)
sampler <- function(e) rnorm(30, rmorie:::.ghc_norm(e, 1L))
r <- targeted_bootstrap(sampler, estimator = function(s) mean(s),
                        B = 100L)
str(r, max.level = 1)
}
