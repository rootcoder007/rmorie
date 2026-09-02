% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_gamma}
\alias{poltrx_gamma}
\title{Marsaglia-Tsang Gamma sampler, shape >= 1, matching the Python arm}
\usage{
poltrx_gamma(e, shape) 
}
\arguments{
\item{e}{Passed to \code{.ghc_unif}.}
\item{shape}{Numeric; combined arithmetically in the body.}
}
\description{
Strict: the SAME uniform draws in the SAME order as the Python implementation, so both arms agree on every variate.
}
\value{
The value of \code{repeat}.
}
\keyword{internal}
\examples{
e <- .ghc_rng(1)
poltrx_gamma(e, shape = 2.5)
}
