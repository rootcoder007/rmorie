% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{quantile_bins}
\alias{quantile_bins}
\title{quantile_bins}
\usage{
quantile_bins(samples, n_bins = 4096L)
}
\arguments{
\item{samples}{Passed to \code{.chronos_vec}.}
\item{n_bins}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4096L}.}
}
\description{
A step of the chronos_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{centers}, \code{edges}, \code{n_bins}, \code{scheme}, \code{range}, \code{caveat}.
}
\keyword{internal}
\examples{
set.seed(1)
quantile_bins(rnorm(500), n_bins = 16L)
}
