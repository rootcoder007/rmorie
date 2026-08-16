% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ksdistfree}
\alias{Ksdistfree}
\title{KS statistics via the PIT -- Theorem 4.3.1, p. 111}
\usage{
Ksdistfree(x, cdf)
}
\description{
KS statistics via the PIT -- Theorem 4.3.1, p. 111
}
\value{
A list with \code{statistic}, \code{dplus}, \code{dminus}, \code{z}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(2)
Ksdistfree(rnorm(25), cdf = pnorm)
}
