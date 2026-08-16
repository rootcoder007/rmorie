% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatbeta}
\alias{Ostatbeta}
\title{Uniform order statistic is Beta(r, n-r+1) -- Theorem 2.4.3, p. 38}
\usage{
Ostatbeta(u, r, n)
}
\description{
Uniform order statistic is Beta(r, n-r+1) -- Theorem 2.4.3, p. 38
}
\value{
A list with \code{pdf}, \code{cdf}, \code{mean}, \code{var}, \code{alpha}, \code{beta}, \code{r}, \code{n}.
}
\keyword{internal}
\examples{
Ostatbeta(u = 0.3, r = 2, n = 5)
}
