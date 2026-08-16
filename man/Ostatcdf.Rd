% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatcdf}
\alias{Ostatcdf}
\title{CDF of the r-th order statistic -- Theorem 2.4.1, eq. (2.4.1), p. 37}
\usage{
Ostatcdf(t, r, n, cdf)
}
\description{
CDF of the r-th order statistic -- Theorem 2.4.1, eq. (2.4.1), p. 37
}
\value{
A list with \code{cdf}, \code{sf}, \code{fx}, \code{r}, \code{n}, \code{t}.
}
\keyword{internal}
\examples{
Ostatcdf(t = 0.5, r = 2, n = 5, cdf = pnorm)
}
