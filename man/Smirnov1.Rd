% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Smirnov1}
\alias{Smirnov1}
\title{Exact one-sided Smirnov distribution -- Sec. 6.3, p. 241}
\usage{
Smirnov1(d, m, n)
}
\description{
Exact one-sided Smirnov distribution -- Sec. 6.3, p. 241
}
\value{
A list with \code{sf}, \code{cdf}, \code{sf_asymp}, \code{k}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Smirnov1(d = 0.4, m = 10, n = 12)
}
