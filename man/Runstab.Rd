% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Runstab}
\alias{Runstab}
\title{Table D: exact runs distribution -- Theorem 3.2.2, p. 79}
\usage{
Runstab(n1, n2, r = NULL)
}
\description{
Table D: exact runs distribution -- Theorem 3.2.2, p. 79
}
\value{
A list with \code{support}, \code{pmf}, \code{cdf}, \code{pmf_r}, \code{cdf_r}, \code{sf_r}, \code{mean}, \code{var}, \code{n1}, \code{n2}.
}
\keyword{internal}
\examples{
Runstab(n1 = 5L, n2 = 5L)
}
