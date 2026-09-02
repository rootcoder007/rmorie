% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Jackd}
\alias{Jackd}
\title{Delete-d jackknife variance (Shao & Wu 1989)}
\usage{
Jackd(theta, n, d) 
}
\description{
Delete-d jackknife variance (Shao & Wu 1989)
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{d}{Argument \code{d}; see Usage.}
}
\value{
A list with \code{variance}, \code{se}, \code{mean}, \code{n_subsets}, \code{n}, \code{d}.
}
\keyword{internal}
\examples{
set.seed(4)
Jackd(theta = rnorm(10, 2, 0.1), n = 10, d = 1)
}
