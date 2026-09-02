% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pageexact}
\alias{Pageexact}
\title{Exact null distribution of Page\'s L -- Sec. 12.3, p. 448}
\usage{
Pageexact(k, n, ell = NULL) 
}
\description{
Exact null distribution of Page\'s L -- Sec. 12.3, p. 448
}
\arguments{
\item{k}{Argument \code{k}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{ell}{Argument \code{ell}; see Usage.}
}
\value{
A list with \code{support}, \code{pmf}, \code{pmf_l}, \code{sf_l}, \code{mean}, \code{var}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Pageexact(k = 5L, n = 5L)
}
