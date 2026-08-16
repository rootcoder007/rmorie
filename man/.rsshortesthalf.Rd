% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsshortesthalf}
\alias{.rsshortesthalf}
\title{Shortest window of h points in a sorted univariate sample.  Rousseeuw}
\usage{
.rsshortesthalf(v, h)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
\item{h}{Numeric; combined arithmetically in the body.}
}
\description{
(1984) Theorem 2, p. 873: in one dimension the LMS location is the midpoint of the shortest half, found as the smallest of y_{h:n} - y_{1:n}, ..., y_{n:n} - y_{n-h+1:n}.  The same contiguity argument gives the univariate MCD and MVE subsets.  Returns list(start, width, sorted) with a 1-based start.
}
\value{
A list with \code{start}, \code{width}, \code{sorted}.
}
\keyword{internal}
\examples{
.rsshortesthalf(v = c(1, 2, 3, 4, 5, 6, 7, 8), h = 1)
}
