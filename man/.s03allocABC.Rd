% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03allocABC}
\alias{.s03allocABC}
\title{The A and C pieces of Neyman (1934) eq. (39), p. 580, together with}
\usage{
.s03allocABC(M, S, m0)
}
\arguments{
\item{M}{A vector; its length is taken and its elements indexed.}
\item{S}{A vector; indexed elementwise.}
\item{m0}{Numeric; combined arithmetically in the body.}
}
\description{
M_0 and T = sum_j M_j S_j.  B is omitted because it depends on the allocation rather than on the population; the callers add it themselves.
}
\value{
A list with \code{A}, \code{C}, \code{M0}, \code{T}.
}
\keyword{internal}
\examples{
.s03allocABC(M = 5L, S = c(1, 2, 3, 4, 5, 6, 7, 8), m0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
