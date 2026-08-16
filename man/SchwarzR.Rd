% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SchwarzR}
\alias{SchwarzR}
\title{Eq (4.43), the real case of eq (4.42); equality when a = K b, the two}
\usage{
SchwarzR(a, b, grid = NULL, dt = 1)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
\item{grid}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
functions collinear as vectors in function space.
}
\value{
A list with \code{lhs}, \code{rhs}, \code{holds}, \code{equality}, \code{k}, \code{collinear}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SchwarzR(V, V)
}
