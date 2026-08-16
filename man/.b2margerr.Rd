% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2margerr}
\alias{.b2margerr}
\title{.b2margerr}
\usage{
.b2margerr(T, a, b)
}
\arguments{
\item{T}{Passed to \code{rowSums}.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Otsinkh}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.b2margerr(T = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
