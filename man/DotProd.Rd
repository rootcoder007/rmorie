% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DotProd}
\alias{DotProd}
\title{Eqs (4.24)-(4.25): the inner product and the correlation coefficient}
\usage{
DotProd(x, y, subtract_mean = FALSE)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{subtract_mean}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
it normalizes to.  The book notes the means may be removed first (eq 3.97), which is a DIFFERENT quantity -- without removal gamma is the cosine between the raw vectors, with it, Pearson\'s r.
}
\value{
A list with \code{dot_product}, \code{gamma}, \code{energy_x}, \code{energy_y}, \code{n}, \code{mean_removed}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DotProd(V, V)
}
