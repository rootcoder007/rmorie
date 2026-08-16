% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PzMag}
\alias{PzMag}
\title{Eq (3.72): the geometric reading of a filter.  Approaching a zero}
\usage{
PzMag(l_k, r_k, N = NULL, M = NULL)
}
\arguments{
\item{l_k}{See Usage.}
\item{r_k}{See Usage.}
\item{N}{Defaults to \code{NULL}.}
\item{M}{Defaults to \code{NULL}.}
}
\description{
drives the response to nought, approaching a pole drives it up, and a pole ON the evaluation point makes it unbounded -- which is why a vanishing pole distance is refused rather than returning Inf.
}
\value{
A list with \code{magnitude}, \code{zero_product}, \code{pole_product}, \code{n_zeros}, \code{n_poles}, \code{on_a_zero}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PzMag(V, V)
}
