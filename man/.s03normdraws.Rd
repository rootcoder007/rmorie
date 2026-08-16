% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03normdraws}
\alias{.s03normdraws}
\title{.s03normdraws}
\usage{
.s03normdraws(n, base = 2L)
}
\arguments{
\item{n}{Passed to \code{.s03unif}.}
\item{base}{Passed to \code{.s03unif}. Defaults to \code{2L}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.ot_directions}, \code{.vitdraw}, \code{MedCI} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{qnorm}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03normdraws(V)
}
