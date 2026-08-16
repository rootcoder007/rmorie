% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{S_function}
\alias{S_function}
\title{S_function}
\usage{
S_function(std_moments, form = "sum", n_equality = 0L)
}
\arguments{
\item{std_moments}{See Usage.}
\item{form}{Defaults to \code{"sum"}.}
\item{n_equality}{Defaults to \code{0L}.}
}
\description{
Part of the bnskmt_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
S_function(V)
}
