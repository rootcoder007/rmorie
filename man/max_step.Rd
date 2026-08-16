% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{max_step}
\alias{max_step}
\title{max_step}
\usage{
max_step(v, dv, eta = 0.9995)
}
\arguments{
\item{v}{See Usage.}
\item{dv}{See Usage.}
\item{eta}{Defaults to \code{0.9995}.}
}
\description{
Part of the mehtad_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
max_step(V, V)
}
