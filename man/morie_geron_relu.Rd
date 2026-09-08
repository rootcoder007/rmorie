% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_relu}
\alias{morie_geron_relu}
\title{Rectified linear unit activation, with a leaky negative slope and dead-fraction diagnostic (Geron Ch 9, hmrelu)}
\usage{
morie_geron_relu(z, leaky = 0) 
}
\arguments{
\item{z}{Pre-activations.}
\item{leaky}{Negative slope; 0 is plain ReLU.}
}
\description{
Rectified linear unit activation, with a leaky negative slope and dead-fraction diagnostic (Geron Ch 9, hmrelu)
}
\value{
A list with \code{a}, \code{output}, \code{gradient}, \code{dead_fraction}, \code{leaky}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_relu(V)
}
