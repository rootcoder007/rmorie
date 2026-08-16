% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_mish}
\alias{morie_geron_mish}
\title{Mish activation z*tanh(softplus(z)) (Geron Ch 11, morie.fn hmmish)}
\usage{
morie_geron_mish(z)
}
\arguments{
\item{z}{Pre-activations. @return List with activation, derivative, softplus, minimum, estimate, n, method.}
}
\description{
Mish activation z*tanh(softplus(z)) (Geron Ch 11, morie.fn hmmish)
}
\value{
A list with \code{activation}, \code{derivative}, \code{softplus}, \code{minimum}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_mish(V)
}
