% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_prelu}
\alias{morie_geron_prelu}
\title{Parametric ReLU: per-channel learnable negative slope (Geron Ch 11, hmprel)}
\usage{
morie_geron_prelu(z, alpha = 0.25, upstream = NULL)
}
\arguments{
\item{z}{Pre-activations, channels on last axis.}
\item{alpha}{Scalar or per-channel slope.}
\item{upstream}{Optional dL/da from the next layer.}
}
\description{
Parametric ReLU: per-channel learnable negative slope (Geron Ch 11, hmprel)
}
\value{
A list with \code{a}, \code{output}, \code{grad_z}, \code{grad_alpha}, \code{alpha}, \code{negative_fraction}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_prelu(V)
}
