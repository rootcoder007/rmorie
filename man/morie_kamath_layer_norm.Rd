% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_layer_norm}
\alias{morie_kamath_layer_norm}
\title{morie_kamath_layer_norm}
\usage{
morie_kamath_layer_norm(h_i, mu = NULL, sigma = NULL, g = 1, eps = 1e-05)
}
\arguments{
\item{h_i}{Vector.}
\item{mu,sigma}{Optional pinned statistics.}
\item{g}{Gain.}
\item{eps}{Unused stabiliser kept for the signature.}
}
\description{
morie_kamath_layer_norm
}
\value{
A list with \code{output}, \code{normalised}, \code{mu}, \code{sigma}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_layer_norm(V)
}
