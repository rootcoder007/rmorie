% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_single_marker}
\alias{morie_single_marker}
\title{Single-marker regression LOD}
\usage{
morie_single_marker(y, g)
}
\arguments{
\item{y}{Numeric phenotype vector.}
\item{g}{Numeric marker-genotype vector (same length as \code{y}).}
}
\description{
Regression of the phenotype on one marker genotype; returns the LOD both from the likelihood ratio and from the closed form \code{(n/2) log10(RSS0 / RSS1)}.
}
\value{
A named list with keys \code{estimate}, \code{lod},   \code{lod_likelihood}, \code{a}, \code{b}, \code{sigma2},   \code{rss}, \code{rss_null}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_single_marker(V, V)
}
