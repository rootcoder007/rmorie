% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpgrf_sample_labels}
\alias{.dpgrf_sample_labels}
\title{Sample cluster labels from the Polya urn}
\usage{
.dpgrf_sample_labels(n_regions, alpha, rng = NULL, seed = 0)
}
\arguments{
\item{n_regions}{Number of regions (positive integer).}
\item{alpha}{Positive concentration.}
\item{rng}{Generator environment from \code{.ghc_rng} (or   \code{NULL} to build one with \code{seed = 0}).}
\item{seed}{Seed for the generator shared with the Python arm.}
}
\description{
Mirrors \code{urn.sample_urn}: at each step either start a new cluster with probability \code{alpha / (alpha + n)} or repeat an existing one with probability proportional to its current count. Ties occur with POSITIVE probability, which is the whole reason the DP clusters.
}
\value{
A list with \code{labels}, \code{counts},   \code{n_clusters}, \code{alpha}, \code{note}.
}
\keyword{internal}
\examples{
.dpgrf_sample_labels(n_regions = 5L, alpha = 0.05)
}
