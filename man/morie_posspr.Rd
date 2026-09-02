% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_posspr}
\alias{morie_posspr}
\alias{sample_urn}
\title{Draw a Polya urn sequence of cluster labels}
\usage{
morie_posspr(n, alpha, seed = 0) 

sample_urn(n, alpha, seed = 0) 
}
\arguments{
\item{n}{Number of draws, at least 1.}
\item{alpha}{Concentration, positive.}
\item{seed}{Seed for the generator shared with the Python arm.}
}
\description{
Repeatedly draws the next cluster label from the urn: with probability \code{alpha/(alpha+n)} a brand new cluster appears, otherwise an existing one is repeated in proportion to its current occupancy. Ties between draws therefore have positive probability, which is what makes the DP a clustering prior at all, and why a DP mixture (not a DP) is used for continuous data.
}
\value{
A list with \code{labels}, \code{counts},   \code{n_clusters}, \code{alpha}, \code{note}.
}
\keyword{internal}
\examples{
morie_posspr(n = 5L, alpha = 0.5)
}
