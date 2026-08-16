% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{unsupervised_loss}
\alias{unsupervised_loss}
\title{Sec. 3.2 unsupervised graph-based loss}
\usage{
unsupervised_loss(z_u, z_v, z_negatives)
}
\arguments{
\item{z_u}{Source embedding.}
\item{z_v}{Positive neighbour embedding.}
\item{z_negatives}{List of negative embeddings.}
}
\description{
Sec. 3.2 unsupervised graph-based loss
}
\value{
Scalar negative log-likelihood.
}
\keyword{internal}
