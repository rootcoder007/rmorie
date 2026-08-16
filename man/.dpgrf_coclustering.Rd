% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpgrf_coclustering}
\alias{.dpgrf_coclustering}
\title{Posterior coclustering probabilities from sampled labels}
\usage{
.dpgrf_coclustering(label_draws)
}
\arguments{
\item{label_draws}{List of integer label vectors, each of length n.}
}
\description{
For each pair \code{(i, j)} returns the fraction of label draws in which regions \code{i} and \code{j} share a label. The result is symmetric with a unit diagonal by construction -- a cheap invariant that catches an indexing error.
}
\value{
A list with \code{matrix} (n x n), \code{n_draws}, \code{n},   \code{symmetric}, \code{unit_diagonal}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dpgrf_coclustering(V)
}
