% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{residue_vectors}
\alias{residue_vectors}
\title{residue_vectors}
\usage{
residue_vectors(group, weights = NULL, seq_type = "aa")
}
\arguments{
\item{group}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
\item{seq_type}{Defaults to \code{"aa"}.}
}
\description{
Part of the mafft_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{vol}, \code{pol}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
residue_vectors(V)
}
