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
\item{weights}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{seq_type}{Compared against \code{"nt"}. Defaults to \code{"aa"}.}
}
\description{
A step of the mafft_native implementation. Called by \code{correlation}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A list with \code{vol}, \code{pol}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
residue_vectors(V)
}
