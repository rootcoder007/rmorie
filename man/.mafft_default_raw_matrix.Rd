% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_default_raw_matrix}
\alias{.mafft_default_raw_matrix}
\title{.mafft_default_raw_matrix}
\usage{
.mafft_default_raw_matrix(seq_type, which = "jtt200")
}
\arguments{
\item{seq_type}{Compared against \code{"nt"}.}
\item{which}{Compared against \code{"grantham"}. Defaults to \code{"jtt200"}.}
}
\description{
A step of the mafft_native implementation. Called by \code{normalized_similarity_matrix}. See the file header for the source the module follows.
}
\value{
A list with \code{M}, \code{freqs}.
}
\keyword{internal}
