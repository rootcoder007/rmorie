% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{normalized_similarity_matrix}
\alias{normalized_similarity_matrix}
\title{normalized_similarity_matrix}
\usage{
normalized_similarity_matrix(raw_matrix = NULL, freqs = NULL, s_a = 0.06, seq_type = "aa", 
    mode = "normalized", default = "jtt200") 
}
\arguments{
\item{raw_matrix}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{freqs}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{s_a}{Numeric; combined arithmetically in the body. Defaults to \code{0.06}.}
\item{seq_type}{Compared against \code{"nt"}. Defaults to \code{"aa"}.}
\item{mode}{Compared against \code{"all_positive"}. Defaults to \code{"normalized"}.}
\item{default}{One of \code{"grantham"}, \code{"jtt200"}. Defaults to \code{"jtt200"}.}
}
\description{
A step of the mafft_native implementation. Called by \code{mafft_alignment}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A list with \code{matrix}, \code{s_a}, \code{alphabet}, \code{average1}, \code{average2}, \code{freqs}, \code{mode}.
}
\keyword{internal}
\examples{
normalized_similarity_matrix()
}
