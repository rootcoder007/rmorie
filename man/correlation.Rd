% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{correlation}
\alias{correlation}
\title{correlation}
\usage{
correlation(group1, group2, weights1 = NULL, weights2 = NULL, seq_type = "aa", method = "fft")
}
\arguments{
\item{group1}{Passed to \code{residue_vectors}.}
\item{group2}{Passed to \code{residue_vectors}.}
\item{weights1}{Passed to \code{residue_vectors}.}
\item{weights2}{Passed to \code{residue_vectors}.}
\item{seq_type}{Passed to \code{residue_vectors}. Defaults to \code{"aa"}.}
\item{method}{One of \code{"direct"}, \code{"fft"}. Defaults to \code{"fft"}.}
}
\description{
A step of the mafft_native implementation. Called by \code{find_homologous_segments}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A list with \code{lags}, \code{c}, \code{size}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
correlation(V, V)
}
