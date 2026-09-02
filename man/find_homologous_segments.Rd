% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{find_homologous_segments}
\alias{find_homologous_segments}
\title{find_homologous_segments}
\usage{
find_homologous_segments(group1, group2, scoring, weights1 = NULL, weights2 = NULL, seq_type = "aa", 
    window = 30L, n_peaks = 20L, threshold = 0.7, max_len = 150L, corr_method = "fft") 
}
\arguments{
\item{group1}{Iterated over elementwise, with \code{vapply}.}
\item{group2}{Iterated over elementwise, with \code{vapply}.}
\item{scoring}{A list; the body reads \code{$matrix} from it.}
\item{weights1}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{weights2}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{seq_type}{Passed to \code{correlation}. Defaults to \code{"aa"}.}
\item{window}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{30L}.}
\item{n_peaks}{Passed to \code{.mafft_peaks}. Defaults to \code{20L}.}
\item{threshold}{Passed to \code{>}. Defaults to \code{0.7}.}
\item{max_len}{Numeric; combined arithmetically in the body. Defaults to \code{150L}.}
\item{corr_method}{Passed to \code{correlation}. Defaults to \code{"fft"}.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{morie_mafft}, \code{progressive_align}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
find_homologous_segments(group1 = c(1, 2, 3, 4, 5, 6, 7, 8), group2 = c(1, 2, 3, 4, 5, 6, 7, 8), scoring = c(1, 2, 3, 4, 5, 6, 7, 8))
}
