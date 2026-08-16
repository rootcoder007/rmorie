% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wsp_score}
\alias{wsp_score}
\title{wsp_score}
\usage{
wsp_score(alignment, scoring, s_op = 2.4, weights = NULL)
}
\arguments{
\item{alignment}{Iterated over elementwise, with \code{vapply}.}
\item{scoring}{A list; the body reads \code{$matrix} from it.}
\item{s_op}{Numeric; combined arithmetically in the body. Defaults to \code{2.4}.}
\item{weights}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{mafft_alignment}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\keyword{internal}
\examples{
sc <- normalized_similarity_matrix(seq_type = "aa")
aln <- progressive_align(c("MKVLATLLPLAA", "MKVLATLLPLAG",
                           "MRVLATGLPLAA"), sc, use_fft = FALSE)
wsp_score(aln, sc)
}
