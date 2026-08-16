% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{group_align}
\alias{group_align}
\title{group_align}
\usage{
group_align(group1, group2, scoring, weights1 = NULL, weights2 = NULL, s_op = 2.4, anchors = NULL)
}
\arguments{
\item{group1}{Iterated over elementwise, with \code{vapply}.}
\item{group2}{Iterated over elementwise, with \code{vapply}.}
\item{scoring}{A list; the body reads \code{$matrix} from it.}
\item{weights1}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{weights2}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{s_op}{Passed to \code{.mafft_nw}. Defaults to \code{2.4}.}
\item{anchors}{Optional; may be \code{NULL}. Iterated over elementwise, with \code{lapply}.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{morie_mafft}, \code{progressive_align}. See the file header for the source the module follows.
}
\value{
The value of \code{.mafft_nw}.
}
\keyword{internal}
\examples{
sc <- normalized_similarity_matrix(seq_type = "aa")
r <- group_align(list("MKVLA"), list("MKVLG"), sc)
str(r, max.level = 1)
}
