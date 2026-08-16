% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_cds_frame}
\alias{.vepan_cds_frame}
\title{CDNA index (0-based) of each coding base, and the coding sequence}
\usage{
.vepan_cds_frame(t, genome)
}
\arguments{
\item{t}{A list; the body reads \code{$cds_end}, \code{$cds_start} from it.}
\item{genome}{Passed to \code{morie_vepan_transcript_sequence}.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
A list with \code{cds}, \code{coding}, \code{sg}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.vepan_cds_frame(D, V)
}
