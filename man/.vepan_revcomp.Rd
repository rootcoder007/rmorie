% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_revcomp}
\alias{.vepan_revcomp}
\title{.vepan_revcomp}
\usage{
.vepan_revcomp(s)
}
\arguments{
\item{s}{Character; passed to \code{toupper}.}
}
\description{
A step of the vepan_native implementation. Called by \code{.vepan_apply}, \code{.vepan_hgvs_c}, \code{morie_vepan_transcript_sequence}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vepan_revcomp(V)
}
