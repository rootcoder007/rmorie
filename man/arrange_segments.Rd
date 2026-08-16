% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{arrange_segments}
\alias{arrange_segments}
\title{arrange_segments}
\usage{
arrange_segments(segments)
}
\arguments{
\item{segments}{A vector; indexed elementwise.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{morie_mafft}, \code{progressive_align}. See the file header for the source the module follows.
}
\value{
A vector, from \code{rev}.
}
\keyword{internal}
\examples{
segs <- list(c(0L, 0L, 4L, 5L), c(5L, 6L, 3L, 3L), c(2L, 1L, 2L, 4L))
arrange_segments(segs)
}
