% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_segment_merge}
\alias{morie_crsfmr_segment_merge}
\title{Merge adjacent segments (one step up the hierarchy)}
\usage{
morie_crsfmr_segment_merge(Z, factor = 2L)
}
\arguments{
\item{Z}{A length-L list of length-D lists of d_model vectors.}
\item{factor}{Integer merge factor.}
}
\description{
The encoder\'s upper layer sees half as many segments, each covering twice the span.
}
\value{
A length-(L/factor) list.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_crsfmr_segment_merge(V)
}
