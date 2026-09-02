% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{segment_layout_boxes}
\alias{segment_layout_boxes}
\title{segment_layout_boxes}
\usage{
segment_layout_boxes(boxes, segment_ids, width, height, scale = 1000) 
}
\arguments{
\item{boxes}{A matrix; indexed by row and column.}
\item{segment_ids}{Coerced to list by the body, with \code{as.list}.}
\item{width}{Passed to \code{normalise_bbox}.}
\item{height}{Passed to \code{normalise_bbox}.}
\item{scale}{Passed to \code{normalise_bbox}. Defaults to \code{1000}.}
}
\description{
A step of the ocrwit_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{segment_boxes}, \code{per_token}, \code{n_segments}, \code{note}.
}
\keyword{internal}
\examples{
boxes <- matrix(c(10, 10, 60, 30, 70, 10, 110, 30), 2, 4, byrow = TRUE)
segment_layout_boxes(boxes, c(0, 1), width = 200, height = 100)
}
