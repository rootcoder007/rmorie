% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{normalise_bbox}
\alias{normalise_bbox}
\title{normalise_bbox}
\usage{
normalise_bbox(box, width, height, scale = 1000)
}
\arguments{
\item{box}{A vector; its length is taken and its elements indexed.}
\item{width}{See Usage.}
\item{height}{See Usage.}
\item{scale}{Defaults to \code{1000}.}
}
\description{
A step of the ocrwit_native implementation. Called by \code{patch_of_box}, \code{segment_layout_boxes}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
normalise_bbox(box = c(1, 2, 3, 4, 5, 6, 7, 8), width = 5L, height = 5L)
}
