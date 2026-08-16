% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{iou}
\alias{iou}
\title{iou}
\usage{
iou(a, b, threshold = 0.5)
}
\arguments{
\item{a}{Passed to \code{.sammkr_flat}.}
\item{b}{Passed to \code{.sammkr_flat}.}
\item{threshold}{Defaults to \code{0.5}.}
}
\description{
A step of the sammkr_native implementation. Called by \code{rank_masks}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
iou(V, V)
}
