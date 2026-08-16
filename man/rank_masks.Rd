% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rank_masks}
\alias{rank_masks}
\title{rank_masks}
\usage{
rank_masks(masks, predicted_iou, target = NULL)
}
\arguments{
\item{masks}{See Usage.}
\item{predicted_iou}{See Usage.}
\item{target}{Defaults to \code{NULL}.}
}
\description{
Part of the sammkr_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rank_masks(V, V)
}
