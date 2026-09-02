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
\item{masks}{A vector; its length is taken.}
\item{predicted_iou}{Passed to \code{unlist}.}
\item{target}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the sammkr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rank_masks(V, V)
}
