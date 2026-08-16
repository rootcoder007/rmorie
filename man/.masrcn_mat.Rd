% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.masrcn_mat}
\alias{.masrcn_mat}
\title{.masrcn_mat}
\usage{
.masrcn_mat(X)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the masrcn_native implementation. Called by \code{mask_loss}, \code{roi_align}, \code{roi_pool}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
