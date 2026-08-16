% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sxrhrt_chol}
\alias{.sxrhrt_chol}
\title{.sxrhrt_chol}
\usage{
.sxrhrt_chol(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
A step of the sxrhrt_native implementation. Called by \code{.sxrhrt_reml}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
