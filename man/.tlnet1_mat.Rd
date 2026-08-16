% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlnet1_mat}
\alias{.tlnet1_mat}
\title{Private helper: coerce to a list of numeric row-vectors (equivalent}
\usage{
.tlnet1_mat(W)
}
\arguments{
\item{W}{A matrix; indexed by row and column.}
}
\description{
of k.mat) Part of the tlnet1_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tlnet1_mat(V)
}
