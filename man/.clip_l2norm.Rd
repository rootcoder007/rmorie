% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clip_l2norm}
\alias{.clip_l2norm}
\title{.clip_l2norm}
\usage{
.clip_l2norm(v)
}
\arguments{
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the clipsi implementation. Called by \code{Clipxi}, \code{Colbrt}, \code{Contse} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.clip_l2norm(V)
}
