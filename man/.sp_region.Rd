% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_region}
\alias{.sp_region}
\title{Internal: bounding region as c(xmin, ymin, xmax, ymax).}
\usage{
.sp_region(region = NULL, points = NULL)
}
\arguments{
\item{region}{Either a length-4 box or an (m, 2) matrix of vertices.}
\item{points}{Fallback point set whose bounding box is used.}
}
\description{
Internal: bounding region as c(xmin, ymin, xmax, ymax).
}
\value{
Numeric vector of length 4.
}
\keyword{internal}
