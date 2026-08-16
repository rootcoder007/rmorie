% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_nn}
\alias{.sp_nn}
\title{Internal: nearest-neighbour distances h_i.}
\usage{
.sp_nn(points)
}
\arguments{
\item{points}{Event coordinates (n by 2).}
}
\description{
Internal: nearest-neighbour distances h_i.
}
\value{
Numeric vector of length n.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sp_nn(V)
}
