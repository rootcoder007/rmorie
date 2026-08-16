% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{moments}
\alias{moments}
\title{Entry-distribution moments}
\usage{
moments(distribution = "rademacher")
}
\arguments{
\item{distribution}{One of \code{"rademacher"}, \code{"sparse"}.}
}
\description{
Both Achlioptas distributions have mean \code{0} and variance \code{1} exactly, which is what Theorem 1.1 needs and what this function verifies in closed form rather than by simulation.
}
\value{
A list with \code{mean}, \code{variance},   \code{fourth_moment}, \code{support}, \code{density},   \code{note}.
}
\keyword{internal}
