% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.scalar}
\alias{.scalar}
\title{.scalar}
\usage{
.scalar(state)
}
\arguments{
\item{state}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the prtcl_native implementation. Called by \code{morie_prtcl_particle_filter}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.scalar(V)
}
