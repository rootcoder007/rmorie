% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.agalfsy_coords}
\alias{.agalfsy_coords}
\title{.agalfsy_coords}
\usage{
.agalfsy_coords(x, what)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{what}{See Usage.}
}
\description{
A step of the agalfsy_native implementation. Called by \code{morie_agalfsy_rl_pose_search}. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.agalfsy_coords(V, V)
}
