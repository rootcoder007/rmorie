% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t1_vec}
\alias{.t1_vec}
\title{.t1_vec}
\usage{
.t1_vec(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the tail1_core implementation. Called by \code{.ecfp_percol}, \code{Admmlasso}, \code{Advielbo} and 154 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t1_vec(V)
}
