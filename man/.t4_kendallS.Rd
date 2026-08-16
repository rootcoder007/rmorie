% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_kendallS}
\alias{.t4_kendallS}
\title{.t4_kendallS}
\usage{
.t4_kendallS(x, y)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
}
\description{
A step of the tail4_core implementation. Called by \code{.t4_kendalltaub}. See the file header for the source the module follows.
}
\value{
The value of \code{S}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_kendallS(V, V)
}
