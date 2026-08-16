% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bd_paths}
\alias{.bd_paths}
\title{.bd_paths}
\usage{
.bd_paths(x, y, ch, pa)
}
\arguments{
\item{x}{Carried through into a list the body builds.}
\item{y}{See Usage.}
\item{ch}{A vector; indexed elementwise.}
\item{pa}{A vector; indexed elementwise.}
}
\description{
A step of the causal_native implementation. Called by \code{morie_backdoor_criterion}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.bd_paths(x = 5L, y = 5L, ch = c(1, 2, 3, 4, 5, 6, 7, 8), pa = c(1, 2, 3, 4, 5, 6, 7, 8))
}
