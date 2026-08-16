% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_paths}
\alias{.morie_ml_paths}
\title{.morie_ml_paths}
\usage{
.morie_ml_paths(x, y, children, parents)
}
\arguments{
\item{x}{Carried through into a list the body builds.}
\item{y}{See Usage.}
\item{children}{A vector; indexed elementwise.}
\item{parents}{A vector; indexed elementwise.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}, \code{morie_dseptest}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_ml_paths(x = 5L, y = 5L, children = c(1, 2, 3, 4, 5, 6, 7, 8), parents = c(1, 2, 3, 4, 5, 6, 7, 8))
}
