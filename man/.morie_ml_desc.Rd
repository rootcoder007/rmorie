% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_desc}
\alias{.morie_ml_desc}
\title{.morie_ml_desc}
\usage{
.morie_ml_desc(node, children)
}
\arguments{
\item{node}{See Usage.}
\item{children}{A vector; indexed elementwise.}
}
\description{
A step of the causal_molak implementation. Called by \code{.morie_ml_blocked}, \code{morie_docalc}. See the file header for the source the module follows.
}
\value{
The value of \code{seen}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ml_desc(V, V)
}
