% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.k02relabel}
\alias{.k02relabel}
\title{.k02relabel}
\usage{
.k02relabel(comm)
}
\arguments{
\item{comm}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the sgtcoml implementation. Called by \code{louv}, \code{sgtcoml}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.k02relabel(V)
}
