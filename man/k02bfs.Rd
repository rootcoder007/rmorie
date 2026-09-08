% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02bfs}
\alias{k02bfs}
\title{k02bfs}
\usage{
k02bfs(A) 
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the k02util implementation. Called by \code{smwgrp}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02bfs(V)
}
