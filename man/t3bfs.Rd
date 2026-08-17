% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{t3bfs}
\alias{t3bfs}
\title{t3bfs}
\usage{
t3bfs(A, s)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
\item{s}{See Usage.}
}
\description{
A step of the t3util implementation. Called by \code{ecccen}. See the file header for the source the module follows.
}
\value{
The value of \code{d}, as built in the body.
}
\keyword{internal}
\examples{
A <- matrix(0, 4, 4)
A[1, 2] <- A[2, 1] <- 1
A[2, 3] <- A[3, 2] <- 1
A[3, 4] <- A[4, 3] <- 1
t3bfs(A, 1)
}
