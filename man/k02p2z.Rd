% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02p2z}
\alias{k02p2z}
\title{k02p2z}
\usage{
k02p2z(z)
}
\arguments{
\item{z}{Numeric; passed to \code{abs}.}
}
\description{
A step of the k02util implementation. Called by \code{mafix}, \code{marndm}, \code{matr}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02p2z(V)
}
