% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02z}
\alias{k02z}
\title{k02z}
\usage{
k02z(p)
}
\arguments{
\item{p}{See Usage.}
}
\description{
A step of the k02util implementation. Called by \code{mabay}, \code{macum}, \code{mafix} and 9 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{stats::qnorm}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02z(V)
}
