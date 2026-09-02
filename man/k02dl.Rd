% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02dl}
\alias{k02dl}
\title{k02dl}
\usage{
k02dl(y, v) 
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the k02util implementation. Called by \code{mabay}, \code{mac3}, \code{macum} and 7 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{tau2}, \code{mu}, \code{var}, \code{Q}, \code{df}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02dl(V, V)
}
