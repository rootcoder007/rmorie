% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02fe}
\alias{k02fe}
\title{k02fe}
\usage{
k02fe(y, v)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the k02util implementation. Called by \code{k02dl}, \code{mafix}, \code{mai2} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{mu}, \code{var}, \code{sw}, \code{Q}, \code{df}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02fe(V, V)
}
