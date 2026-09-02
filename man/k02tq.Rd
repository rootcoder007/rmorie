% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{k02tq}
\alias{k02tq}
\title{k02tq}
\usage{
k02tq(p, df) 
}
\arguments{
\item{p}{See Usage.}
\item{df}{See Usage.}
}
\description{
A step of the k02util implementation. Called by \code{mahks}, \code{mahsj}, \code{matau2pi}. See the file header for the source the module follows.
}
\value{
The value of \code{stats::qt}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
k02tq(V, V)
}
