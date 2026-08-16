% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DftK}
\alias{DftK}
\title{Eq (3.79): K need not equal N.  K > N samples the same DTFT more}
\usage{
DftK(x, k_points)
}
\arguments{
\item{x}{See Usage.}
\item{k_points}{See Usage.}
}
\description{
finely; K < N folds and the signal cannot be recovered.
}
\value{
A list with \code{X}, \code{K}, \code{n}, \code{aliased}, \code{method}.
}
\keyword{internal}
\examples{
DftK(x = c(1, 2, 3, 4, 5, 6, 7, 8), k_points = 5L)
}
