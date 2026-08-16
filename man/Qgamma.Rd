% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qgamma}
\alias{Qgamma}
\title{Qgamma}
\usage{
Qgamma(p, shape, rate = 1)
}
\arguments{
\item{p}{See Usage.}
\item{shape}{See Usage.}
\item{rate}{Defaults to \code{1}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qchisq}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Qgamma(V, V)
}
