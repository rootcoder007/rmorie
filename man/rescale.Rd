% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rescale}
\alias{rescale}
\title{Map the outcome to [0,1] by an affine transform}
\usage{
rescale(y, lower = NULL, upper = NULL)
}
\arguments{
\item{y}{Outcome vector.}
\item{lower}{Lower bound (default: minimum of \code{y}).}
\item{upper}{Upper bound (default: maximum of \code{y}).}
}
\description{
Map the outcome to [0,1] by an affine transform
}
\value{
A list with \code{scaled}, \code{lower}, \code{upper},   \code{range}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rescale(V)
}
