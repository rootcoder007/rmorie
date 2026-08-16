% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{remainder_bound}
\alias{remainder_bound}
\title{remainder_bound}
\usage{
remainder_bound(err_Q, err_g, delta)
}
\arguments{
\item{err_Q}{See Usage.}
\item{err_g}{See Usage.}
\item{delta}{See Usage.}
}
\description{
Part of the tlhaltm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{bound}, \code{delta}, \code{err_Q}, \code{err_g}, \code{note}.
}
\keyword{internal}
\examples{
remainder_bound(err_Q = c(1, 2, 3, 4, 5, 6, 7, 8), err_g = c(1, 2, 3, 4, 5, 6, 7, 8), delta = 0.5)
}
