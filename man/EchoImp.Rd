% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoImp}
\alias{EchoImp}
\title{Two-impulse echo excitation, eq (4.74)}
\usage{
EchoImp(a, n_0, n)
}
\arguments{
\item{a}{See Usage.}
\item{n_0}{See Usage.}
\item{n}{See Usage.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{x}, \code{n}, \code{a}, \code{n_0}, \code{method}.
}
\keyword{internal}
\examples{
EchoImp(a = c(1, 2, 3, 4, 5, 6, 7, 8), n_0 = 5L, n = 5L)
}
