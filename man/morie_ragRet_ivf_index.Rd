% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ragRet_ivf_index}
\alias{morie_ragRet_ivf_index}
\title{morie_ragRet_ivf_index}
\usage{
morie_ragRet_ivf_index(corpus, n.cells = 4L, iters = 25L, seed = 0L)
}
\arguments{
\item{corpus}{See Usage.}
\item{n.cells}{Defaults to \code{4L}.}
\item{iters}{Defaults to \code{25L}.}
\item{seed}{Defaults to \code{0L}.}
}
\description{
A step of the ragRet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{centroids}, \code{lists}, \code{assign}, \code{n.cells}, \code{n}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ragRet_ivf_index(V)
}
