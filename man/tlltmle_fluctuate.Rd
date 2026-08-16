% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tlltmle_fluctuate}
\alias{tlltmle_fluctuate}
\title{tlltmle_fluctuate}
\usage{
tlltmle_fluctuate(Q, H, Y, iters = 100, tol = 1e-10)
}
\arguments{
\item{Q}{See Usage.}
\item{H}{See Usage.}
\item{Y}{See Usage.}
\item{iters}{Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
A step of the tlltmle_native implementation. Called by \code{tlltmle_ltmle}, \code{tlltmle_tmle_point}. See the file header for the source the module follows.
}
\value{
A list with \code{epsilon}, \code{Q_star}, \code{score}.
}
\keyword{internal}
\examples{
tlltmle_fluctuate(Q = c(1, 2, 3, 4, 5, 6, 7, 8), H = c(1, 2, 3, 4, 5, 6, 7, 8), Y = c(1, 2, 3, 4, 5, 6, 7, 8))
}
