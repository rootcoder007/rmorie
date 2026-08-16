% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_poly4}
\alias{.t4_poly4}
\title{Degree-4 polynomial fit; coefficients and residual MSE}
\usage{
.t4_poly4(mp, val)
}
\arguments{
\item{mp}{A vector; its length is taken.}
\item{val}{Passed to \code{.t4_olsfit}.}
}
\description{
A step of the causrddm implementation. Called by \code{Rddmanip}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{mse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_poly4(V, V)
}
