% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dwt2Tap}
\alias{Dwt2Tap}
\title{Two-tap (Haar / db1) orthogonal DWT}
\usage{
Dwt2Tap(x, levels = 3)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{approx}, \code{details}, \code{coeffs}, \code{lengths}, \code{levels}, \code{energy}, \code{input_energy}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Dwt2Tap(V)
}
