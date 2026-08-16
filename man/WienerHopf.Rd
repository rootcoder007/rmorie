% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerHopf}
\alias{WienerHopf}
\title{Eq. (3.168): Phi w = Theta.  At the solution the input vector and the}
\usage{
WienerHopf(phi, theta)
}
\arguments{
\item{phi}{See Usage.}
\item{theta}{See Usage.}
}
\description{
error are orthogonal, and so are the output and the error -- the orthogonality principle, which is what makes the solution optimal.
}
\value{
A list with \code{w}, \code{residual}, \code{max_residual}, \code{order}, \code{condition}, \code{orthogonality}, \code{method}.
}
\keyword{internal}
\examples{
WienerHopf(phi = 0.5, theta = 0.5)
}
