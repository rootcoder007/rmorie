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
\item{phi}{A matrix; passed to \code{as.matrix}.}
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}.}
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
