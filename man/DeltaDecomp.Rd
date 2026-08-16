% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DeltaDecomp}
\alias{DeltaDecomp}
\title{Eq (3.29): x(t) = integral x(alpha) delta(t - alpha) d alpha.  The}
\usage{
DeltaDecomp(x, t = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
}
\description{
weight at alpha_i is x(alpha_i) times a trapezoidal spacing, so the weights sum to the integral of x rather than to the sum of samples.
}
\value{
A list with \code{locations}, \code{weights}, \code{amplitudes}, \code{total_weight}, \code{integral}, \code{reconstruction_error}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DeltaDecomp(V)
}
