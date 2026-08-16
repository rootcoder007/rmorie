% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerOut}
\alias{WienerOut}
\title{Eq. (3.154): the estimate is the convolution of the tap weights with}
\usage{
WienerOut(w, x)
}
\arguments{
\item{w}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
the input.  The first M-1 outputs run on a partly empty delay line and are reported as unsettled rather than trimmed away.
}
\value{
A list with \code{d_hat}, \code{n}, \code{order}, \code{settled_from}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WienerOut(V, V)
}
