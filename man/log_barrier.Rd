% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{log_barrier}
\alias{log_barrier}
\title{Log barrier}
\usage{
log_barrier(fvals) 
}
\arguments{
\item{fvals}{Numeric vector of \eqn{f_i(x)} values.}
}
\description{
Boyd equation 11.5: \code{phi(x) = -sum log(-f_i(x))}. Returns \code{+Inf} outside the strict interior.
}
\value{
Scalar barrier value, or \code{Inf} if any entry is >= 0.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
log_barrier(V)
}
