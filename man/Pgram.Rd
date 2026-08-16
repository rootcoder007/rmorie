% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pgram}
\alias{Pgram}
\title{Eq (4.57) specialised to R^1 in Sec. 4.7.1.1, checked against the}
\usage{
Pgram(y)
}
\arguments{
\item{y}{See Usage.}
}
\description{
covariance form of eq (4.58).  THE ZERO FREQUENCY IS EXCLUDED: the derivation of (4.58) turns on sum_u cos(w_j u) = 0, true at every Fourier frequency EXCEPT w = 0.
}
\value{
A list with \code{omega}, \code{periodogram}, \code{from_covariance}, \code{max_difference}, \code{acov}, \code{zero_frequency_excluded}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Pgram(V)
}
