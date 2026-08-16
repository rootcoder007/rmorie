% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Triangle}
\alias{Triangle}
\title{Eq (4.45): |a+b| <= |a|+|b|, with equality when the vectors align}
\usage{
Triangle(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
It is the statement that no combination of two signals carries more amplitude than their amplitudes sum to -- the reason uncorrelated noise adds in POWER, not in amplitude.
}
\value{
A list with \code{lhs}, \code{rhs}, \code{holds}, \code{equality}, \code{norm_sum}, \code{norm_a}, \code{norm_b}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Triangle(V, V)
}
