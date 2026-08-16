% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannFilt}
\alias{HannFilt}
\title{Eq (3.100): y(n) = (1/4)[x(n) + 2x(n-1) + x(n-2)].  Three taps in the}
\usage{
HannFilt(x, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
ratio 1:2:1.  It is a delaying smoother, not a symmetric one: the output lags the input by exactly one sample.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{n}, \code{taps}, \code{delay_samples}, \code{settled_from}, \code{dc_gain}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HannFilt(V)
}
