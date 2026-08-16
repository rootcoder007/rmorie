% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Diff2}
\alias{Diff2}
\title{The second derivative has response (jw)(jw) = -w^2, a QUADRATIC rise}
\usage{
Diff2(x, T = 1, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{T}{Defaults to \code{1}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
with frequency, and the book notes it "may be realized as a cascade of two" first differences.  Both are computed and compared.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{T}, \code{as_cascaded_first_differences}, \code{max_difference}, \code{cascade_agrees}, \code{b}, \code{a}, \code{zeros}, \code{double_zero_at_dc}, \code{gain_rises_quadratically}, \code{method}.
}
\keyword{internal}
