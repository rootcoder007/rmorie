% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lrankcov}
\alias{Lrankcov}
\title{Covariance of two linear rank statistics -- Theorem 7.3.3, p. 279}
\usage{
Lrankcov(a, b, m, n) 
}
\description{
Covariance of two linear rank statistics -- Theorem 7.3.3, p. 279
}
\arguments{
\item{a}{Argument \code{a}; see Usage.}
\item{b}{Argument \code{b}; see Usage.}
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{cov}, \code{corr}, \code{var_a}, \code{var_b}, \code{N}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Lrankcov(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), m = 3L, n = 5L)
}
