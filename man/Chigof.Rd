% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Chigof}
\alias{Chigof}
\title{Chi-square goodness of fit -- eq. (4.2.1), p. 104}
\usage{
Chigof(observed, expected, ddof = 0) 
}
\description{
Chi-square goodness of fit -- eq. (4.2.1), p. 104
}
\arguments{
\item{observed}{Argument \code{observed}; see Usage.}
\item{expected}{Argument \code{expected}; see Usage.}
\item{ddof}{Argument \code{ddof}; see Usage.}
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{k}, \code{n}, \code{contrib}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Chigof(V, V)
}
