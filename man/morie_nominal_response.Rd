% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_nominal_response}
\alias{morie_nominal_response}
\title{morie_nominal_response}
\usage{
morie_nominal_response(theta, a, c) 
}
\description{
morie_nominal_response
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{a}{Argument \code{a}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
}
\value{
A list with \code{p}, \code{expected}, \code{info}, \code{test_info}, \code{theta}, \code{a}, \code{c}, \code{ncat}, \code{n}, \code{k}, \code{method}.
}
\keyword{internal}
\examples{
rmorie:::morie_nominal_response(theta = 0.5, a = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), c = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
