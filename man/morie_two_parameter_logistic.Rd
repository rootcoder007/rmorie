% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_two_parameter_logistic}
\alias{morie_two_parameter_logistic}
\title{morie_two_parameter_logistic}
\usage{
morie_two_parameter_logistic(theta, a = 1, b = 0) 
}
\description{
morie_two_parameter_logistic
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{a}{Argument \code{a}; see Usage.}
\item{b}{Argument \code{b}; see Usage.}
}
\value{
A list with \code{p}, \code{logit}, \code{theta}, \code{a}, \code{b}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_two_parameter_logistic(V)
}
