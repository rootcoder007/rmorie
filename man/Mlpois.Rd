% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mlpois}
\alias{Mlpois}
\title{Multilevel Poisson log-rate (Lawson eq. 15.2)}
\usage{
Mlpois(beta0, beta1, age, race_effect, v, W) 
}
\description{
Multilevel Poisson log-rate (Lawson eq. 15.2)
}
\arguments{
\item{beta0}{Argument \code{beta0}; see Usage.}
\item{beta1}{Argument \code{beta1}; see Usage.}
\item{age}{Argument \code{age}; see Usage.}
\item{race_effect}{Argument \code{race_effect}; see Usage.}
\item{v}{Argument \code{v}; see Usage.}
\item{W}{Argument \code{W}; see Usage.}
}
\value{
A list with \code{lograte}, \code{rate}, \code{n}.
}
\keyword{internal}
\examples{
Mlpois(beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta1 = c(1, 2, 3, 4, 5, 6, 7, 8), age = c(1, 2, 3, 4, 5, 6, 7, 8), race_effect = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8))
}
