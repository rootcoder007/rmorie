% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wsrsimpow}
\alias{Wsrsimpow}
\title{Simulated signed-rank power over supplied samples -- Sec. 5.7.3, p. 204}
\usage{
Wsrsimpow(samples, m0, tcrit) 
}
\description{
Simulated signed-rank power over supplied samples -- Sec. 5.7.3, p. 204
}
\arguments{
\item{samples}{Argument \code{samples}; see Usage.}
\item{m0}{Argument \code{m0}; see Usage.}
\item{tcrit}{Argument \code{tcrit}; see Usage.}
}
\value{
A list with \code{power}, \code{rejections}, \code{nsim}, \code{tmean}, \code{tcrit}.
}
\keyword{internal}
\examples{
rmorie:::Wsrsimpow(samples = c(1, 2, 3, 4, 5, 6, 7, 8), m0 = c(1, 2, 3, 4, 5, 6, 7, 8), tcrit = c(1, 2, 3, 4, 5, 6, 7, 8))
}
