% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Medtestci}
\alias{Medtestci}
\title{Median-test confidence interval -- Sec. 6.4.2, p. 251}
\usage{
Medtestci(x, y, c) 
}
\description{
Median-test confidence interval -- Sec. 6.4.2, p. 251
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
}
\value{
A list with \code{lower}, \code{upper}, \code{estimate}, \code{c}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(3)
rmorie:::Medtestci(rnorm(15), rnorm(15, 0.5), c = 3)
}
