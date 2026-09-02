% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gaussian_variogram_model}
\alias{morie_gaussian_variogram_model}
\title{morie_gaussian_variogram_model}
\usage{
morie_gaussian_variogram_model(h, c0 = 0, c = 1, a = 1) 
}
\description{
morie_gaussian_variogram_model
}
\arguments{
\item{h}{Argument \code{h}; see Usage.}
\item{c0}{Argument \code{c0}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
\item{a}{Argument \code{a}; see Usage.}
}
\value{
A list with \code{gamma}, \code{covariance}, \code{h}, \code{c0}, \code{c}, \code{a}, \code{sill}, \code{practical_range}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_gaussian_variogram_model(V)
}
