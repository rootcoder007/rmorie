% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_item_characteristic_curve}
\alias{morie_item_characteristic_curve}
\title{morie_item_characteristic_curve}
\usage{
morie_item_characteristic_curve(theta, a = 1, b = 0, c = 0) 
}
\description{
morie_item_characteristic_curve
}
\arguments{
\item{theta}{Argument \code{theta}; see Usage.}
\item{a}{Argument \code{a}; see Usage.}
\item{b}{Argument \code{b}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
}
\value{
A list with \code{p}, \code{logit}, \code{theta}, \code{a}, \code{b}, \code{c}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_item_characteristic_curve(V)
}
