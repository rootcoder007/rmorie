% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{unscale}
\alias{unscale}
\title{Invert the affine map -- the influence curve scales with it}
\usage{
unscale(value, lower, upper) 
}
\arguments{
\item{value}{Value in the rescaled units.}
\item{lower}{Original lower bound.}
\item{upper}{Original upper bound.}
}
\description{
Invert the affine map -- the influence curve scales with it
}
\value{
The value on the original scale.
}
\keyword{internal}
\examples{
unscale(value = c(1, 2, 3, 4, 5, 6, 7, 8), lower = c(1, 2, 3, 4, 5, 6, 7, 8), upper = c(1, 2, 3, 4, 5, 6, 7, 8))
}
