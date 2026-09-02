% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{resample}
\alias{resample}
\title{resample}
\usage{
resample(weights, rng, scheme = "systematic") 
}
\arguments{
\item{weights}{Numeric; passed to \code{sum}.}
\item{rng}{Passed to \code{.ghc_unif}.}
\item{scheme}{One of \code{"multinomial"}, \code{"residual"}, \code{"stratified"}, \code{"systematic"}. Defaults to \code{"systematic"}.}
}
\description{
A step of the smcsam_native implementation. Called by \code{morie_btdbl}, \code{morie_btiseq}, \code{smcsam}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
resample(weights = 5L, rng = list(a = 1, b = 2))
}
