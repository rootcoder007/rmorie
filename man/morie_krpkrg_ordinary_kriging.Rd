% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_krpkrg_ordinary_kriging}
\alias{morie_krpkrg_ordinary_kriging}
\alias{morie_krpkrg}
\title{Ordinary kriging with a Lagrange unbiasedness}
\usage{
morie_krpkrg_ordinary_kriging(coords, values, targets, model = "spherical", nugget = 0, sill = 1, rng = 1)
}
\description{
R arm of krpkrg -- ordinary kriging with a Lagrange unbiasedness Evaluation, Oxford University Press, Ch. 5.
}
\value{
A list with components \code{n}, \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{weights}, \code{n_targets}, \code{model}, \code{nugget}, \code{sill}, \code{range}, \code{method}, \code{note}.
}
\references{
constraint. Goovaerts, P. (2005) Geostatistics for Natural Resources
}
\keyword{internal}
