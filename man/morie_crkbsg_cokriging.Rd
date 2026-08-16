% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_crkbsg_cokriging}
\alias{morie_crkbsg_cokriging}
\alias{morie_crkbsg}
\title{Ordinary cokriging under a linear model of}
\usage{
morie_crkbsg_cokriging(coords, y, z, s_predict, cross_variogram = NULL, coords_z = NULL)
}
\description{
R arm of crkbsg -- ordinary cokriging under a linear model of Natural Resources Evaluation, OUP, Sec. 6.2.
}
\value{
A list with components \code{n1}, \code{b22}, \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{kriging_prediction}, \code{kriging_variance}, \code{variance_reduction}, \code{weights_primary}, \code{weights_secondary}, \code{lagrange}, \code{targets}, \code{coregionalisation}, \code{byrow}, \code{nugget_matrix}, \code{model}, \code{range}, \code{n_primary}, \code{n_secondary}, \code{method}, \code{note}, \code{b12}.
}
\references{
coregionalisation. Wackernagel, H. (2003) Multivariate Geostatistics,
3rd ed., Springer, Ch. 24-25; Goovaerts, P. (1997) Geostatistics for
}
\keyword{internal}
