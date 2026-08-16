% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crkbsg_cokriging}
\alias{morie_crkbsg_cokriging}
\title{morie_crkbsg_cokriging}
\usage{
morie_crkbsg_cokriging(coords, y, z, s_predict, cross_variogram = NULL, coords_z = NULL)
}
\arguments{
\item{coords}{See Usage.}
\item{y}{See Usage.}
\item{z}{See Usage.}
\item{s_predict}{See Usage.}
\item{cross_variogram}{Defaults to \code{NULL}.}
\item{coords_z}{Defaults to \code{NULL}.}
}
\description{
Part of the crkbsg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{prediction}, \code{variance}, \code{std_error}, \code{kriging_prediction}, \code{kriging_variance}, \code{variance_reduction}, \code{weights_primary}, \code{weights_secondary}, \code{lagrange}, \code{targets}, \code{coregionalisation}, \code{nugget_matrix}, \code{model}, \code{range}, \code{n_primary}, \code{n_secondary}, \code{method}, \code{note}.
}
\keyword{internal}
