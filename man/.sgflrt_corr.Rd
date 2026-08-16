% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_corr}
\alias{.sgflrt_corr}
\title{.sgflrt_corr}
\usage{
.sgflrt_corr(h, model, phi, kappa)
}
\arguments{
\item{h}{Numeric; combined arithmetically in the body.}
\item{model}{One of \code{"exponential"}, \code{"gaussian"}, \code{"matern"}, \code{"spherical"}.}
\item{phi}{Numeric; combined arithmetically in the body.}
\item{kappa}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{morie_sgflrt_spatial_glmm_fit}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
