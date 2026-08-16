% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_separable_covariance}
\alias{.schab_st_separable_covariance}
\title{.schab_st_separable_covariance}
\usage{
.schab_st_separable_covariance(h, k, cov_spatial, cov_temporal, form = "product")
}
\arguments{
\item{h}{Passed to \code{.schab_st_as_lags}.}
\item{k}{Passed to \code{.schab_st_as_lags}.}
\item{cov_spatial}{Accepted by the signature and not used anywhere in the body.}
\item{cov_temporal}{Accepted by the signature and not used anywhere in the body.}
\item{form}{The body requires: `form` must be 'product', 'sum' or 'product_sum'. Defaults to \code{"product"}.}
}
\description{
A step of the schab_st_shared implementation. Called by \code{spstcv}. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
