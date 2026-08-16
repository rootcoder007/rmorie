% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rn_ks_stat}
\alias{.rn_ks_stat}
\title{Internal: KS statistic for a sorted sample against CDF values}
\usage{
.rn_ks_stat(cdf_vals)
}
\arguments{
\item{cdf_vals}{A vector; its length is taken.}
}
\description{
A step of the reds_native implementation. Called by \code{.rn_mc_p_fitted}, \code{morie_multi_horizon_ks}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
