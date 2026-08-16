% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_anisotropic_correlation}
\alias{.schab_st_anisotropic_correlation}
\title{.schab_st_anisotropic_correlation}
\usage{
.schab_st_anisotropic_correlation(h, k, theta_s, theta_t, corr_fn)
}
\arguments{
\item{h}{Passed to \code{.schab_st_as_lags}.}
\item{k}{Passed to \code{.schab_st_as_lags}.}
\item{theta_s}{Numeric; combined arithmetically in the body.}
\item{theta_t}{Numeric; combined arithmetically in the body.}
\item{corr_fn}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the schab_st_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
