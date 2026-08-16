% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_moran_moments}
\alias{.schab_moran_moments}
\title{.schab_moran_moments}
\usage{
.schab_moran_moments(z, w)
}
\arguments{
\item{z}{A vector; its length is taken.}
\item{w}{Passed to \code{.schab_weight_sums}.}
}
\description{
A step of the schab_rest_shared implementation. Called by \code{spmenv}. See the file header for the source the module follows.
}
\value{
A list with \code{I}, \code{expectation}, \code{variance_normal}, \code{variance_randomization}, \code{sd_normal}, \code{sd_randomization}, \code{z_normal}, \code{z_randomization}, \code{kurtosis_b}, \code{S0}, \code{S1}, \code{S2}, \code{n}, \code{geary_c}, \code{geary_expectation}.
}
\keyword{internal}
