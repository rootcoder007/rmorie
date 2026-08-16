% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dp_seed}
\alias{.morie_dp_seed}
\title{.morie_dp_seed}
\usage{
.morie_dp_seed(seed)
}
\arguments{
\item{seed}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the dp_native implementation. Called by \code{morie_approx_dp}, \code{morie_autoencoder_anomaly}, \code{morie_boot_nonoverlap_block} and 18 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{old}, as built in the body.
}
\keyword{internal}
