% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dp_unseed}
\alias{.morie_dp_unseed}
\title{.morie_dp_unseed}
\usage{
.morie_dp_unseed(old)
}
\arguments{
\item{old}{See Usage.}
}
\description{
A step of the dp_native implementation. Called by \code{morie_approx_dp}, \code{morie_autoencoder_anomaly}, \code{morie_boot_nonoverlap_block} and 18 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dp_unseed(V)
}
