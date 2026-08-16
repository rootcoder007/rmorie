% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_filters}
\alias{.tf_filters}
\title{.tf_filters}
\usage{
.tf_filters(wavelet)
}
\arguments{
\item{wavelet}{Passed to \code{.tf_dbname}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_dwt}, \code{.tf_idwt}, \code{.tf_swt} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{g}, \code{rec_lo}, \code{rec_hi}.
}
\keyword{internal}
