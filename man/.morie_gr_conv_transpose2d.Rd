% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_conv_transpose2d}
\alias{.morie_gr_conv_transpose2d}
\title{.morie_gr_conv_transpose2d}
\usage{
.morie_gr_conv_transpose2d(Z, K, s)
}
\arguments{
\item{Z}{A matrix; indexed by row and column.}
\item{K}{A matrix; passed to \code{nrow}.}
\item{s}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_convolutional_autoencoder}, \code{morie_geron_dcgan_generator}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
