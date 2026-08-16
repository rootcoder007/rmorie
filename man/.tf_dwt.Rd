% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_dwt}
\alias{.tf_dwt}
\title{.tf_dwt}
\usage{
.tf_dwt(x, wavelet, levels)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{wavelet}{Passed to \code{.tf_filters}.}
\item{levels}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Dwt}, \code{Dwt2Tap}, \code{Mra} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{approx}, \code{details}, \code{lengths}.
}
\keyword{internal}
