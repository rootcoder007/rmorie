% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WtVar}
\alias{WtVar}
\title{Unbiased wavelet variance by scale, Percival (1995)}
\usage{
WtVar(x, wavelet = "db1", levels = 3)
}
\arguments{
\item{x}{See Usage.}
\item{wavelet}{Defaults to \code{"db1"}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{variances}, \code{scales}, \code{n_used}, \code{total_variance}, \code{sample_variance}, \code{dominant_scale}, \code{is_allan}, \code{wavelet}, \code{method}.
}
\keyword{internal}
