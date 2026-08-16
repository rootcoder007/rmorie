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
\item{x}{Passed to \code{.tf_need}.}
\item{wavelet}{Passed to \code{.tf_filters}. Defaults to \code{"db1"}.}
\item{levels}{Defaults to \code{3}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{variances}, \code{scales}, \code{n_used}, \code{total_variance}, \code{sample_variance}, \code{dominant_scale}, \code{is_allan}, \code{wavelet}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WtVar(V)
}
