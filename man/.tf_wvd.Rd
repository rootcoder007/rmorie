% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_wvd}
\alias{.tf_wvd}
\title{.tf_wvd}
\usage{
.tf_wvd(x, fs, nfreq = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{nfreq}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{Gtfd}, \code{WvDist}. See the file header for the source the module follows.
}
\value{
A list with \code{tfd}, \code{freqs}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tf_wvd(V, V)
}
