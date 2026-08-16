% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlboot}
\alias{morie_tlboot}
\title{morie_tlboot}
\usage{
morie_tlboot(data, estimator, B = 200L, seed = 0L, method = c("naive", "targeted", "multiplier"), ic = NULL)
}
\arguments{
\item{data}{See Usage.}
\item{estimator}{See Usage.}
\item{B}{A vector; its length is taken. Defaults to \code{200L}.}
\item{seed}{Defaults to \code{0L}.}
\item{method}{One of \code{"naive"}, \code{"targeted"}.}
\item{ic}{Defaults to \code{NULL}.}
}
\description{
A step of the tlboot_native implementation. Called by \code{multiplier_bootstrap}, \code{naive_bootstrap}, \code{targeted_bootstrap}. See the file header for the source the module follows.
}
\value{
A list with \code{replicates}, \code{mean}, \code{se}, \code{influence_curve_se}, \code{ratio}, \code{note}.
}
\keyword{internal}
