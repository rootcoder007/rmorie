% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{correlation}
\alias{correlation}
\title{correlation}
\usage{
correlation(group1, group2, weights1 = NULL, weights2 = NULL, seq_type = "aa", method = "fft")
}
\arguments{
\item{group1}{See Usage.}
\item{group2}{See Usage.}
\item{weights1}{Defaults to \code{NULL}.}
\item{weights2}{Defaults to \code{NULL}.}
\item{seq_type}{Defaults to \code{"aa"}.}
\item{method}{Defaults to \code{"fft"}.}
}
\description{
Part of the mafft_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lags}, \code{c}, \code{size}.
}
\keyword{internal}
