% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bin_contigs}
\alias{bin_contigs}
\title{bin_contigs}
\usage{
bin_contigs(tnfs, coverages = NULL, lengths = NULL, threshold = 0.15, min_bin_size = 2e+05)
}
\arguments{
\item{tnfs}{See Usage.}
\item{coverages}{Defaults to \code{NULL}.}
\item{lengths}{Defaults to \code{NULL}.}
\item{threshold}{Defaults to \code{0.15}.}
\item{min_bin_size}{Defaults to \code{2e+05}.}
}
\description{
Part of the metabd_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{bins}, \code{unbinned}, \code{n_bins}, \code{n_unbinned}, \code{method}, \code{note}.
}
\keyword{internal}
