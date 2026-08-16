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
\item{tnfs}{Passed to \code{.metabd_mat}.}
\item{coverages}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{lengths}{Optional; may be \code{NULL}. Passed to \code{.metabd_vec}.}
\item{threshold}{Defaults to \code{0.15}.}
\item{min_bin_size}{Defaults to \code{2e+05}.}
}
\description{
A step of the metabd_native implementation. Called by \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{bins}, \code{unbinned}, \code{n_bins}, \code{n_unbinned}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
bin_contigs(V)
}
