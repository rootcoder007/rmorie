% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{composite_distance}
\alias{composite_distance}
\title{composite_distance}
\usage{
composite_distance(tnf_a, tnf_b, cov_a = NULL, cov_b = NULL, len_a = NULL, len_b = NULL, w_abundance = 0.5)
}
\arguments{
\item{tnf_a}{Passed to \code{.metabd_vec}.}
\item{tnf_b}{Passed to \code{.metabd_vec}.}
\item{cov_a}{Optional; may be \code{NULL}. Passed to \code{.metabd_vec}.}
\item{cov_b}{Defaults to \code{NULL}.}
\item{len_a}{Defaults to \code{NULL}.}
\item{len_b}{Defaults to \code{NULL}.}
\item{w_abundance}{Defaults to \code{0.5}.}
}
\description{
A step of the metabd_native implementation. Called by \code{bin_contigs}, \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{distance}, \code{composition}, \code{abundance}, \code{abundance_usable}, \code{confidence}, \code{effective_weight}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
composite_distance(V, V)
}
