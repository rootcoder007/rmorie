% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{abundance_correlation}
\alias{abundance_correlation}
\title{abundance_correlation}
\usage{
abundance_correlation(cov_a, cov_b)
}
\arguments{
\item{cov_a}{Passed to \code{.metabd_vec}.}
\item{cov_b}{Passed to \code{.metabd_vec}.}
}
\description{
A step of the metabd_native implementation. Called by \code{composite_distance}, \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{correlation}, \code{n_samples}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
abundance_correlation(V, V)
}
