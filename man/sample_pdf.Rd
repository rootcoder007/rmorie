% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sample_pdf}
\alias{sample_pdf}
\title{sample_pdf}
\usage{
sample_pdf(bins, weights, n_samples, seed = 0, eps = 1e-05)
}
\arguments{
\item{bins}{See Usage.}
\item{weights}{See Usage.}
\item{n_samples}{See Usage.}
\item{seed}{Defaults to \code{0}.}
\item{eps}{Defaults to \code{1e-05}.}
}
\description{
Part of the nrfrad_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
sample_pdf(bins = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8), n_samples = 5L)
}
