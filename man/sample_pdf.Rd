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
\item{bins}{Passed to \code{.nrfrad_vec}.}
\item{weights}{Passed to \code{.nrfrad_vec}.}
\item{n_samples}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{eps}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-05}.}
}
\description{
A step of the nrfrad_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
sample_pdf(bins = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8), n_samples = 5L)
}
