% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{purity_completeness}
\alias{purity_completeness}
\title{purity_completeness}
\usage{
purity_completeness(bins, truth)
}
\arguments{
\item{bins}{See Usage.}
\item{truth}{See Usage.}
}
\description{
Part of the metabd_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{per_bin}, \code{mean_purity}, \code{mean_completeness}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
purity_completeness(V, V)
}
