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
\item{truth}{Passed to \code{unlist}.}
}
\description{
A step of the metabd_native implementation. Called by \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{per_bin}, \code{mean_purity}, \code{mean_completeness}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
purity_completeness(V, V)
}
