% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_domain_total}
\alias{.morie_domain_total}
\title{.morie_domain_total}
\usage{
.morie_domain_total(y, w, mask)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{w}{A vector; indexed elementwise.}
\item{mask}{Passed to \code{any}.}
}
\description{
Part of the median_frames_dose_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
.morie_domain_total(y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), mask = c(1, 2, 3, 4, 5, 6, 7, 8))
}
