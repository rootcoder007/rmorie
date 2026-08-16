% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{triplet_loss}
\alias{triplet_loss}
\title{triplet_loss}
\usage{
triplet_loss(better, worse, src, ref, margin = 1)
}
\arguments{
\item{better}{Passed to \code{.comet_dist}.}
\item{worse}{Passed to \code{.comet_dist}.}
\item{src}{Passed to \code{.comet_dist}.}
\item{ref}{Passed to \code{.comet_dist}.}
\item{margin}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the comet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{source_term}, \code{reference_term}, \code{satisfied}, \code{note}.
}
\keyword{internal}
\examples{
triplet_loss(better = c(1, 2, 3, 4, 5, 6, 7, 8), worse = c(1, 2, 3, 4, 5, 6, 7, 8), src = c(1, 2, 3, 4, 5, 6, 7, 8), ref = c(1, 2, 3, 4, 5, 6, 7, 8))
}
