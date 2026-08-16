% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{patch_of_box}
\alias{patch_of_box}
\title{patch_of_box}
\usage{
patch_of_box(box, width, height, patch_grid = 14)
}
\arguments{
\item{box}{Passed to \code{normalise_bbox}.}
\item{width}{Passed to \code{normalise_bbox}.}
\item{height}{Passed to \code{normalise_bbox}.}
\item{patch_grid}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{14}.}
}
\description{
A step of the ocrwit_native implementation. Called by \code{word_patch_alignment}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
patch_of_box(box = c(1, 2, 3, 4, 5, 6, 7, 8), width = 5L, height = 5L)
}
