% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{word_patch_alignment}
\alias{word_patch_alignment}
\title{word_patch_alignment}
\usage{
word_patch_alignment(text_boxes, masked_patches, width, height, patch_grid = 14, masked_text = list())
}
\arguments{
\item{text_boxes}{A vector; its length is taken and its elements indexed.}
\item{masked_patches}{Passed to \code{unlist}.}
\item{width}{Passed to \code{patch_of_box}.}
\item{height}{Passed to \code{patch_of_box}.}
\item{patch_grid}{Passed to \code{patch_of_box}. Defaults to \code{14}.}
\item{masked_text}{Passed to \code{unlist}. Defaults to \code{list()}.}
}
\description{
A step of the ocrwit_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{labels}, \code{patches}, \code{n_examples}, \code{positive_rate}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
text_boxes <- list(c(10, 10, 60, 30), c(70, 10, 120, 30))
word_patch_alignment(text_boxes, list(0, 5), width = 200, height = 100)
}
