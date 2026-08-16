% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encode_mask_prompt}
\alias{encode_mask_prompt}
\title{encode_mask_prompt}
\usage{
encode_mask_prompt(mask, image_embedding, weight = 1)
}
\arguments{
\item{mask}{See Usage.}
\item{image_embedding}{See Usage.}
\item{weight}{Defaults to \code{1}.}
}
\description{
Part of the samseg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{embedding}, \code{sparse}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
encode_mask_prompt(M, M)
}
