% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dynamic_mask_head}
\alias{dynamic_mask_head}
\title{dynamic_mask_head}
\usage{
dynamic_mask_head(output_token, image_grid_vectors, mlp = NULL)
}
\arguments{
\item{output_token}{Passed to \code{.samdec_vec}.}
\item{image_grid_vectors}{A matrix; passed to \code{as.matrix}.}
\item{mlp}{Defaults to \code{NULL}.}
}
\description{
A step of the samdec_native implementation. Called by \code{decode_mask}. See the file header for the source the module follows.
}
\value{
A list with \code{logits}, \code{probability}, \code{weights}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
dynamic_mask_head(S, M)
}
