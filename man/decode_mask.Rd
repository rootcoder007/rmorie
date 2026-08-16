% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{decode_mask}
\alias{decode_mask}
\title{decode_mask}
\usage{
decode_mask(prompt_tokens, image_tokens, grid_shape, n_blocks = 2, upsample_factor = 2, output_index = 0)
}
\arguments{
\item{prompt_tokens}{Passed to \code{.samdec_mat}.}
\item{image_tokens}{Passed to \code{.samdec_mat}.}
\item{grid_shape}{A vector; indexed elementwise.}
\item{n_blocks}{Defaults to \code{2}.}
\item{upsample_factor}{Defaults to \code{2}.}
\item{output_index}{Defaults to \code{0}.}
}
\description{
A step of the samdec_native implementation. Called by \code{morie_samdec}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mask}, \code{logits}, \code{shape}, \code{n_blocks}, \code{method}, \code{note}.
}
\keyword{internal}
