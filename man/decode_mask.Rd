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
\item{prompt_tokens}{See Usage.}
\item{image_tokens}{See Usage.}
\item{grid_shape}{See Usage.}
\item{n_blocks}{Defaults to \code{2}.}
\item{upsample_factor}{Defaults to \code{2}.}
\item{output_index}{Defaults to \code{0}.}
}
\description{
Part of the samdec_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{mask}, \code{logits}, \code{shape}, \code{n_blocks}, \code{method}, \code{note}.
}
\keyword{internal}
