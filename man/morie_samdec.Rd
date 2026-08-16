% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_samdec}
\alias{morie_samdec}
\title{morie_samdec}
\usage{
morie_samdec(prompt_tokens, image_tokens, grid_shape, n_blocks = 2, upsample_factor = 2, output_index = 0)
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
The value of \code{decode_mask}.
}
\keyword{internal}
