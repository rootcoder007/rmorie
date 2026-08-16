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
\item{prompt_tokens}{Passed to \code{decode_mask}.}
\item{image_tokens}{Passed to \code{decode_mask}.}
\item{grid_shape}{Passed to \code{decode_mask}.}
\item{n_blocks}{Passed to \code{decode_mask}. Defaults to \code{2}.}
\item{upsample_factor}{Passed to \code{decode_mask}. Defaults to \code{2}.}
\item{output_index}{Passed to \code{decode_mask}. Defaults to \code{0}.}
}
\description{
A step of the samdec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{decode_mask}.
}
\keyword{internal}
