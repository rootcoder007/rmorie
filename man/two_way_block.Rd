% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{two_way_block}
\alias{two_way_block}
\title{two_way_block}
\usage{
two_way_block(prompt_tokens, image_tokens)
}
\arguments{
\item{prompt_tokens}{Passed to \code{.samdec_mat}.}
\item{image_tokens}{Passed to \code{.samdec_mat}.}
}
\description{
A step of the samdec_native implementation. Called by \code{decode_mask}. See the file header for the source the module follows.
}
\value{
A list with \code{prompt_tokens}, \code{image_tokens}, \code{prompt_to_image}, \code{image_to_prompt}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
two_way_block(V, V)
}
