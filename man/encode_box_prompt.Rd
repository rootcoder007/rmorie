% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encode_box_prompt}
\alias{encode_box_prompt}
\title{encode_box_prompt}
\usage{
encode_box_prompt(box, dim = 8, type_embeddings = NULL)
}
\arguments{
\item{box}{The body requires: samseg: the box is empty or inverted.}
\item{dim}{Passed to \code{.samseg_pos_enc}. Defaults to \code{8}.}
\item{type_embeddings}{Defaults to \code{NULL}.}
}
\description{
A step of the samseg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tokens}, \code{n_prompts}, \code{sparse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
encode_box_prompt(V)
}
