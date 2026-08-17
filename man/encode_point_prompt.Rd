% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encode_point_prompt}
\alias{encode_point_prompt}
\title{encode_point_prompt}
\usage{
encode_point_prompt(points, labels, dim = 8, type_embeddings = NULL)
}
\arguments{
\item{points}{Iterated over elementwise, with \code{lapply}.}
\item{labels}{Passed to \code{unlist}.}
\item{dim}{Passed to \code{.samseg_pos_enc}. Defaults to \code{8}.}
\item{type_embeddings}{Defaults to \code{NULL}.}
}
\description{
A step of the samseg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{tokens}, \code{n_prompts}, \code{sparse}, \code{note}.
}
\keyword{internal}
\examples{
e <- encode_point_prompt(points = list(c(10, 20), c(30, 40)),
                         labels = c(1, 0), dim = 8)
is.list(e) || is.matrix(e) || is.numeric(e)
}
