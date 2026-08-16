% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{build_sequence}
\alias{build_sequence}
\title{build_sequence}
\usage{
build_sequence(visual_tokens, text_embeddings)
}
\arguments{
\item{visual_tokens}{Iterated over elementwise, with \code{lapply}.}
\item{text_embeddings}{Iterated over elementwise, with \code{lapply}.}
}
\description{
A step of the llavx_native implementation. Called by \code{morie_llavx}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{sequence}, \code{n_visual}, \code{n_text}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
build_sequence(V, V)
}
