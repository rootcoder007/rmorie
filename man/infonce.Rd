% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{infonce}
\alias{infonce}
\title{infonce}
\usage{
infonce(image_embeddings, text_embeddings, temperature = 0.07)
}
\arguments{
\item{image_embeddings}{See Usage.}
\item{text_embeddings}{See Usage.}
\item{temperature}{Defaults to \code{0.07}.}
}
\description{
Part of the opnclp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{loss}, \code{image_to_text}, \code{text_to_image}, \code{logits}, \code{note}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
infonce(M, M)
}
