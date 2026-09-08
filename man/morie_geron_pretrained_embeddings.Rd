% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pretrained_embeddings}
\alias{morie_geron_pretrained_embeddings}
\title{Pretrained word embeddings as initialisation, LCG-filled OOV rows (Geron Ch 14, hmpemb)}
\usage{
morie_geron_pretrained_embeddings(vocab, pretrained, freeze = TRUE, seed = 0, oov_scale = 0.05) 
}
\arguments{
\item{vocab}{Tokens in index order.}
\item{pretrained}{Named list token -> vector.}
\item{freeze}{Whether the table is held fixed.}
\item{seed,oov_scale}{OOV row controls.}
}
\description{
Pretrained word embeddings as initialisation, LCG-filled OOV rows (Geron Ch 14, hmpemb)
}
\value{
A list with \code{embeddings}, \code{coverage}, \code{oov}, \code{oov_indices}, \code{dim}, \code{freeze}, \code{trainable}, \code{n_parameters}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_geron_pretrained_embeddings(V, D)
}
