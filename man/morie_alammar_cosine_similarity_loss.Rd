% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_cosine_similarity_loss}
\alias{morie_alammar_cosine_similarity_loss}
\title{Contrastive and ranking losses of the SBERT/CLIP family}
\usage{
morie_alammar_cosine_similarity_loss(a, b, y_true) 
}
\arguments{
\item{a,b}{Row-matched embedding matrices.}
\item{y_true}{Cosine   targets in \\[-1, 1\\].}
}
\description{
Contrastive and ranking losses of the SBERT/CLIP family
}
\value{
A list with \code{estimate}, \code{losses}, \code{similarities}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
A <- matrix(rnorm(12), 3, 4)
B <- A + matrix(rnorm(12, 0, 0.2), 3, 4)
r <- morie_alammar_cosine_similarity_loss(A, B, c(1, 1, -1))
str(r, max.level = 1)
}
