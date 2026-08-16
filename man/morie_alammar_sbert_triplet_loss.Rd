% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_sbert_triplet_loss}
\alias{morie_alammar_sbert_triplet_loss}
\title{morie_alammar_sbert_triplet_loss}
\usage{
morie_alammar_sbert_triplet_loss(anchor, positive, negative, margin = 1)
}
\arguments{
\item{anchor,positive,negative}{Row-matched matrices.}
\item{margin}{m.}
}
\description{
morie_alammar_sbert_triplet_loss
}
\value{
A list with \code{estimate}, \code{losses}, \code{active}, \code{d_positive}, \code{d_negative}, \code{n}, \code{method}.
}
\keyword{internal}
