% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_recall_at_k}
\alias{morie_alammar_recall_at_k}
\title{morie_alammar_recall_at_k}
\usage{
morie_alammar_recall_at_k(retrieved, relevant, k) 
}
\arguments{
\item{retrieved}{Ranked ids.}
\item{relevant}{Relevant ids.}
\item{k}{k.}
}
\description{
morie_alammar_recall_at_k
}
\value{
A list with \code{estimate}, \code{hits}, \code{n_relevant}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_recall_at_k(retrieved = c(1, 2, 3, 4, 5, 6, 7, 8),
  relevant = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
