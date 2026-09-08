% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_ndcg_at_k}
\alias{morie_alammar_ndcg_at_k}
\title{morie_alammar_ndcg_at_k}
\usage{
morie_alammar_ndcg_at_k(relevances, k) 
}
\arguments{
\item{relevances}{Graded relevances in ranked order.}

\item{k}{Argument \code{k}; see Usage.}
}
\description{
morie_alammar_ndcg_at_k
}
\value{
A list with \code{estimate}, \code{dcg}, \code{idcg}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_ndcg_at_k(relevances = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
