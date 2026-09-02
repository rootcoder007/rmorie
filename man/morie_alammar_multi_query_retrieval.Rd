% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_multi_query_retrieval}
\alias{morie_alammar_multi_query_retrieval}
\title{morie_alammar_multi_query_retrieval}
\usage{
morie_alammar_multi_query_retrieval(query, K, retriever, rephraser) 
}
\arguments{
\item{query}{Original query.}
\item{K}{Rephrasings.}
\item{retriever}{function(query) -> ranked ids.}
\item{rephraser}{function(query, i) -> alternative query.}
}
\description{
morie_alammar_multi_query_retrieval
}
\value{
A list with \code{documents}, \code{queries}, \code{added_per_query}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
q <- matrix(stats::rnorm(3), 1, 3)
i <- morie_alammar_infonce_loss(c(1, 0), c(1, 0), rbind(c(0, 
    1), c(-1, 0)), 0.1)
corpus <- list(q = c(1, 2), q0 = c(2, 3), q1 = 4)
morie_alammar_multi_query_retrieval("q", 2, function(q) corpus[[q]], 
    function(q, i) paste0(q, i))
}
