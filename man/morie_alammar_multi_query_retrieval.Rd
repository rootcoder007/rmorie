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
