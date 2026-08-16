% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_mean_reciprocal_rank}
\alias{morie_alammar_mean_reciprocal_rank}
\title{Retrieval metrics: MRR, recall@k, NDCG@k, MTEB aggregation}
\usage{
morie_alammar_mean_reciprocal_rank(rankings, relevant_indices)
}
\arguments{
\item{rankings}{List of ranked id vectors.}
\item{relevant_indices}{List of relevant-id vectors.}
}
\description{
Retrieval metrics: MRR, recall@k, NDCG@k, MTEB aggregation
}
\value{
A list with \code{estimate}, \code{reciprocal_ranks}, \code{queries_missed}, \code{n}, \code{method}.
}
\keyword{internal}
