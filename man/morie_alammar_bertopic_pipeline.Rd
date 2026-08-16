% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_bertopic_pipeline}
\alias{morie_alammar_bertopic_pipeline}
\title{BERTopic pipeline: reduce, cluster, c-TF-IDF (Grootendorst 2022)}
\usage{
morie_alammar_bertopic_pipeline(documents, embeddings, min_cluster_size = 2)
}
\arguments{
\item{documents}{List of token vectors.}
\item{embeddings}{One row   per document.}
\item{min_cluster_size}{Cluster floor.}
}
\description{
BERTopic pipeline: reduce, cluster, c-TF-IDF (Grootendorst 2022)
}
\value{
A list with \code{labels}, \code{reduced}, \code{topic_top_word}, \code{n_topics}, \code{vocabulary}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
