% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_lda_topic_distribution}
\alias{morie_alammar_lda_topic_distribution}
\title{LDA by collapsed Gibbs on the shared LCG (Griffiths and Steyvers}
\usage{
morie_alammar_lda_topic_distribution(documents, n_topics, alpha = 0.1, beta = 0.01, n_iter = 200, 
    seed = 1) 
}
\arguments{
\item{documents}{List of token vectors.}
\item{n_topics}{K.}
\item{alpha,beta,n_iter,seed}{Settings.}
}
\description{
2004, Eq 5)
}
\value{
A list with \code{theta}, \code{phi}, \code{vocabulary}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_lda_topic_distribution(documents = c(1, 2, 3, 4, 5, 6, 7, 8), n_topics = 5L)
}
