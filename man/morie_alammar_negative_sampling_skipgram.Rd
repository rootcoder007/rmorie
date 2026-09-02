% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_negative_sampling_skipgram}
\alias{morie_alammar_negative_sampling_skipgram}
\title{Skip-gram negative sampling and the Bradley-Terry reward loss}
\usage{
morie_alammar_negative_sampling_skipgram(center_vec, context_vec, negative_vecs) 
}
\arguments{
\item{center_vec,context_vec,negative_vecs}{Word vectors.}
}
\description{
Skip-gram negative sampling and the Bradley-Terry reward loss
}
\value{
A list with \code{estimate}, \code{positive_logsig}, \code{negative_logsigs}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_negative_sampling_skipgram(center_vec = 5L, context_vec = 5L, negative_vecs = c(1, 2, 3, 4, 5, 6, 7, 8))
}
