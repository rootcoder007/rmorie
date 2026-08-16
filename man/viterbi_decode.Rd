% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{viterbi_decode}
\alias{viterbi_decode}
\title{viterbi_decode}
\usage{
viterbi_decode(emissions, labels, transitions = NULL, transition_scores = NULL)
}
\arguments{
\item{emissions}{A matrix; passed to \code{as.matrix}.}
\item{labels}{A vector; its length is taken and its elements indexed.}
\item{transitions}{Defaults to \code{NULL}.}
\item{transition_scores}{Defaults to \code{NULL}.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}. See the file header for the source the module follows.
}
\value{
A list with \code{path}, \code{score}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
viterbi_decode(M, S)
}
