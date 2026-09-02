% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ner_decode}
\alias{ner_decode}
\title{ner_decode}
\usage{
ner_decode(emissions, types, decoder = "viterbi", transition_scores = NULL, 
    gold = NULL) 
}
\arguments{
\item{emissions}{A matrix; passed to \code{as.matrix}.}
\item{types}{Passed to \code{bio_labels}.}
\item{decoder}{One of \code{"greedy"}, \code{"viterbi"}. Defaults to \code{"viterbi"}.}
\item{transition_scores}{Passed to \code{viterbi_decode}.}
\item{gold}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the benRea_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
ner_decode(emissions = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), types = 5L)
}
