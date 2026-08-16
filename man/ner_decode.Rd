% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ner_decode}
\alias{ner_decode}
\title{ner_decode}
\usage{
ner_decode(emissions, types, decoder = "viterbi", transition_scores = NULL, gold = NULL)
}
\arguments{
\item{emissions}{See Usage.}
\item{types}{See Usage.}
\item{decoder}{Defaults to \code{"viterbi"}.}
\item{transition_scores}{Defaults to \code{NULL}.}
\item{gold}{Defaults to \code{NULL}.}
}
\description{
Part of the benRea_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
