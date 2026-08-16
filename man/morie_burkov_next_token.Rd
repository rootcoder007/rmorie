% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_next_token}
\alias{morie_burkov_next_token}
\title{Autoregressive next-token probability, bigram MLE (Burkov Eq 2.2/2.3)}
\usage{
morie_burkov_next_token(t_next, s)
}
\arguments{
\item{t_next}{Candidate token.}
\item{s}{Token sequence.}
}
\description{
Eq 2.2 DEFINES a language model rather than giving an estimator, so the operational content is the simplest one the book builds on: count how often the last token of s is followed by t_next within s. Eq 2.3\'s notational equivalence folds in as `notations_agree`.
}
\value{
A list with \code{estimate}, \code{context}, \code{distribution}, \code{notations_agree}, \code{n}, \code{method}.
}
\keyword{internal}
