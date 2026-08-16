% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_repetition_penalty}
\alias{morie_burkov_repetition_penalty}
\title{Repetition penalty on decoder logits (Burkov Ch 5)}
\usage{
morie_burkov_repetition_penalty(logits, prev_tokens, penalty = 1.2)
}
\arguments{
\item{logits}{Logit vector.}
\item{prev_tokens}{0-based indices of   already-generated tokens (matching the Python mirror).}
\item{penalty}{Positive penalty, usually above 1.}
}
\description{
Divide positive logits of seen tokens by the penalty, multiply negative ones -- both move the token DOWN in odds against every unpenalised token. Note the invariant is the ODDS: with several tokens penalised at once, softmax renormalisation can raise one penalised token\'s absolute probability when another falls further.
}
\value{
A list with \code{penalised}, \code{estimate}, \code{penalty}, \code{tokens_hit}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_burkov_repetition_penalty(V, M)
}
