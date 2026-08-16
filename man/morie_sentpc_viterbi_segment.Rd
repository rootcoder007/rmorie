% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sentpc_viterbi_segment}
\alias{morie_sentpc_viterbi_segment}
\title{Unigram LM segmentation via Viterbi}
\usage{
morie_sentpc_viterbi_segment(text, piece_logp, add_prefix = TRUE)
}
\arguments{
\item{text}{Character.}
\item{piece_logp}{Named numeric vector of log probabilities.}
\item{add_prefix}{Logical; escape-and-prefix.}
}
\description{
Maximises \code{sum(log p(piece))} over every split of the escaped input. Unlike BPE this scores every segmentation, so the best is a maximisation rather than the by-product of a construction.
}
\value{
List with \code{pieces}, \code{logp}, \code{n_pieces},   \code{algorithm}.
}
\keyword{internal}
