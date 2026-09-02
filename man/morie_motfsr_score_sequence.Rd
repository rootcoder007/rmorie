% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_motfsr_score_sequence}
\alias{morie_motfsr_score_sequence}
\title{Score every W-mer of a sequence}
\usage{
morie_motfsr_score_sequence(spec, sequence, alphabet, threshold = NULL) 
}
\arguments{
\item{spec}{Log-odds matrix.}
\item{sequence}{Sequence string.}
\item{alphabet}{Alphabet.}
\item{threshold}{Optional threshold; if given, also returns hits.}
}
\description{
Score every W-mer of a sequence
}
\value{
Vector of scores, or list(scores, hits).
}
\keyword{internal}
\examples{
morie_motfsr_score_sequence(spec = c(1, 2, 3, 4, 5, 6, 7, 8), sequence = c(1, 2, 3, 4, 5, 6, 7, 8), alphabet = c(1, 2, 3, 4, 5, 6, 7, 8))
}
