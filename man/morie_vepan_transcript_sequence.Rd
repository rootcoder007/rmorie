% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vepan_transcript_sequence}
\alias{morie_vepan_transcript_sequence}
\title{The spliced transcript, 5\' to 3\', and the genomic position of each}
\usage{
morie_vepan_transcript_sequence(tr, genome)
}
\arguments{
\item{tr}{Passed to \code{.vepan_transcript}.}
\item{genome}{Coerced to character by the body, with \code{as.character}.}
}
\description{
of its bases (returns list(seq, gpos)).
}
\value{
A list with \code{seq}, \code{gpos}.
}
\keyword{internal}
\examples{
genome <- "AATGGCCTAGGCTTAA"
tr <- list(exons = rbind(c(3L, 8L), c(11L, 14L)), strand = "+")
morie_vepan_transcript_sequence(tr, genome)
}
