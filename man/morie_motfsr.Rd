% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_motfsr}
\alias{morie_motfsr}
\title{Discover motifs by fitting the MM mixture model}
\usage{
morie_motfsr(sequences, w, alphabet = NULL, n_motifs = 1, beta = 0.01, lambda0 = NULL, max_iter = 1000, tol = 1e-06, normalize_overlaps = TRUE, starts = "subsequences", start_weight = 0.5, max_starts = 200, start_scoring = "one_step", erase_by = "letter", loss = NULL)
}
\arguments{
\item{sequences}{Dataset.}
\item{w}{Motif width.}
\item{alphabet}{Optional alphabet.}
\item{n_motifs}{Number of passes.}
\item{beta}{Pseudo-count total.}
\item{lambda0}{Optional fixed lambda0.}
\item{max_iter,tol}{EM stopping rule.}
\item{normalize_overlaps}{Apply the window constraint.}
\item{starts}{"subsequences" or "uniform".}
\item{start_weight}{Probability on the observed letter when seeding   from a subsequence.}
\item{max_starts}{Cap on subsequence starts tried.}
\item{start_scoring}{"one_step" or "none".}
\item{erase_by}{"letter" or "start".}
\item{loss}{Optional loss matrix.}
}
\description{
Discover motifs by fitting the MM mixture model
}
\value{
A list with estimate / motifs, alphabet, w, n_subsequences,   erasing, method.
}
\keyword{internal}
\examples{
set.seed(1)
seqs <- c("ACGTACGTAC", "TGCAACGTTG", "ACGTTGCAAC", "GGACGTACGT",
          "ACGTAACCGG", "TTACGTACGT")
r <- morie_motfsr(seqs, w = 4, n_motifs = 1)
is.list(r)
}
