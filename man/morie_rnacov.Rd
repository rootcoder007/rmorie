% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rnacov}
\alias{morie_rnacov}
\title{Score the covariation supporting a structure in an alignment}
\usage{
morie_rnacov(alignment, structure = NULL, correction = "none", mode = "given", min_loop = 3L, min_sequences = 4L)
}
\arguments{
\item{alignment}{A character vector of aligned sequences.}
\item{structure}{A dot-bracket consensus structure, or NULL.}
\item{correction}{"none" or "miller_madow".}
\item{mode}{A member of the structure list.}
\item{min_loop}{The minimum hairpin loop for folding.}
\item{min_sequences}{Pairs with fewer ungapped sequences than this   are reported as unsupported rather than scored as if they were.}
}
\description{
Score the covariation supporting a structure in an alignment
}
\value{
A list with per-pair mutual information and support, the   total, the pairs used, and how many were too sparsely covered to   judge.
}
\references{
  Eddy, S.R. and Durbin, R. (1994) "RNA sequence analysis using
    covariance models." Nucleic Acids Research 22(11), 2079-2088.
    doi:10.1093/nar/22.11.2079.
  Nawrocki, E.P. and Eddy, S.R. (2013) "Infernal 1.1: 100-fold faster
    RNA homology searches." Bioinformatics 29(22), 2933-2935.
  Rivas, E., Clements, J. and Eddy, S.R. (2017) "A statistical test
    for conserved RNA structure shows lack of evidence for structure
    in lncRNAs." Nature Methods 14(1), 45-48.
  Nussinov, R. and Jacobson, A.B. (1980) "Fast algorithm for
    predicting the secondary structure of single-stranded RNA." PNAS
    77(11), 6309-6313.
  Miller, G.A. (1955) "Note on the bias of information estimates." In
    Information Theory in Psychology, 95-100.
}
\keyword{internal}
\examples{
STRUCT <- "(((((...))))).."
PSET <- list(c("G", "C"), c("C", "G"), c("A", "U"), c("U", "A"))
STEM <- list(c(0L, 12L), c(1L, 11L), c(2L, 10L), c(3L, 9L), c(4L, 
    8L))
ALN <- vapply(0:7, function(sq) {
    row <- rep(".", 15L)
    for (k in seq_along(STEM)) {
        ab <- PSET[[(sq + k - 1L)\%\%4L + 1L]]
        row[STEM[[k]][1] + 1L] <- ab[1]
        row[STEM[[k]][2] + 1L] <- ab[2]
    }
    row[6] <- "A"
    row[7] <- substr("ACGU", sq\%\%4L + 1L, sq\%\%4L + 1L)
    row[8] <- "G"
    paste(row, collapse = "")
}, character(1))
morie_rnacov(ALN, STRUCT)
}
