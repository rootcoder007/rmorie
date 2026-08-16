% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_pmfsc_bin}
\alias{morie_pmfsc_bin}
\title{Which bin a separation falls in, or -1 if it is past the cutoff}
\usage{
morie_pmfsc_bin(r, r_max, n_bins)
}
\arguments{
\item{r}{The separation.}
\item{r_max}{The cutoff.}
\item{n_bins}{The number of bins.}
}
\description{
A distance exactly on an edge goes to the UPPER bin, so the bins are half-open on the left and the assignment cannot depend on which side of an edge the arithmetic happens to land.
}
\value{
The zero-based bin index, or -1 past the cutoff.
}
\references{
  Muegge, I. and Martin, Y.C. (1999) "A general and fast scoring
    function for protein-ligand interactions: a simplified potential
    approach." Journal of Medicinal Chemistry 42(5), 791-804.
    doi:10.1021/jm980536j.
  Muegge, I. (2001) "Effect of ligand volume correction on PMF
    scoring." Journal of Computational Chemistry 22(4), 418-425.
  Muegge, I. (2006) "PMF scoring revisited." Journal of Medicinal
    Chemistry 49(20), 5895-5902.
  Sippl, M.J. (1990) "Calculation of conformational ensembles from
    potentials of mean force." Journal of Molecular Biology 213(4),
    859-883.

A note on what is sourced. The functional form and the role of each
factor above are from the papers. The particular realisation of the
volume correction as an excluded-volume ratio, and the capping rule
for unobserved bins, are this module's -- they are described in the
sources in words rather than given as formulas, so they are named here
as choices rather than attributed.
}
\keyword{internal}
