% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_pmfsc_score}
\alias{morie_pmfsc_score}
\title{Score a pose against a derived potential}
\usage{
morie_pmfsc_score(pairs, potential, r_max = .PMFSC_DEFAULT_CUTOFF, n_bins = 24, missing = 0)
}
\arguments{
\item{pairs}{A list of list(type_i, type_j, separation).}
\item{potential}{A potential from the derive function.}
\item{r_max}{The radial cutoff.}
\item{n_bins}{The number of radial bins.}
\item{missing}{The contribution of an unparameterised pair.}
}
\description{
A pair whose type combination is absent from the potential contributes the missing value and is counted separately -- silently scoring it as zero would let a pose made entirely of unparameterised atoms come out looking average.
}
\value{
A list with the score and the three counts.
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
