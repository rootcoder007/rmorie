% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_pmfsc_derive}
\alias{morie_pmfsc_derive}
\title{Turn observed contacts into a potential, one curve per type pair}
\usage{
morie_pmfsc_derive(observations, n_complexes = 1, r_max = .PMFSC_DEFAULT_CUTOFF, 
    n_bins = 24, reference = "bulk", correction = "none", occupied = NULL, 
    kT = 1, cap = 6) 
}
\arguments{
\item{observations}{A list of list(type_i, type_j, separation) for   every contact seen in the training complexes. Order within a pair   matters only in that the two type labels are kept as given.}
\item{n_complexes}{How many complexes the observations came from. It   scales every density identically and therefore cancels out of a   bulk-referenced potential.}
\item{r_max}{The radial cutoff.}
\item{n_bins}{The number of radial bins.}
\item{reference}{A member of the reference list.}
\item{correction}{A member of the correction list.}
\item{occupied}{Per-bin ligand-occupied volume, or NULL.}
\item{kT}{The energy scale; one leaves the potential in units of kT.}
\item{cap}{The magnitude, in units of kT, at which an unobserved bin   is held. Unobserved is not the same as forbidden.}
}
\description{
Turn observed contacts into a potential, one curve per type pair
}
\value{
A named list keyed by "type_i|type_j" with the counts, the   density, the reference density, the potential and the count of   capped bins.
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
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_pmfsc_derive(D)
}
