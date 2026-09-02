% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_pmfsc}
\alias{morie_pmfsc}
\title{Derive a potential if needed, then score the pose}
\usage{
morie_pmfsc(receptor, ligand, potential = NULL, observations = NULL, n_complexes = 1, 
    r_max = .PMFSC_DEFAULT_CUTOFF, n_bins = 24, reference = "bulk", correction = "none", 
    occupied = NULL, kT = 1, cap = 6, missing = 0) 
}
\arguments{
\item{receptor}{Atom rows: x, y, z, type.}
\item{ligand}{The ligand atom rows, likewise.}
\item{potential}{A derived potential, or NULL to derive one.}
\item{observations}{Training contacts, or NULL.}
\item{n_complexes}{How many complexes the observations came from.}
\item{r_max}{The radial cutoff.}
\item{n_bins}{The number of radial bins.}
\item{reference}{A member of the reference list.}
\item{correction}{A member of the correction list.}
\item{occupied}{Per-bin ligand-occupied volume, or NULL.}
\item{kT}{The energy scale.}
\item{cap}{The unobserved-bin cap, in units of kT.}
\item{missing}{The contribution of an unparameterised pair.}
}
\description{
Derive a potential if needed, then score the pose
}
\value{
A list with the score, the potential used, and the counts   that say how much of the pose the potential actually covered.
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
RMAX <- 12
NB <- 24L
PT <- c("C", "N", "O")
LT <- c("c", "n", "o")
OBS <- lapply(0:399, function(i) {
    a <- PT[(i\%\%3) + 1L]
    b <- LT[((i\%/\%3)\%\%3) + 1L]
    r <- if (a == "C" && b == "c") 
        3 + ((i * 7)\%\%20)/10
    else if (a == "O" && b == "n") 
        7 + ((i * 11)\%\%40)/10
    else 1 + ((i * 13)\%\%110)/10
    list(a, b, r)
})
REC <- list(list(0, 0, 0, "C"), list(4, 0, 0, "N"), list(0, 4.5, 
    0, "O"), list(20, 0, 0, "C"))
LIG <- list(list(3.4, 0, 0, "c"), list(1, 3, 0, "n"), list(5, 
    5, 1, "o"), list(2, 2, 2, "x"))
morie_pmfsc(REC, LIG, observations = OBS, r_max = RMAX, n_bins = NB)
}
