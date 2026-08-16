% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_goldsc}
\alias{morie_goldsc}
\title{The GoldScore fitness of a pose}
\usage{
morie_goldsc(receptor, ligand, radii = list(), depths = list(), hbonds = list(), internal = list(), torsions = list(), potential = "4-8", internal_potential = "6-12", vdw_weight = .GOLDSC_VDW_WEIGHT, max_distance = 2.5, cutoff = NULL)
}
\arguments{
\item{receptor}{Atom rows: x, y, z, type. Every cross pair enters the   external van der Waals sum.}
\item{ligand}{The ligand atom rows, likewise.}
\item{radii}{A list of list(type, contact radius).}
\item{depths}{A list of list(type, well depth).}
\item{hbonds}{A list of list(distance, energy) per candidate bond.}
\item{internal}{A list of list(distance, type_i, type_j) for the   ligand's own non-bonded pairs.}
\item{torsions}{A list of c(phi, A, n, phi0) per rotatable bond.}
\item{potential}{A member of the potential list, for the external   term.}
\item{internal_potential}{Likewise for the internal term.}
\item{vdw_weight}{The factor on the external van der Waals term.}
\item{max_distance}{The hydrogen-bond distance threshold.}
\item{cutoff}{Drop external contacts beyond this separation, or NULL.}
}
\description{
The GoldScore fitness of a pose
}
\value{
A list with the fitness, each component, and the per-contact   energies.
}
\references{
  Jones, G., Willett, P., Glen, R.C., Leach, A.R. and Taylor, R.
    (1997) "Development and validation of a genetic algorithm for
    flexible docking." Journal of Molecular Biology 267(3), 727-748.
    doi:10.1006/jmbi.1996.0897.
  Jones, G., Willett, P. and Glen, R.C. (1995) "Molecular recognition
    of receptor sites using a genetic algorithm with a description of
    desolvation." Journal of Molecular Biology 245(1), 43-53.
  Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
    R.D. (2003) "Improved protein-ligand docking using GOLD."
    Proteins 52(4), 609-623. doi:10.1002/prot.10465.
  Cambridge Crystallographic Data Centre, "GOLD User Guide," section
    8.3 for the four components and the 1.375 external van der Waals
    factor, and section 5.4 for the 6-12 internal, 4-8 external
    defaults and the two split potentials.
}
\keyword{internal}
