% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_goldsc_vdw}
\alias{morie_goldsc_vdw}
\title{Sum a Lennard-Jones potential over a list of contacts}
\usage{
morie_goldsc_vdw(pairs, radii, depths, potential = "4-8", cutoff = NULL)
}
\arguments{
\item{pairs}{A list of list(distance, type_i, type_j).}
\item{radii}{A list of list(type, radius).}
\item{depths}{A list of list(type, well depth).}
\item{potential}{A member of the potential list.}
\item{cutoff}{Drop contacts beyond this separation, or NULL.}
}
\description{
The combining rule is arithmetic on the radii and geometric on the well depths, which is the ordinary Lorentz-Berthelot convention and is stated here rather than hidden. A cutoff drops contacts beyond it entirely rather than tapering them; that is a discontinuity, and it is the caller's decision, so it is off by default.
}
\value{
A list with the total, the per-contact energies and the count.
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
\examples{
RAD <- list(list("C", 1.9), list("N", 1.7), list("O", 1.6))
DEP <- list(list("C", 0.15), list("N", 0.16), list("O", 0.2))
morie_goldsc_vdw(list(list(3.4, "C", "C"), list(5, "C", "N")), 
    RAD, DEP)
}
