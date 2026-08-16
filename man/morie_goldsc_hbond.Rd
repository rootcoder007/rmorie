% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_goldsc_hbond}
\alias{morie_goldsc_hbond}
\title{Sum the tabulated hydrogen bond energies of the close pairs}
\usage{
morie_goldsc_hbond(bonds, max_distance = 2.5)
}
\arguments{
\item{bonds}{A list of list(distance, energy).}
\item{max_distance}{The distance threshold.}
}
\description{
GOLD counts a bond towards the fitness only when the donor-hydrogen to acceptor fitting-point distance is below the threshold, and anneals that threshold down over a run so that poor bonds are tolerated early and not at the end. The threshold is therefore a parameter of the CALL, not a constant: a fitness computed at the starting threshold is not the same number as one computed at the finishing threshold, and the guide is explicit that only the final one means anything.
}
\value{
A list with the total, the counted energies and the count.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_goldsc_hbond(D)
}
