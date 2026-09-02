% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_goldsc_lj}
\alias{morie_goldsc_lj}
\title{A general (m, n) Lennard-Jones energy}
\usage{
morie_goldsc_lj(r, r0, eps, m = 6, n = 12) 
}
\arguments{
\item{r}{The separation.}
\item{r0}{The separation at the minimum.}
\item{eps}{The well depth.}
\item{m}{The attractive exponent.}
\item{n}{The repulsive exponent.}
}
\description{
Minimum of -eps at r0, zero at infinity, and singular at zero. The parameterisation is by the MINIMUM rather than by the zero crossing because that is what a docking parameter file stores: a contact radius and a well depth.
}
\value{
The energy.
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
morie_goldsc_lj(r = 5L, r0 = c(1, 2, 3, 4, 5, 6, 7, 8), eps = 0.5)
}
