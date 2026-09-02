% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_clash}
\alias{morie_chemsc_clash}
\title{The clash penalty for one too-close contact}
\usage{
morie_chemsc_clash(r, kind = "general", slope = 1, par = NULL) 
}
\arguments{
\item{r}{The contact distance.}
\item{kind}{One of hbond, metal, sulphur, general.}
\item{slope}{The penalty per angstrom of overlap.}
\item{par}{Overrides for the clash radii, or NULL.}
}
\description{
The radii are the GOLD defaults: 1.6 for a hydrogen-bonding contact, 1.3 for a metal coordination contact, 3.35 to a protein sulphur and 3.10 for anything else. A hydrogen bond is allowed much closer than a general contact because it IS a close contact. The SHAPE of the penalty is this module's, not a source's: the guide states the radii and renders the function as an image. A linear ramp per angstrom of overlap is used, which is monotone, zero at the radius and continuous there -- the three properties a docking run actually needs from it.
}
\value{
The penalty, zero at or beyond the radius.
}
\references{
  Eldridge, M.D., Murray, C.W., Auton, T.R., Paolini, G.V. and Mee,
    R.P. (1997) "Empirical scoring functions: I. The development of a
    fast empirical scoring function to estimate the binding affinity
    of ligands in receptor complexes." Journal of Computer-Aided
    Molecular Design 11(5), 425-445. doi:10.1023/A:1007996124545.
  Baxter, C.A., Murray, C.W., Clark, D.E., Westhead, D.R. and
    Eldridge, M.D. (1998) "Flexible docking using Tabu search and an
    empirical estimate of binding affinity." Proteins 33(3), 367-382.
  Cambridge Crystallographic Data Centre, "GOLD User Guide," sections
    8.4.1 to 8.4.6. Every default parameter value used here.
  Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
    R.D. (2003) "Improved protein-ligand docking using GOLD."
    Proteins 52(4), 609-623.
}
\keyword{internal}
\examples{
morie_chemsc_clash(r = 5L)
}
