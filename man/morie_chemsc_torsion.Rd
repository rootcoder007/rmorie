% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_torsion}
\alias{morie_chemsc_torsion}
\title{One rotatable bond's internal torsional strain}
\usage{
morie_chemsc_torsion(phi, A, n, phi0)
}
\arguments{
\item{phi}{The torsion angle, in degrees.}
\item{A}{The barrier height.}
\item{n}{The periodicity.}
\item{phi0}{The phase, in radians.}
}
\description{
The cosine form the ChemScore parameter file carries, with A, n and the phase read from lines like SP3_SP3_BOND. The angle is in degrees on the way in, because that is how a torsion is measured.
}
\value{
The strain contribution.
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
