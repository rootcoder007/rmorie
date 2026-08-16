% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_rot}
\alias{morie_chemsc_rot}
\title{The frozen-rotatable-bond entropy term}
\usage{
morie_chemsc_rot(fractions)
}
\arguments{
\item{fractions}{A list of length-two vectors, the non-lipophilic   fractions on the two sides of each FROZEN rotatable bond.}
}
\description{
Hrot = 1 + (1 - 1/Nrot) Sum (P_nl + P'_nl) / 2, which is one unit for the first frozen bond and progressively less for each one after it -- freezing a bond in an already rigid ligand costs less than freezing the first. With no frozen bonds it is zero, not one: there is no entropy to pay. The GOLD guide quotes the two fractions as percentages; they are taken here as fractions in the unit interval.
}
\value{
The entropy term.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_chemsc_rot(D)
}
