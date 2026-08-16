% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_score}
\alias{morie_chemsc_score}
\title{Assemble a ChemScore from its already-measured geometric terms}
\usage{
morie_chemsc_score(hbonds = list(), metals = numeric(0), lipophilic = numeric(0), rotatable = list(), clashes = list(), torsions = list(), smoothing = "gaussian", dg0 = 0, clash_slope = 1, intra_coefficient = 1, coefficients = NULL, par = NULL)
}
\arguments{
\item{hbonds}{A list of list(r, alpha, betas) per donor-acceptor pair.}
\item{metals}{Acceptor-metal distances.}
\item{lipophilic}{Lipophilic pair distances.}
\item{rotatable}{Non-lipophilic fraction pairs for frozen bonds.}
\item{clashes}{A list of list(r, kind) per close contact.}
\item{torsions}{A list of c(phi, A, n, phi0) per rotatable bond.}
\item{smoothing}{A member of the smoothing list.}
\item{dg0}{The intercept.}
\item{clash_slope}{The clash penalty per angstrom of overlap.}
\item{intra_coefficient}{The weight on internal torsional strain.}
\item{coefficients}{Overrides for the regression coefficients.}
\item{par}{Overrides for the geometry parameters, as a named list with   hbond, metal, lipo and clash entries.}
}
\description{
This is the function the docking front end calls once it has turned coordinates into contacts, and it is separately callable because a reader who wants to check the arithmetic should not have to build a protein first.
}
\value{
A list with the free energy estimate, the fitness and every   term separately so the total can be checked against its parts.
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
morie_chemsc_score()
}
