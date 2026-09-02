% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc}
\alias{morie_chemsc}
\title{Score a pose from coordinates and atom roles}
\usage{
morie_chemsc(receptor, ligand, smoothing = "gaussian", dg0 = 0, clash_slope = 1, 
    intra_coefficient = 1, rotatable = list(), torsions = list(), coefficients = NULL, 
    par = NULL) 
}
\arguments{
\item{receptor}{A list of atom rows: x, y, z, role, then the   coordinates of the attached atom the geometry needs -- the donor's   hydrogen for a donor, the acceptor's attached heavy atom for an   acceptor. Roles are "donor", "acceptor", "metal", "lipophilic",   "sulphur" and "other". Rows whose partner coordinates are absent   contribute no directional term.}
\item{ligand}{The ligand atom rows, likewise.}
\item{smoothing}{A member of the smoothing list.}
\item{dg0}{The intercept.}
\item{clash_slope}{The clash penalty per angstrom of overlap.}
\item{intra_coefficient}{The weight on internal torsional strain.}
\item{rotatable}{Non-lipophilic fraction pairs for each FROZEN   rotatable bond. Nothing in a set of coordinates says which bonds   are frozen, so this is supplied rather than guessed.}
\item{torsions}{A list of c(phi, A, n, phi0) per rotatable bond.}
\item{coefficients}{Overrides for the regression coefficients.}
\item{par}{Overrides for the geometry parameters.}
}
\description{
Score a pose from coordinates and atom roles
}
\value{
As the score function, with the contact lists it built.
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
morie_chemsc(D, D)
}
