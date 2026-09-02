% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_block}
\alias{morie_chemsc_block}
\title{The ChemScore block function of a deviation from ideal}
\usage{
morie_chemsc_block(d, d_ideal, d_max) 
}
\arguments{
\item{d}{The deviation.}
\item{d_ideal}{The tolerance window.}
\item{d_max}{The maximum deviation.}
}
\description{
One inside the tolerance, zero past the maximum, a straight line between. Note it is a function of the DEVIATION, so it is already folded about zero and never sees a sign.
}
\value{
A value in the unit interval.
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
morie_chemsc_block(2.5, 2.0, 4.0)
}
