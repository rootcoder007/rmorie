% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.chemsc_par}
\alias{.chemsc_par}
\title{.chemsc_par}
\usage{
.chemsc_par(base, over)
}
\arguments{
\item{base}{A vector; indexed elementwise.}
\item{over}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the chemsc_native implementation. Called by \code{morie_chemsc_clash}, \code{morie_chemsc_hbond}, \code{morie_chemsc_lipophilic} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{base}, as built in the body.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.chemsc_par(V, V)
}
