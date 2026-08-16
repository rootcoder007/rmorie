% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.chemsc_over}
\alias{.chemsc_over}
\title{How far past the ideal separation a contact is, never negative. The}
\usage{
.chemsc_over(r, r1)
}
\arguments{
\item{r}{See Usage.}
\item{r1}{See Usage.}
}
\description{
metal and lipophilic terms are RANGES, not windows: the guide calls R1 the ideal separation and R2 "the maximum distance to be considered a binding interaction", so anything at or inside R1 is fully ideal and only the far side ramps down. Folding this about zero the way the hydrogen-bond deviations are folded would penalise a contact for being too close, which is the clash term\\\'s job and not this one\\\'s -- and it would also make a contact sitting exactly on R1 score a hair under one whenever the coordinate arithmetic put it a single bit on the wrong side.
}
\value{
One of two values, depending on the branch taken.
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
