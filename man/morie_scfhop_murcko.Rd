% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_scfhop_murcko}
\alias{morie_scfhop_murcko}
\title{The Bemis-Murcko framework: ring systems and the linkers between}
\usage{
morie_scfhop_murcko(smiles)
}
\arguments{
\item{smiles}{The molecule.}
}
\description{
Strip side chains by repeatedly deleting any atom that is not in a ring and has at most one remaining neighbour. What survives is the rings plus every atom on a path between two of them. A molecule with no rings loses everything, and the empty scaffold is returned rather than the whole molecule -- an acyclic lead has no skeleton to hop away from and saying so is the useful answer.
}
\value{
A list with the surviving zero-based atom indices and the   surviving bonds.
}
\references{
  Schneider, G., Neidhart, W., Giller, T. and Schmid, G. (1999)
    "'Scaffold-hopping' by topological pharmacophore search."
    Angewandte Chemie International Edition 38(19), 2894-2896.
  Bemis, G.W. and Murcko, M.A. (1996) "The properties of known drugs.
    1. Molecular frameworks." Journal of Medicinal Chemistry 39(15),
    2887-2893. doi:10.1021/jm9602928.
  Weisfeiler, B. and Leman, A.A. (1968) "The reduction of a graph to
    canonical form and the algebra which appears therein."
    Nauchno-Technicheskaya Informatsia 2(9), 12-16.
  Shervashidze, N. et al. (2011) "Weisfeiler-Lehman graph kernels."
    Journal of Machine Learning Research 12, 2539-2561.
}
\keyword{internal}
\examples{
r <- morie_scfhop_murcko("c1ccccc1CCN")
str(r, max.level = 1)
}
