% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_scfhop_signature}
\alias{morie_scfhop_signature}
\title{A Weisfeiler-Leman colour of the framework, as a fingerprint}
\usage{
morie_scfhop_signature(smiles, rounds = 3L)
}
\arguments{
\item{smiles}{The molecule.}
\item{rounds}{The refinement depth.}
}
\description{
Each surviving atom starts coloured by its element, aromaticity and charge; a round replaces the colour by a hash of it together with the sorted multiset of its neighbours' colours and the bond orders reaching them. The signature is the sorted multiset of final colours. An INVARIANT, not a canonical form: isomorphic scaffolds always agree; different ones agree only in the rare cases the refinement cannot separate.
}
\value{
A sorted numeric vector, empty for an acyclic molecule.
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
