% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_scfhop_types}
\alias{morie_scfhop_types}
\title{Assign each atom its pharmacophore categories}
\usage{
morie_scfhop_types(smiles)
}
\arguments{
\item{smiles}{The molecule.}
}
\description{
An atom may have several, or none. The rules, written out so they can be argued with. D donor: nitrogen or oxygen carrying at least one hydrogen. A acceptor: nitrogen or oxygen -- an atom can be both, which is correct, since a hydroxyl is a donor and an acceptor. P positive: an aliphatic nitrogen with no neighbouring carbonyl carbon, an amine rather than an amide, which is the distinction that decides whether it is protonated at physiological pH. N negative: the oxygens of a carboxyl group, a carbon bearing both a doubly bonded oxygen and a singly bonded one. L lipophilic: carbon with no nitrogen or oxygen neighbour, and sulfur and the halogens.
}
\value{
A list of character vectors, one per atom.
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
