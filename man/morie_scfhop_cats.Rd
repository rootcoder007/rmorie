% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_scfhop_cats}
\alias{morie_scfhop_cats}
\title{The CATS correlation vector: type pairs by topological distance}
\usage{
morie_scfhop_cats(smiles, maxdist = 9L, scaling = "type")
}
\arguments{
\item{smiles}{The molecule.}
\item{maxdist}{The distance limit, in bonds.}
\item{scaling}{One of type, count or none.}
}
\description{
Laid out pair-major, distance-minor, so entry \code{p * (maxdist + 1) + d} is the count of pairs of the p-th type combination exactly d bonds apart. Distance zero is an atom with itself, which is how an atom carrying two types registers at all. Scaling routes, all three defensible and all giving different answers, so the choice is the caller's. \code{type} divides each entry by the number of atoms carrying the two types, which is Schneider's scaling and stops a large molecule dominating simply by being large. \code{count} divides by the total pairs counted. \code{none} leaves raw counts, which is what you want if you intend to compare absolute frequencies.
}
\value{
A numeric vector.
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
r <- morie_scfhop_cats("c1ccccc1CCN")
str(r, max.level = 1)
}
