% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_scfhop}
\alias{morie_scfhop}
\title{Rank candidates by pharmacophore, and say which ones are hops}
\usage{
morie_scfhop(lead_smiles, scaffold_db, maxdist = 9L, scaling = "type", metric = "tanimoto", 
    rounds = 3L, threshold = 0) 
}
\arguments{
\item{lead_smiles}{The lead compound.}
\item{scaffold_db}{Candidate molecules, as SMILES.}
\item{maxdist}{The descriptor's distance limit.}
\item{scaling}{The descriptor's scaling.}
\item{metric}{The comparison.}
\item{rounds}{Weisfeiler-Leman refinement depth.}
\item{threshold}{Candidates below this similarity are ranked but not   called hops. Zero calls every different-scaffold candidate a hop,   which is the honest default: the cut-off is a decision about the   screening campaign, not a property of the method.}
}
\description{
Rank candidates by pharmacophore, and say which ones are hops
}
\value{
A list with the ranked candidates, each with its similarity,   whether its scaffold differs from the lead's, and whether it is a   hop.
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
LEAD <- "OCCc1ccccc1"
DB <- c("OCCc1ccccc1", "OCCc1ccncc1", "OCCC1CCCCC1", "OCCCCCC", 
    "Cc1ccccc1", "CC(=O)O")
morie_scfhop(LEAD, DB)
}
