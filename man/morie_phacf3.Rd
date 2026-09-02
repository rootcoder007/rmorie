% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_phacf3}
\alias{morie_phacf3}
\title{Fingerprint a set of typed 3D feature points}
\usage{
morie_phacf3(mol_3d, feature_set = .PHACF3_FEATURES, edges = .PHACF3_DEFAULT_EDGES, 
    mode = "binary", space = NULL) 
}
\arguments{
\item{mol_3d}{A list of rows: x, y, z, type. The type must be a   member of the alphabet.}
\item{feature_set}{The alphabet. Its ORDER fixes the bit space, so   two fingerprints are only comparable if they were built on the same   alphabet in the same order.}
\item{edges}{Bin edges in angstroms.}
\item{mode}{"binary" sets a bit once however many triangles hit it;   "count" counts them, which keeps the information that a molecule   presented the same pharmacophore in six different places.}
\item{space}{A prebuilt space, since building it is the expensive   part and it does not depend on the molecule.}
}
\description{
Fingerprint a set of typed 3D feature points
}
\value{
A list with the fingerprint, the bits set, the triangles that   produced them, and the counts of triangles rejected for falling   outside the distance range or for failing the triangle inequality.
}
\references{
  Gund, P. (1977) "Three-dimensional pharmacophoric pattern
    searching." Progress in Molecular and Subcellular Biology 5,
    117-143.
  Mason, J.S., Good, A.C. and Martin, E.J. (2001) "3-D pharmacophores
    in drug discovery." Current Pharmaceutical Design 7(7), 567-597.
  Mason, J.S., Morize, I., Menard, P.R., Cheney, D.L., Hulme, C. and
    Labaudiniere, R.F. (1999) "New 4-point pharmacophore method for
    molecular similarity and diversity applications." Journal of
    Medicinal Chemistry 42(17), 3251-3264.
  Tanimoto, T.T. (1958) "An elementary mathematical theory of
    classification and prediction." IBM internal report.
}
\keyword{internal}
\examples{
FS <- c("donor", "acceptor", "hydrophobic", "aromatic")
EDGES <- c(2, 4.5, 7, 10, 14)
SPACE <- morie_phacf3_space(FS, length(EDGES) - 1L, EDGES)
MOL <- list(list(0, 0, 0, "donor"), list(3, 0, 0, "acceptor"), 
    list(0, 4, 0, "hydrophobic"), list(2, 2, 5, "aromatic"), 
    list(-3, 1, 2, "donor"), list(6, -2, 1, "acceptor"))
morie_phacf3(MOL, FS, EDGES, "binary", SPACE)
}
