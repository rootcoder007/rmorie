% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_phacf3_space}
\alias{morie_phacf3_space}
\title{Every canonical triangle the alphabet allows, in a fixed order}
\usage{
morie_phacf3_space(features = .PHACF3_FEATURES, n_bins = NULL, edges = .PHACF3_DEFAULT_EDGES) 
}
\arguments{
\item{features}{The feature alphabet.}
\item{n_bins}{The number of distance bins, or NULL to take it from   the edges.}
\item{edges}{The bin edges.}
}
\description{
Built by enumerating all orderings and canonicalising, so it is exactly the set of keys the canonical function can produce and nothing else -- a space derived by a formula could be off by the number of degenerate triangles and nobody would notice until two molecules disagreed.
}
\value{
A list with the key matrix, their string forms and a lookup   environment from string to zero-based bit index.
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
morie_phacf3_space(FS, length(EDGES) - 1L, EDGES)
}
