% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cypin_descriptors}
\alias{morie_cypin_descriptors}
\title{The descriptor block, computed exactly from the graph}
\usage{
morie_cypin_descriptors(smiles)
}
\arguments{
\item{smiles}{The compound.}
}
\description{
Rotatable bonds are single bonds outside a ring whose two ends each have more than one heavy neighbour -- the standard definition, which excludes a terminal methyl because spinning it changes nothing, and excludes ring bonds because they cannot turn. The saturated fraction counts carbons with four single bonds and no aromaticity, over all carbons; a molecule with no carbon has no such fraction and reports zero rather than dividing by nothing.
}
\value{
A numeric vector in the order given by the module's names.
}
\references{
  Veith, H. et al. (2009) "Comprehensive characterization of
    cytochrome P450 isozyme selectivity across chemical libraries."
    Nature Biotechnology 27(11), 1050-1055. doi:10.1038/nbt.1581.
  Meija, J. et al. (2016) "Atomic weights of the elements 2013."
    Pure and Applied Chemistry 88(3), 265-291.
  McCullagh, P. and Nelder, J.A. (1989) "Generalized Linear Models",
    2nd edition, Chapman and Hall.
}
\keyword{internal}
\examples{
r <- morie_cypin_descriptors("c1ccccc1O")
str(r, max.level = 1)
}
