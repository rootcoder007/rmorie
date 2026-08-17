% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon_parse}
\alias{morie_avalon_parse}
\title{A SMILES string as an atom-bond graph}
\usage{
morie_avalon_parse(smiles)
}
\arguments{
\item{smiles}{The molecule, in the subset the parser accepts.}
}
\description{
Returns the element of each atom, whether it was written aromatic, its formal charge, its explicit hydrogen count if one was given, and the bonds as index triples with the order. Ring-closure bonds are reported separately because the ring perception starts from them.
}
\value{
A list with el, arom, chg, hexp, bonds and closures.
}
\references{
  Gedeck, P., Rohde, B. and Bartels, C. (2006) "QSAR -- how good is it
    in practice? Comparison of descriptor sets on an unbiased cross
    section of database candidates." Journal of Chemical Information
    and Modeling 46(5), 1924-1936. doi:10.1021/ci050413p.
  Carhart, R.E., Smith, D.H. and Venkataraghavan, R. (1985) "Atom
    pairs as molecular features in structure-activity studies."
    Journal of Chemical Information and Computer Sciences 25(2),
    64-73.
  Weininger, D. (1988) "SMILES, a chemical language and information
    system. 1." Journal of Chemical Information and Computer Sciences
    28(1), 31-36.
  Fowler, G., Noll, L.C. and Vo, K.-P. FNV-1a, the 32-bit
    multiply-and-xor hash used to fold a feature into a bit.
}
\keyword{internal}
\examples{
p <- morie_avalon_parse("c1ccccc1O")
str(p, max.level = 1)
}
