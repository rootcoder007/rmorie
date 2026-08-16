% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon_features}
\alias{morie_avalon_features}
\title{The feature keys of a molecule, as sorted strings}
\usage{
morie_avalon_features(smiles, maxpath = 5L, classes = NULL)
}
\arguments{
\item{smiles}{The molecule.}
\item{maxpath}{The longest path feature, in bonds.}
\item{classes}{Which feature classes to use, or NULL for all five.}
}
\description{
Exposed because a fingerprint that cannot be asked what it saw is a fingerprint that cannot be debugged: the bits are these keys hashed, and a surprising bit can always be traced back to a feature here.
}
\value{
A sorted character vector.
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
