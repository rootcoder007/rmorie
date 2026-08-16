% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon}
\alias{morie_avalon}
\title{Hash a molecule's structural features into a bit vector}
\usage{
morie_avalon(smiles, n_bits = 512L, maxpath = 5L, classes = NULL)
}
\arguments{
\item{smiles}{The molecule, in the SMILES subset the parser accepts.}
\item{n_bits}{The width of the vector. Folding is by remainder, so a   narrower vector is the wider one folded and collisions rise with it   -- which is why the collision count is reported and not hidden.}
\item{maxpath}{The longest path feature, in bonds.}
\item{classes}{Which feature classes to use; NULL is all five.   Restricted to bond and path the fingerprint is subgraph monotone.}
}
\description{
Hash a molecule's structural features into a bit vector
}
\value{
A list with the bits, the features that set them, and the   collision count.
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
