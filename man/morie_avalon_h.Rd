% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon_h}
\alias{morie_avalon_h}
\title{Hydrogens filled in to the standard valence of the organic subset}
\usage{
morie_avalon_h(el, arom, chg, hexp, bonds)
}
\arguments{
\item{el}{Element symbols.}
\item{arom}{Aromatic flags.}
\item{chg}{Formal charges.}
\item{hexp}{Explicit hydrogen counts, or minus one.}
\item{bonds}{The bond list.}
}
\description{
An aromatic atom is counted as carrying one extra bond, which is what makes each carbon of benzene come out with exactly one hydrogen. An explicit count in brackets is taken as given and nothing is filled in.
}
\value{
One count per atom.
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
