% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon_fnv}
\alias{morie_avalon_fnv}
\title{FNV-1a over the bytes of a feature key}
\usage{
morie_avalon_fnv(s, seed = 2166136261) 
}
\arguments{
\item{s}{A character scalar.}
\item{seed}{The FNV offset basis.}
}
\description{
Written as explicit modular arithmetic rather than with a language's integer type, so both arms of the package compute the same number instead of a number that rounds the same way.
}
\value{
A number below two to the thirty-second.
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
S <- c("a", "b", "c")
morie_avalon_fnv(S)
}
