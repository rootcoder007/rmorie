% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.avalon_lte}
\alias{.avalon_lte}
\title{Byte order, not the locale\\'s collation. R would otherwise compare}
\usage{
.avalon_lte(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
strings by whatever collation happens to be in force, and the canonical spelling of a path would depend on the machine.
}
\value{
A logical value.
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
