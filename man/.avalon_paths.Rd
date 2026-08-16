% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.avalon_paths}
\alias{.avalon_paths}
\title{A path and its reverse are the same feature, so only the smaller of}
\usage{
.avalon_paths(adj, n, maxpath, ty)
}
\arguments{
\item{adj}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{rep(...)}.}
\item{maxpath}{See Usage.}
\item{ty}{A vector; indexed elementwise.}
}
\description{
the two spellings is kept -- otherwise a symmetric molecule would light twice as many bits as an asymmetric one for no chemical reason. The key is built here rather than by the caller so the two arms of this package cannot canonicalise it differently.
}
\value{
The value of \code{unique}.
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
