% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.avalon_ty}
\alias{.avalon_ty}
\title{.avalon_ty}
\usage{
.avalon_ty(el, arom, i)
}
\arguments{
\item{el}{A vector; indexed elementwise.}
\item{arom}{A vector; indexed elementwise.}
\item{i}{See Usage.}
}
\description{
A step of the avalon_native implementation. Called by \code{morie_avalon_features}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
