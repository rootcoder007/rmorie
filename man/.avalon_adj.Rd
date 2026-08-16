% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.avalon_adj}
\alias{.avalon_adj}
\title{.avalon_adj}
\usage{
.avalon_adj(n, bonds)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{bonds}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the avalon_native implementation. Called by \code{morie_avalon_features}, \code{morie_avalon_rings}, \code{morie_cypin_descriptors} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{adj}, as built in the body.
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
.avalon_adj(n = 5L, bonds = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
