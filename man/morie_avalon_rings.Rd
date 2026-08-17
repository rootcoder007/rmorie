% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_avalon_rings}
\alias{morie_avalon_rings}
\title{One ring per ring-closure bond: that bond plus the shortest path}
\usage{
morie_avalon_rings(n, bonds, closures)
}
\arguments{
\item{n}{The atom count.}
\item{bonds}{The bond list.}
\item{closures}{Which bonds were ring closures.}
}
\description{
For a single ring this is the ring. For a fused system it is a valid ring set of the right size -- the cyclomatic number -- but not necessarily the canonical smallest set of smallest rings, and it is reported as what it is rather than labelled SSSR.
}
\value{
A list with the rings and a per-atom ring-membership flag.
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
p <- morie_avalon_parse("c1ccccc1")
r <- morie_avalon_rings(length(p$el), p$bonds, p$closures)
str(r, max.level = 1)
}
