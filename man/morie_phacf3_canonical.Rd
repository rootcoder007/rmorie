% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_phacf3_canonical}
\alias{morie_phacf3_canonical}
\title{The canonical (types, bins) form of one feature triangle}
\usage{
morie_phacf3_canonical(t1, t2, t3, d12, d13, d23)
}
\arguments{
\item{t1}{First vertex type, zero-based.}
\item{t2}{Second vertex type.}
\item{t3}{Third vertex type.}
\item{d12}{Bin of the edge between vertices one and two.}
\item{d13}{Bin of the edge between vertices one and three.}
\item{d23}{Bin of the edge between vertices two and three.}
}
\description{
Each vertex carries a type and each EDGE a bin. Under a permutation of the vertices the edges permute with them, so all six orderings are generated and the smallest tuple wins. Generating all six is the point: a comparison rule that sorted the types first and hoped the edges followed is wrong whenever two types are equal, which on a six-letter alphabet is most of the time.
}
\value{
A length-six integer vector, the canonical key.
}
\references{
  Gund, P. (1977) "Three-dimensional pharmacophoric pattern
    searching." Progress in Molecular and Subcellular Biology 5,
    117-143.
  Mason, J.S., Good, A.C. and Martin, E.J. (2001) "3-D pharmacophores
    in drug discovery." Current Pharmaceutical Design 7(7), 567-597.
  Mason, J.S., Morize, I., Menard, P.R., Cheney, D.L., Hulme, C. and
    Labaudiniere, R.F. (1999) "New 4-point pharmacophore method for
    molecular similarity and diversity applications." Journal of
    Medicinal Chemistry 42(17), 3251-3264.
  Tanimoto, T.T. (1958) "An elementary mathematical theory of
    classification and prediction." IBM internal report.
}
\keyword{internal}
