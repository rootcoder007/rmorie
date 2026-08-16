% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_xor32}
\alias{.karpv_xor32}
\title{32-bit operations on doubles. R\\'s bitwXor is defined over the signed}
\usage{
.karpv_xor32(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
32-bit range and overflows above 2^31, so the words are split into two 16-bit halves; every value stays well under 2^53 and the arithmetic is exact.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.karpv_xor32(V, V)
}
