% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_csum}
\alias{.karpv_csum}
\title{Compensated accumulation, so both arms agree bit for bit: R\\'s sum()}
\usage{
.karpv_csum(v)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
}
\description{
accumulates in long double, CPython\\'s compensates, and neither is the plain loop the other one is.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.karpv_csum(V)
}
