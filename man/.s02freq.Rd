% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s02freq}
\alias{.s02freq}
\title{Minor allele frequencies for markers coded 0/1/2: MVSML p.51 uses}
\usage{
.s02freq(M, freq = NULL)
}
\arguments{
\item{M}{A matrix; passed to \code{as.matrix}.}
\item{freq}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
phat = colMeans(X)/2.
}
\value{
A numeric value.
}
\keyword{internal}
