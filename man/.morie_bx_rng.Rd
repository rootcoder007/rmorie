% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_rng}
\alias{.morie_bx_rng}
\title{Numerical Recipes ranqd1 LCG on (0, 1).  1664525 * (2^32 - 1) is}
\usage{
.morie_bx_rng(seed)
}
\arguments{
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
below 2^53, so the double arithmetic here is exact and matches the Python arm\\'s 32-bit masked integer step for step.
}
\value{
The value of \code{function}.
}
\keyword{internal}
