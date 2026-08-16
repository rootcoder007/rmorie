% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.msm_lcg}
\alias{.msm_lcg}
\title{Park-Miller multiplicative LCG: a bootstrap needs an INDEPENDENT}
\usage{
.msm_lcg(seed)
}
\arguments{
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
stream, not a low-discrepancy one -- a shared quasi-random sequence correlates the resample positions and shrinks the bootstrap spread.
}
\value{
The value of \code{function}.
}
\keyword{internal}
\examples{
.msm_lcg(seed = 5L)
}
