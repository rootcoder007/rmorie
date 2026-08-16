% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_scale_factor}
\alias{morie_deepar_scale_factor}
\title{Per-series scale factor}
\usage{
morie_deepar_scale_factor(z, t0 = NULL)
}
\arguments{
\item{z}{Numeric vector.}
\item{t0}{Optional integer, number of leading terms to use.}
}
\description{
\code{nu = 1 + (1/t_0) sum_t z_t}. The +1 is what keeps the divisor away from zero for a series that is all zeros, which intermittent series often are.
}
\value{
A single numeric.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_deepar_scale_factor(V)
}
