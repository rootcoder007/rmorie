% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tqipb_variance}
\alias{morie_tqipb_variance}
\title{Variance of the inner-product estimate over packed blocks}
\usage{
morie_tqipb_variance(bits, d, norm_sq = 1, x_norm_sq = 1, n_blocks = 1, route = "table")
}
\arguments{
\item{bits}{Bit-width per coordinate.}
\item{d}{Total dimension.}
\item{norm_sq}{Squared norm of y.}
\item{x_norm_sq}{Squared norm of x; the paper states its bounds for x   on the unit sphere and the distortion is homogeneous in this.}
\item{n_blocks}{Independently rotated and quantized blocks.}
\item{route}{See morie_tqipb_constant.}
}
\description{
Mass is split across blocks in proportion to their dimension, which is the right split for a randomly rotated vector: after the rotation no coordinate is special, so a block of d_j coordinates carries d_j / d of the squared norm in expectation.
}
\value{
A single numeric variance bound.
}
\keyword{internal}
\examples{
morie_tqipb_variance(bits = 5L, d = 5L)
}
