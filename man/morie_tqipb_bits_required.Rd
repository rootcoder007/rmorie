% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tqipb_bits_required}
\alias{morie_tqipb_bits_required}
\title{Smallest bit-width meeting a target (eps, delta)}
\usage{
morie_tqipb_bits_required(eps, delta, d, norm_sq = 1, x_norm_sq = 1, n_blocks = 1, route = "table", tail = "chebyshev", max_bits = 32)
}
\arguments{
\item{eps}{Relative accuracy.}
\item{delta}{Target failure probability.}
\item{d}{Total dimension.}
\item{norm_sq}{Squared norm of y.}
\item{x_norm_sq}{Squared norm of x.}
\item{n_blocks}{Independently quantized blocks.}
\item{route}{Variance route.}
\item{tail}{Tail inequality.}
\item{max_bits}{Largest bit-width searched.}
}
\description{
Searched rather than inverted: the "table" route is not a closed-form function of b below 5 bits, and a search over at most 32 values costs nothing.
}
\value{
The bit-width, or NULL if none within max_bits.
}
\keyword{internal}
\examples{
morie_tqipb_bits_required(eps = 0.5, delta = 0.5, d = 5L)
}
