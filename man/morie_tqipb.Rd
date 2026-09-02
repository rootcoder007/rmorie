% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tqipb}
\alias{morie_tqipb}
\title{Inner-product distortion and tail bound for TurboQuant blocks}
\usage{
morie_tqipb(bits, norm_sq = 1, d = NULL, eps = 0.1, delta = NULL, x_norm_sq = 1, 
    n_blocks = 1, route = "table", tail = "chebyshev") 
}
\arguments{
\item{bits}{Bit-width per coordinate, at least 1.}
\item{norm_sq}{Squared norm of the vector the quantized one is dotted   against.}
\item{d}{Dimension. Required.}
\item{eps}{Relative accuracy in the tail statement.}
\item{delta}{If given, the result also carries the smallest bit-width   attaining this on the chosen route and tail.}
\item{x_norm_sq}{Squared norm of x; the paper's statements take x on   the unit sphere.}
\item{n_blocks}{Independently rotated blocks the coordinates are   packed into.}
\item{route}{Variance route: "table", "panter_dite", "qjl" or   "lower_bound".}
\item{tail}{Tail inequality: "chebyshev" or "sub_gaussian".}
}
\description{
Inner-product distortion and tail bound for TurboQuant blocks
}
\value{
A list with the variance, rmse, relative error, tail bound,   the constant, the block dimensions, and the route, tail and   assumption each carries. bits_needed is present when delta is given.
}
\keyword{internal}
\examples{
morie_tqipb(4L, norm_sq = 9, d = 512L, eps = 0.05, delta = 0.01)
}
