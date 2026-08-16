% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tqipb_constant}
\alias{morie_tqipb_constant}
\title{Per-unit-norm inner-product distortion constant}
\usage{
morie_tqipb_constant(bits, d, route = "table")
}
\arguments{
\item{bits}{Bit-width per coordinate, at least 1.}
\item{d}{Block dimension.}
\item{route}{One of "table" (the paper's solved b = 1..4 constants,   falling back to Panter-Dite above 4 bits, where the high-resolution   form is accurate), "panter_dite" (Theorem 2), "qjl" (Lemma 4, one   bit, independent of b) or "lower_bound" (Theorem 3).}
}
\description{
The returned c satisfies E[(<x_hat, y> - <x, y>)^2] <= c ||x||^2 ||y||^2 for the chosen route.
}
\value{
A single numeric constant.
}
\keyword{internal}
\examples{
morie_tqipb_constant(bits = 5L, d = 5L)
}
