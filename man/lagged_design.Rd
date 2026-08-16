% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lagged_design}
\alias{lagged_design}
\title{Covariates at time `k_time` together with `lag` earlier values}
\usage{
lagged_design(L_hist, Y_hist = NULL, k_time = 0, lag = 1)
}
\arguments{
\item{L_hist}{See Usage.}
\item{Y_hist}{Defaults to \code{NULL}.}
\item{k_time}{Defaults to \code{0}.}
\item{lag}{Defaults to \code{1}.}
}
\description{
Part of the lggvls_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.lggvls_lagged_design}.
}
\references{
----------
Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period -- application to control of
the healthy worker survivor effect", Mathematical Modelling 7(9-12),
1393-1512, doi:10.1016/0270-0255(86)90088-6.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
and Ch. 20 for why conditioning fails where weighting works.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
lagged_design(V)
}
