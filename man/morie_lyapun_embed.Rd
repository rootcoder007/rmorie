% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_lyapun_embed}
\alias{morie_lyapun_embed}
\title{Largest Lyapunov exponent from a scalar time series}
\usage{
morie_lyapun_embed(y, m, tau)
}
\description{
Largest Lyapunov exponent from a scalar time series. "A practical method for calculating largest Lyapunov exponents from small data sets", *Physica D* 65(1-2), 117-134. The attractor is reconstructed by delay embedding (the paper's eq. 5), each point's nearest neighbour is found subject to a temporal separation larger than the mean period (eqs. 7 and 8), and the neighbours are then assumed to separate at the rate of the largest exponent, d_j(i) ~= C_j exp(lambda_1 * i * dt) (eq. 11). Taking logs (eq. 12) turns that into a family of roughly parallel lines, and the exponent is the slope of their average (eq. 13),   y(i) = (1/dt) * <ln d_j(i)>_j. The averaging over j is what makes the estimate work on short series, and the paper's point about C_j is that no normalisation by d_j(0) is needed, since a constant offset does not change a slope. Three routes are available, all printed in the paper, and all reachable through `method`:   "rosenstein" (default) - eq. 13, least squares on y(i) over the     initial rise.   "sato" - Sato et al.'s eq. 9, read at the end of the fitting window.   "sato_k" - Sato et al.'s eq. 10, lambda_1 read off the plateau in i. Table 1 of the paper gives the expected exponents this module is anchored against: 0.693 for the logistic map at mu = 4 and 0.418 for the Henon map at a = 1.4, b = 0.3.
}
\value{
A list with components \code{m}, \code{J}.
}
\references{
Sources: Rosenstein, M. T., Collins, J. J., & De Luca, C. J. (1993)
}
\keyword{internal}
