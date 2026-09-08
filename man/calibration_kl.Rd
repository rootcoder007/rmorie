% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{calibration_kl}
\alias{calibration_kl}
\title{\eqn{KL(p \,\|\, (1-\alpha)q + \alpha p)}}
\usage{
calibration_kl(p, q, alpha = 0.01) 
}
\arguments{
\item{p}{Target distribution \eqn{p(g \mid u)}.}
\item{q}{Recommended distribution \eqn{q(g \mid u)}.}
\item{alpha}{Smoothing weight in \eqn{(0, 1)}.}
}
\description{
Implements eqs. (4)-(5): the KL divergence from the target distribution \eqn{p} to the alpha-smoothed recommended distribution \eqn{\tilde q = (1-\alpha) q + \alpha p}. The small alpha (0.01 in the paper) is needed because \eqn{q} can be zero where \eqn{p} is not.
}
\value{
Numeric scalar, the KL value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
calibration_kl(V, V)
}
