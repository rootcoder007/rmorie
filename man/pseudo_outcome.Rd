% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pseudo_outcome}
\alias{pseudo_outcome}
\title{Kennedy et al. Theorem 1\'s xi(Z; pi, mu), one value per row}
\usage{
pseudo_outcome(y, A, X, ridge = 1e-08) 
}
\arguments{
\item{y}{Outcome vector.}
\item{A}{Treatment vector.}
\item{X}{Covariate matrix.}
\item{ridge}{Ridge regulariser.}
}
\description{
The nuisances are a Gaussian conditional treatment density and a linear outcome regression with a treatment interaction; both are the working models, and the point of the construction is that only one of them has to be right.
}
\value{
A list with \code{xi} and \code{info} (marginal_density,   standardized_mu, treatment_coef, treatment_sigma2, outcome_coef,   pi_obs).
}
\keyword{internal}
\examples{
pseudo_outcome(y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8),
  X = c(1, 2, 3, 4, 5, 6, 7, 8))
}
