% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{renyi_divergence_gaussian}
\alias{renyi_divergence_gaussian}
\title{Renyi divergence D_alpha between two Gaussians of equal variance}
\usage{
renyi_divergence_gaussian(mu0, mu1, sigma, alpha) 
}
\arguments{
\item{mu0}{Mean of the first Gaussian.}
\item{mu1}{Mean of the second Gaussian.}
\item{sigma}{Standard deviation, must be positive.}
\item{alpha}{Renyi order, must lie strictly in (1, inf).}
}
\description{
Equals alpha (mu0 - mu1)^2 / (2 sigma^2) -- linear in alpha, which is what makes the Gaussian mechanism sit exactly on the zCDP boundary rather than inside it.
}
\value{
A scalar numeric: the Renyi divergence.
}
\keyword{internal}
