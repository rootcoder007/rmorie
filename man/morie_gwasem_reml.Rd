% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwasem_reml}
\alias{morie_gwasem_reml}
\title{REML variance component estimation (step 2)}
\usage{
morie_gwasem_reml(y, kinship, covariates = NULL, ml = FALSE)
}
\arguments{
\item{y}{Phenotype vector.}
\item{kinship}{n x n relatedness matrix.}
\item{covariates}{Optional n x q covariate matrix.}
\item{ml}{Use ML rather than REML.}
}
\description{
Estimates \code{sigma_a^2} and \code{sigma_e^2} in equation 6. Returns the components, the pseudo-heritability, the restricted log-likelihood, the null log-likelihood (at \code{sigma_a^2 = 0}) and the LRT statistic for \code{H_0: sigma_a^2 = 0}.
}
\value{
A list with the components and supporting quantities.
}
\keyword{internal}
