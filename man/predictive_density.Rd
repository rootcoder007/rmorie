% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{predictive_density}
\alias{predictive_density}
\title{Predictive density}
\usage{
predictive_density(grid, cluster_params, counts, alpha, kernel, base_predictive)
}
\arguments{
\item{grid}{Numeric vector of evaluation points.}
\item{cluster_params}{Parameters of the occupied clusters,   one per entry in \code{counts} (may be any R object the   user\'s \code{kernel} knows how to consume).}
\item{counts}{Numeric vector of positive cluster occupancies.}
\item{alpha}{Concentration, positive.}
\item{kernel}{Function \code{(y, theta)} returning the kernel   value at \code{y} for cluster parameter \code{theta}.}
\item{base_predictive}{Function \code{(y)} returning the   prior predictive \code{integrate k(y|theta) dG_0(theta)}.}
}
\description{
The DP mixture posterior predictive density at each point of \code{grid}: a weighted sum over the occupied clusters of their kernel values, plus one term \code{alpha/(alpha+n) * base_predictive(y)} for a cluster not yet seen. The new-cluster weight is reported separately because it IS the model\'s stated probability that the next observation is unlike everything seen so far.
}
\value{
A list with \code{estimate}, \code{density},   \code{grid}, \code{new_cluster_weight},   \code{occupied_weights}, \code{K}, \code{n}, \code{method},   \code{note}.
}
\keyword{internal}
