% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj}
\alias{morie_sctraj}
\title{morie_sctraj}
\usage{
morie_sctraj(X, labels, root, ends = NULL, cov = "full", max_iter = 15, shrink = TRUE, weight_arg = "shifted", span = 0.4, n_points = 100)
}
\arguments{
\item{X}{Passed to \code{.sctraj_matrix}.}
\item{labels}{Coerced to character by the body, with \code{as.character}.}
\item{root}{Passed to \code{morie_sctraj_lineages_from_tree}.}
\item{ends}{Passed to \code{morie_sctraj_minimum_spanning_tree}.}
\item{cov}{Passed to \code{morie_sctraj_cluster_distances}. Defaults to \code{"full"}.}
\item{max_iter}{Passed to \code{morie_sctraj_principal_curve}. Defaults to \code{15}.}
\item{shrink}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{weight_arg}{Passed to \code{morie_sctraj_shrinkage_weight}. Defaults to \code{"shifted"}.}
\item{span}{Passed to \code{morie_sctraj_principal_curve}. Defaults to \code{0.4}.}
\item{n_points}{Passed to \code{morie_sctraj_average_curve}. Defaults to \code{100}.}
}
\description{
A step of the sctraj_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{pseudotime}, \code{weights}, \code{lineages}, \code{curves}, \code{tree}, \code{distance}, \code{clusters}, \code{centers}, \code{n_lineages}, \code{root}, \code{cov}, \code{shrink}, \code{method}, \code{note}.
}
\keyword{internal}
