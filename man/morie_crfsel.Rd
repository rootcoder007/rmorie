% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crfsel}
\alias{morie_crfsel}
\title{Rank covariates by how the CATE forest uses them}
\usage{
morie_crfsel(y, W, X, n_trees = 200L, min_leaf = 5L, max_depth = 4L, decay = 2, seed = 0L, names = NULL, permutation = FALSE)
}
\arguments{
\item{y}{Numeric outcome vector.}
\item{W}{Numeric treatment vector.}
\item{X}{Numeric covariate matrix.}
\item{n_trees}{Integer number of trees.}
\item{min_leaf}{Integer minimum leaf size.}
\item{max_depth}{Integer, depths to consider.}
\item{decay}{Numeric, depth decay.}
\item{seed}{Integer seed.}
\item{names}{Optional character vector of covariate names.}
\item{permutation}{Logical, also compute permutation importance.}
}
\description{
Grows a forest on the treatment-residualised pseudo-outcome, scores each covariate by depth-weighted split frequency and (optionally) by permutation error rise, and returns the ranking with the same field names as the Python arm.
}
\value{
A list mirroring the Python arm\'s payload.
}
\keyword{internal}
