% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_did_forest}
\alias{morie_didfst_did_forest}
\title{Heterogeneous ATT: DiD taken locally under forest weights}
\usage{
morie_didfst_did_forest(Y, D, X, event_time, x_eval = NULL, n_trees = 200L, min_leaf = 5L, alpha = 0.05, max_depth = 12L, seed = 0L, kind = "double-sample", clusters = NULL)
}
\arguments{
\item{Y}{Numeric panel (n by T).}
\item{D}{Numeric vector of 0/1 adoption indicators.}
\item{X}{Numeric covariate matrix.}
\item{event_time}{Integer H in 1-based period numbers.}
\item{x_eval}{Optional evaluation points.}
\item{n_trees}{Integer number of trees.}
\item{min_leaf}{Integer minimum leaf size.}
\item{alpha}{Numeric honesty fraction.}
\item{max_depth}{Integer, maximum tree depth.}
\item{seed}{Integer seed.}
\item{kind}{One of \code{"single-sample"} or \code{"double-sample"}.}
\item{clusters}{Optional cluster assignment vector.}
}
\description{
Heterogeneous ATT: DiD taken locally under forest weights
}
\value{
A list with \code{tau}, \code{att_uniform} and the   evaluation metadata.
}
\keyword{internal}
