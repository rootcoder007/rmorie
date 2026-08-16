% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_randomized_search}
\alias{morie_geron_randomized_search}
\title{Randomized hyperparameter search, scored by K-fold CV (Geron Ch 2, hmrsc)}
\usage{
morie_geron_randomized_search(param_dist, n_iter, X, y, estimator = NULL, K = 3, seed = 0, score = NULL)
}
\arguments{
\item{param_dist}{Named list of distributions: an R `list(lo, hi)` of length 2 is a uniform   INTERVAL (Python\'s `(lo, hi)` tuple); a plain vector is a discrete CHOICE list (Python\'s   `\[...\]`), matching Python\'s own tuple-vs-list distinction. A function is `f(u)`.}
\item{n_iter,X,y,estimator,K,seed,score}{Controls; `estimator(params) -> list(fit=, predict=)`.}
}
\description{
Randomized hyperparameter search, scored by K-fold CV (Geron Ch 2, hmrsc)
}
\value{
A list with \code{best_params}, \code{best_score}, \code{best_index}, \code{candidates}, \code{scores}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
