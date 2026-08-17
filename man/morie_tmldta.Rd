% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmldta}
\alias{morie_tmldta}
\title{Contrast between data-discovered exposure levels, done honestly}
\usage{
morie_tmldta(y, D, X, candidate_strata = NULL, method = "cv-tmle", n_folds = 10, trim = 0.01, ridge = 1e-08, level = 0.95, bounds = NULL)
}
\arguments{
\item{y}{Outcome vector.}
\item{D}{Exposure vector.}
\item{X}{Covariate matrix W.}
\item{candidate_strata}{Candidate levels.}
\item{method}{One of \code{"cv-tmle"}, \code{"sample-split"},   \code{"naive"}.}
\item{n_folds}{Number of folds.}
\item{trim}{Propensity trimming.}
\item{ridge}{Ridge regulariser.}
\item{level}{Confidence level.}
\item{bounds}{Optional \code{c(lo, hi)}; otherwise inferred.}
}
\description{
\code{cv-tmle} fits Q and g on the parameter-generating split and only the fluctuation on the estimation split. \code{sample-split} fits everything on the estimation sample. \code{naive} is the substitution estimator 9.5 with no split at all -- kept because its bias is the point of the chapter.
}
\value{
A list with \code{estimate}, \code{se}, \code{n},   \code{ci}, \code{level}, \code{levels_by_split},   \code{level_counts}, \code{modal_levels}, \code{level_agreement},   \code{separation}, \code{near_tie}, \code{level_means},   \code{split_estimates}, \code{n_splits}, \code{epsilon},   \code{candidate_levels}, \code{method}, \code{sigma},   \code{algorithm}.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 120
X <- matrix(rnorm(n * 2), n, 2)
D <- rbinom(n, 1, 0.5)
y <- plogis(0.8 * D + X[, 1] + rnorm(n, 0, 0.4))
r <- morie_tmldta(y, D, X, method = "sample-split")
str(r, max.level = 1)
}
