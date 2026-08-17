% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_variable_importance}
\alias{morie_variable_importance}
\title{Loop the contrast over every column of X in turn}
\usage{
morie_variable_importance(y, X, candidate_strata = NULL, method = "cv-tmle", n_folds = 10, names = NULL, ...)
}
\arguments{
\item{y}{Outcome vector.}
\item{X}{Covariate matrix.}
\item{candidate_strata}{Candidate levels.}
\item{method}{One of \code{"cv-tmle"}, \code{"sample-split"},   \code{"naive"}.}
\item{n_folds}{Number of folds.}
\item{names}{Optional column names.}
\item{...}{Passed through to \code{tmle_data_adaptive}.}
}
\description{
Loop the contrast over every column of X in turn
}
\value{
A list of per-variable results, sorted by absolute estimate.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 120
X <- cbind(rbinom(n, 2, 0.5), rbinom(n, 1, 0.5), rbinom(n, 2, 0.4))
y <- plogis(0.6 * X[, 1] + 0.4 * X[, 2] + rnorm(n, 0, 0.5))
r <- morie_variable_importance(y, X, method = "naive")
str(r, max.level = 1)
}
