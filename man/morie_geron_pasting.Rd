% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pasting}
\alias{morie_geron_pasting}
\title{Pasting: base models on samples drawn without replacement (Geron Ch 6, hmpas)}
\usage{
morie_geron_pasting(X, y, base_estimator = NULL, n_estimators = 10, sample_size = NULL, 
    seed = 0, task = "auto") 
}
\arguments{
\item{X,y}{Data and targets.}
\item{base_estimator}{Optional `base_estimator(Xs, ys) -> predict`.}
\item{n_estimators}{Ensemble size.}
\item{sample_size}{Rows per model (int or fraction).}
\item{seed}{Integer-LCG seed.}
\item{task}{"auto", "regression" or "classification".}
}
\description{
Pasting: base models on samples drawn without replacement (Geron Ch 6, hmpas)
}
\value{
A list with \code{predict}, \code{train_pred}, \code{train_mse}, \code{oob_pred}, \code{oob_mse}, \code{samples}, \code{estimators}, \code{sample_size}, \code{task}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_pasting(V, V)
}
