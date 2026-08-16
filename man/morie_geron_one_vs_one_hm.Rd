% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_one_vs_one_hm}
\alias{morie_geron_one_vs_one_hm}
\title{One-vs-one multiclass: K(K-1)/2 pairwise classifiers with voting (Geron Ch 3, hmovo)}
\usage{
morie_geron_one_vs_one_hm(X, y, base_estimator = NULL, X_new = NULL)
}
\arguments{
\item{X,y}{Data and class labels.}
\item{base_estimator}{Optional `base_estimator(Xp, yp) -> predict`.}
\item{X_new}{Optional rows to classify (default `X`).}
}
\description{
One-vs-one multiclass: K(K-1)/2 pairwise classifiers with voting (Geron Ch 3, hmovo)
}
\value{
A list with \code{predict}, \code{predictions}, \code{classes}, \code{pairs}, \code{n_classifiers}, \code{votes}, \code{accuracy}, \code{tie_fraction}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
