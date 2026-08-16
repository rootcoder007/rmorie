% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_one_vs_rest_hm}
\alias{morie_geron_one_vs_rest_hm}
\title{One-vs-rest multiclass: K binary classifiers, argmax over scores (Geron Ch 3, hmovr)}
\usage{
morie_geron_one_vs_rest_hm(X, y, base_estimator = NULL, X_new = NULL)
}
\arguments{
\item{X,y}{Data and class labels.}
\item{base_estimator}{Optional `base_estimator(X, yb) -> score fn`.}
\item{X_new}{Optional rows to classify.}
}
\description{
One-vs-rest multiclass: K binary classifiers, argmax over scores (Geron Ch 3, hmovr)
}
\value{
A list with \code{predict}, \code{predictions}, \code{classes}, \code{n_classifiers}, \code{scores}, \code{margin}, \code{positive_rate}, \code{accuracy}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
