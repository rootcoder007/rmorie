% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Knn}
\alias{Knn}
\title{Eq (10.29) and Section 10.4.4.  The book is explicit about why k > 1:}
\usage{
Knn(X, y, query, k = 1, metric = "euclidean", C = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{query}{See Usage.}
\item{k}{Defaults to \code{1}.}
\item{metric}{Defaults to \code{"euclidean"}.}
\item{C}{Defaults to \code{NULL}.}
}
\description{
with k = 1 "the nearest neighbor may happen to be an outlier that is not representative of its class", so one freak point owns a whole region.  The Mahalanobis metric is the one to use when features have different units, since Euclidean distance is otherwise dominated by whichever feature has the largest numbers.
}
\value{
A list with \code{assigned}, \code{votes}, \code{k}, \code{metric}, \code{neighbours}, \code{tie}, \code{tied_classes}, \code{nearest_distance}, \code{nearest_label}, \code{single_neighbour_may_be_an_outlier}, \code{method}.
}
\keyword{internal}
