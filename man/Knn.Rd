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
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{query}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{metric}{One of \code{"euclidean"}, \code{"mahalanobis"}. Defaults to \code{"euclidean"}.}
\item{C}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
with k = 1 "the nearest neighbor may happen to be an outlier that is not representative of its class", so one freak point owns a whole region.  The Mahalanobis metric is the one to use when features have different units, since Euclidean distance is otherwise dominated by whichever feature has the largest numbers.
}
\value{
A list with \code{assigned}, \code{votes}, \code{k}, \code{metric}, \code{neighbours}, \code{tie}, \code{tied_classes}, \code{nearest_distance}, \code{nearest_label}, \code{single_neighbour_may_be_an_outlier}, \code{method}.
}
\keyword{internal}
\examples{
Knn(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), query = 5L)
}
