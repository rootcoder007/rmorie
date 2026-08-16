% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LooCv}
\alias{LooCv}
\title{K-fold with K = N.  It uses the most training data of any split, so}
\usage{
LooCv(X, y, classifier = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{classifier}{The body requires: leaves one class; the classifier.}
}
\description{
it is nearly unbiased, and it is deterministic -- there is only one way to leave one out, so unlike 5-fold it gives the same answer every time.  The cost is N fits and a high variance: the training sets differ by a single sample, so the errors are heavily correlated.
}
\value{
A list with \code{error_rate}, \code{accuracy}, \code{errors}, \code{misclassified}, \code{n}, \code{n_fits}, \code{deterministic}, \code{nearly_unbiased}, \code{high_variance}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LooCv(V, V)
}
