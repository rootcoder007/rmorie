% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_predict_gini}
\alias{.tree_predict_gini}
\title{Internal helper: class-probability prediction from a Gini tree}
\usage{
.tree_predict_gini(node, X, K)
}
\description{
Internal helper: class-probability prediction from a Gini tree
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.tree_predict_gini(node = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), K = 5L)
}
