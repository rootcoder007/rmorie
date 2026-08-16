% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_predict_tree}
\alias{.esl_predict_tree}
\title{.esl_predict_tree}
\usage{
.esl_predict_tree(node, X)
}
\arguments{
\item{node}{See Usage.}
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the esl_native0 implementation. Called by \code{morie_esl_random_forest}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
