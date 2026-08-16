% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_grow}
\alias{.esl_grow}
\title{Algorithm 15.1 step 1(b). The mtry-of-p subset is drawn at EVERY}
\usage{
.esl_grow(X, y, mtry, depth, max_depth, min_node)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{mtry}{Numeric; passed to \code{min}.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.esl_stop}.}
\item{min_node}{Passed to \code{.esl_stop}.}
}
\description{
node, not once per tree: averaging B identically distributed trees leaves rho sigma^2 behind, so the gain is bounded by how decorrelated they are, and a per-tree draw decorrelates far less.
}
\value{
A list with \code{leaf}, \code{feature}, \code{threshold}, \code{left}, \code{right}.
}
\keyword{internal}
