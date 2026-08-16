% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CadPipe}
\alias{CadPipe}
\title{Chapter 10: a CAD system is a chain, not a classifier, and the}
\usage{
CadPipe(features, labels, k = 5, standardize = TRUE)
}
\arguments{
\item{features}{See Usage.}
\item{labels}{See Usage.}
\item{k}{Defaults to \code{5}.}
\item{standardize}{Defaults to \code{TRUE}.}
}
\description{
accuracy quoted for it means nothing unless the test patterns were unseen. Prototype discriminant of 10.4.1, partitioning of 10.10.3, scored by eqs (10.100)-(10.103).
}
\value{
A list with \code{accuracy}, \code{sensitivity}, \code{specificity}, \code{weightedaccuracy}, \code{confusion}, \code{predictions}, \code{folds}, \code{prevalence}, \code{method}.
}
\keyword{internal}
