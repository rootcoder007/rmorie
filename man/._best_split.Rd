% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._best_split}
\alias{._best_split}
\title{._best_split}
\usage{
._best_split(X, y, rows, feats, min_leaf, alpha)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{rows}{A vector; its length is taken.}
\item{feats}{See Usage.}
\item{min_leaf}{Numeric; passed to \code{max}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the hntfst_native implementation. Called by \code{honest_tree}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
