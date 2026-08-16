% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.htprd_ridge_cv}
\alias{.htprd_ridge_cv}
\title{.htprd_ridge_cv}
\usage{
.htprd_ridge_cv(X, y, K, params)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{K}{A count; the body uses it as \code{seq_len(...)}.}
\item{params}{A list; the body reads \code{$lam} from it.}
}
\description{
A step of the htprd implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
