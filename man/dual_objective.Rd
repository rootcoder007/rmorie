% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dual_objective}
\alias{dual_objective}
\title{dual_objective}
\usage{
dual_objective(alpha, y, K)
}
\arguments{
\item{alpha}{See Usage.}
\item{y}{See Usage.}
\item{K}{A matrix; indexed by row and column.}
}
\description{
A step of the svmopt_native implementation. Called by \code{smo}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
