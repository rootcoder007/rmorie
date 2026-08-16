% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svmopt_bounds}
\alias{.svmopt_bounds}
\title{.svmopt_bounds}
\usage{
.svmopt_bounds(i, j, a, y, C)
}
\arguments{
\item{i}{See Usage.}
\item{j}{See Usage.}
\item{a}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{C}{Numeric; passed to \code{min}.}
}
\description{
A step of the svmopt_native implementation. Called by \code{solve_pair}. See the file header for the source the module follows.
}
\value{
A list with \code{L}, \code{H}.
}
\keyword{internal}
