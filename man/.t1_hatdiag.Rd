% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t1_hatdiag}
\alias{.t1_hatdiag}
\title{.t1_hatdiag}
\usage{
.t1_hatdiag(X, xtxinv)
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{xtxinv}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the tail1_core implementation. Called by \code{Dfbetas}, \code{Dffitsols}, \code{Olsnormeq}. See the file header for the source the module follows.
}
\value{
The value of \code{rowSums}.
}
\keyword{internal}
