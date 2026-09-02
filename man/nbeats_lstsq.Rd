% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nbeats_lstsq}
\alias{nbeats_lstsq}
\title{X is L x P, y is length L; return theta}
\usage{
nbeats_lstsq(X, y, ridge = 1e-08) 
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the nbeats_native implementation. Called by \code{nbeats_block}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
nbeats_lstsq(V, V)
}
