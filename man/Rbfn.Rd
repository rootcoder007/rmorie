% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Rbfn}
\alias{Rbfn}
\title{Rbfn}
\usage{
Rbfn(X, y, ncenters = NULL, spread = 1, centers = NULL, ridge = 1e-08, query = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{ncenters}{Defaults to \code{NULL}.}
\item{spread}{Defaults to \code{1}.}
\item{centers}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{query}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{centers}, \code{weights}, \code{bias}, \code{predictions}, \code{queryvalues}, \code{mse}, \code{spread}, \code{method}.
}
\keyword{internal}
