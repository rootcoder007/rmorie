% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.thrtmt_lstsq}
\alias{.thrtmt_lstsq}
\title{Z: n x p, y: length n}
\usage{
.thrtmt_lstsq(Z, y, ridge)
}
\arguments{
\item{Z}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{ridge}{A matrix; passed to \code{diag}.}
}
\description{
Returns b solving (Z\\'Z + ridge I) b = Z\\' y
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
