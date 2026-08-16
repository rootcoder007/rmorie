% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t1_lstsq}
\alias{.t1_lstsq}
\title{Minimum-norm least squares via the SVD, matching numpy.linalg.lstsq}
\usage{
.t1_lstsq(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
and so the Python arm\\'s _lstsq.
}
\value{
A list with \code{beta}, \code{fitted}, \code{resid}, \code{xtxinv}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t1_lstsq(V, V)
}
