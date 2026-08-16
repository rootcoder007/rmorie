% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svdd}
\alias{morie_svdd}
\title{morie_svdd}
\usage{
morie_svdd(X, C = NULL, nu = NULL, kernel = "rbf", gamma = NULL, degree = 3, coef0 = 1, tol = 1e-10, max_iter = 20000)
}
\arguments{
\item{X}{See Usage.}
\item{C}{Defaults to \code{NULL}.}
\item{nu}{Defaults to \code{NULL}.}
\item{kernel}{Defaults to \code{"rbf"}.}
\item{gamma}{Defaults to \code{NULL}.}
\item{degree}{Defaults to \code{3}.}
\item{coef0}{Defaults to \code{1}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{Defaults to \code{20000}.}
}
\description{
Part of the svdd_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{R2}, \code{radius}, \code{center}, \code{support_}, \code{boundary_}, \code{bounded_}, \code{n_support}, \code{degenerate}, \code{distance2}, \code{outlier_fraction}, \code{outlier_bound}, \code{decision}, \code{predict}, \code{C}, \code{kernel}, \code{gamma}, \code{n}, \code{method}.
}
\keyword{internal}
