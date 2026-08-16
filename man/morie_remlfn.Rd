% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_remlfn}
\alias{morie_remlfn}
\title{morie_remlfn}
\usage{
morie_remlfn(y, group, tol = 1e-10, max_iter = 5000, solver = "auto")
}
\arguments{
\item{y}{See Usage.}
\item{group}{See Usage.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{Defaults to \code{5000}.}
\item{solver}{Defaults to \code{"auto"}.}
}
\description{
Part of the remlfn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{sigma2_a}, \code{sigma2_e}, \code{mu}, \code{loglik}, \code{n_iter}, \code{converged}, \code{icc}, \code{a}, \code{N}, \code{closed_form}, \code{solver}, \code{method}.
}
\keyword{internal}
