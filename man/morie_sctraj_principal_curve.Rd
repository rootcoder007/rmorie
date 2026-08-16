% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_principal_curve}
\alias{morie_sctraj_principal_curve}
\title{morie_sctraj_principal_curve}
\usage{
morie_sctraj_principal_curve(X, init, weights = NULL, max_iter = 15, tol = 0.001, span = 0.4, n_knots = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{init}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{15}.}
\item{tol}{Defaults to \code{0.001}.}
\item{span}{Defaults to \code{0.4}.}
\item{n_knots}{Defaults to \code{NULL}.}
}
\description{
Part of the sctraj_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{pseudotime}, \code{curve}, \code{distance}, \code{sse}.
}
\keyword{internal}
