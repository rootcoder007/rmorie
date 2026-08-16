% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_scintg}
\alias{morie_scintg}
\title{morie_scintg}
\usage{
morie_scintg(Z, batches, K = NULL, sigma = 0.1, theta = 2, lam = 1, max_iter = 10, cluster_iter = 25, tol = 1e-04, seed = 0, reference = NULL, diversity = "penalise")
}
\arguments{
\item{Z}{See Usage.}
\item{batches}{See Usage.}
\item{K}{Defaults to \code{NULL}.}
\item{sigma}{Defaults to \code{0.1}.}
\item{theta}{Defaults to \code{2}.}
\item{lam}{Defaults to \code{1}.}
\item{max_iter}{Defaults to \code{10}.}
\item{cluster_iter}{Defaults to \code{25}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{seed}{Defaults to \code{0}.}
\item{reference}{Defaults to \code{NULL}.}
\item{diversity}{Defaults to \code{"penalise"}.}
}
\description{
Part of the scintg_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{embedding}, \code{R}, \code{Y}, \code{K}, \code{objective}, \code{history}, \code{n_rounds}, \code{theta}, \code{sigma}, \code{lam}, \code{diversity}, \code{method}, \code{note}.
}
\keyword{internal}
