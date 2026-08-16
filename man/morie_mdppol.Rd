% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mdppol}
\alias{morie_mdppol}
\title{morie_mdppol}
\usage{
morie_mdppol(P, R, gamma, tol = 1e-12, max_eval = 1e+05, max_improve = 1000, pi0 = NULL)
}
\arguments{
\item{P}{See Usage.}
\item{R}{See Usage.}
\item{gamma}{See Usage.}
\item{tol}{Defaults to \code{1e-12}.}
\item{max_eval}{Defaults to \code{1e+05}.}
\item{max_improve}{Defaults to \code{1000}.}
\item{pi0}{Defaults to \code{NULL}.}
}
\description{
Part of the mdppol_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{policy}, \code{q}, \code{n_improve}, \code{n_eval}, \code{policy_stable}, \code{method}.
}
\keyword{internal}
\examples{
morie_mdppol(P = 0.5, R = c(1, 2, 3, 4, 5, 6, 7, 8), gamma = 0.5)
}
