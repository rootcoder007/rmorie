% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{offlrl}
\alias{offlrl}
\title{offlrl}
\usage{
offlrl(dataset, states = NULL, actions = NULL, alpha = 1, gamma = 0.99, variant = "H", backup = "max", policy = NULL, mu = NULL, lr = 0.5, iters = 2000, tol = 1e-12)
}
\arguments{
\item{dataset}{See Usage.}
\item{states}{Defaults to \code{NULL}.}
\item{actions}{Defaults to \code{NULL}.}
\item{alpha}{Defaults to \code{1}.}
\item{gamma}{Defaults to \code{0.99}.}
\item{variant}{Defaults to \code{"H"}.}
\item{backup}{Defaults to \code{"max"}.}
\item{policy}{Defaults to \code{NULL}.}
\item{mu}{Defaults to \code{NULL}.}
\item{lr}{Defaults to \code{0.5}.}
\item{iters}{Defaults to \code{2000}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
Part of the offlrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{value}, \code{greedy}, \code{behavior}, \code{counts}, \code{penalty}, \code{bellman_error}, \code{objective}, \code{alpha}, \code{variant}, \code{backup}, \code{n_transitions}, \code{method}.
}
\keyword{internal}
