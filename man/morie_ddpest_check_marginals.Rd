% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_check_marginals}
\alias{morie_ddpest_check_marginals}
\title{Every G_x must still be a probability measure}
\usage{
morie_ddpest_check_marginals(G, tol = 1e-09)
}
\arguments{
\item{G}{A collection as returned by single_weights or single_atoms.}
\item{tol}{Tolerance on |sum(weights) - 1|.}
}
\description{
Every G_x must still be a probability measure
}
\value{
Named list with ok, offenders, n_x, note.
}
\keyword{internal}
