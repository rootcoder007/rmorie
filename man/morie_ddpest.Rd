% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest}
\alias{morie_ddpest}
\title{Dependent Dirichlet process entry point}
\usage{
morie_ddpest(method, ...)
}
\arguments{
\item{method}{Operation name (character).}
\item{...}{Forwarded to the chosen operation.}
}
\description{
Dispatches to one of the operations on the DDP collection: "dependence_kind", "single_weights", "single_atoms", "check_marginals", "correlation", "predict_density", "cheatsheet". Each operation mirrors its Python counterpart in morie.fn.ddpest in argument names, validation and return-field names.
}
\value{
Whatever the chosen operation returns.
}
\keyword{internal}
