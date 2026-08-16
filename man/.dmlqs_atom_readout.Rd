% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_atom_readout}
\alias{.dmlqs_atom_readout}
\title{.dmlqs_atom_readout}
\usage{
.dmlqs_atom_readout(edge_states, adj, n)
}
\arguments{
\item{edge_states}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{adj}{Passed to \code{.dmlqs_norm_adj}.}
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{morie_dmlqs}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
