% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{flow_log_density}
\alias{flow_log_density}
\title{Push a sample through a flow and correct the density}
\usage{
flow_log_density(z0, log_q0, layers)
}
\arguments{
\item{z0}{Numeric vector; the base sample.}
\item{log_q0}{Numeric scalar; the base log-density at \code{z0}.}
\item{layers}{List of \code{list(u, w, b)} triples.}
}
\description{
\code{log q_K = log q_0 - sum_k log|det J_k|}. The subtraction is the whole content of the change of variables. Depth 0 leaves the density untouched, which is the mean-field case.
}
\value{
A list with \code{estimate, log_q, z, log_dets, depth,   method, note}.
}
\keyword{internal}
