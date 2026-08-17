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
\examples{
layers <- list(list(u = c(0.3, 0.1), w = c(0.2, -0.4), b = 0.1),
               list(u = c(-0.1, 0.2), w = c(0.3, 0.1), b = 0))
r <- flow_log_density(z0 = c(0.5, -0.2), log_q0 = -1.5, layers)
str(r, max.level = 1)
}
