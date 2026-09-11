% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{flow_logprob}
\alias{flow_logprob}
\title{log q_phi(x | theta), change of variables}
\usage{
flow_logprob(flow, x, t) 
}
\description{
log q_phi(x | theta), change of variables
}
\arguments{
\item{flow}{Argument \code{flow}; see Usage.}
\item{x}{Argument \code{x}; see Usage.}
\item{t}{Argument \code{t}; see Usage.}
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
flow_logprob(flow, x = c(0.4, -0.7), t = c(0.1, 0.2))
# for one dimension the density integrates to one
f1 <- MAF(dim_x = 1L, dim_t = 1L, n_layers = 3L, hidden = 5L, seed = 4L)
g <- seq(-12, 12, length.out = 2001)
d <- vapply(g, function(x) exp(flow_logprob(f1, x, 0.3)), numeric(1))
round(sum((d[-1] + d[-length(d)]) / 2) * (g[2] - g[1]), 3)
}
