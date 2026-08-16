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
\value{
A numeric value.
}
\keyword{internal}
\examples{
flow_logprob(flow = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), x = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
