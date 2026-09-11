% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{flow_forward}
\alias{flow_forward}
\title{Push x through every MAF layer; returns (u, total_alpha)}
\usage{
flow_forward(flow, x, t) 
}
\description{
Push x through every MAF layer; returns (u, total_alpha)
}
\arguments{
\item{flow}{Argument \code{flow}; see Usage.}
\item{x}{Argument \code{x}; see Usage.}
\item{t}{Argument \code{t}; see Usage.}
}
\value{
A list with \code{u}, \code{total}.
}
\keyword{internal}
\examples{
flow <- MAF(dim_x = 2L, dim_t = 2L, n_layers = 2L, hidden = 4L, seed = 1L)
fw <- flow_forward(flow, x = c(0.4, -0.7), t = c(0.1, 0.2))
fw$u        # the latent point x maps to
fw$total    # the log-Jacobian of the map
}
