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
flow_forward(flow = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), x = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
