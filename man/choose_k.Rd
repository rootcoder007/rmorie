% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{choose_k}
\alias{choose_k}
\title{choose_k}
\usage{
choose_k(graph_sizes, coverage = 0.6) 
}
\arguments{
\item{graph_sizes}{Coerced to integer by the body, with \code{as.integer}.}
\item{coverage}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.6}.}
}
\description{
A step of the sortP_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{k}, \code{coverage}, \code{fraction_untruncated}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
choose_k(V)
}
