% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{adapted_pagerank}
\alias{adapted_pagerank}
\title{adapted_pagerank}
\usage{
adapted_pagerank(adjacency, nodes, p = NULL, d = 0.7, iters = 200, tol = 1e-12) 
}
\arguments{
\item{adjacency}{A vector; indexed elementwise.}
\item{nodes}{Coerced to list by the body, with \code{as.list}.}
\item{p}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{d}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.7}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-12}.}
}
\description{
A step of the tagRC_native implementation. Called by \code{folkrank}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{ranking}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
S <- c("a", "b", "c")
adapted_pagerank(D, S)
}
