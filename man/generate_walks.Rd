% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{generate_walks}
\alias{generate_walks}
\title{generate_walks}
\usage{
generate_walks(adj, num_walks = 10, length = 10, p = 1, q = 1, seed = 0, weights = NULL) 
}
\arguments{
\item{adj}{Passed to \code{names}.}
\item{num_walks}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{length}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{p}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{q}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{weights}{Passed to \code{walk}.}
}
\description{
A step of the node2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{walks}, \code{p}, \code{q}, \code{n_walks}, \code{length}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
generate_walks(V)
}
