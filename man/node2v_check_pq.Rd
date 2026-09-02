% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{node2v_check_pq}
\alias{node2v_check_pq}
\title{node2v_check_pq}
\usage{
node2v_check_pq(p, q) 
}
\arguments{
\item{p}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{q}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the node2v_native implementation. Called by \code{alpha_pq}. See the file header for the source the module follows.
}
\value{
A list with \code{p}, \code{q}.
}
\keyword{internal}
\examples{
node2v_check_pq(p = 0.5, q = 0.5)
}
