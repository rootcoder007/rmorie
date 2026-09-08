% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{target_dimension}
\alias{target_dimension}
\title{Johnson-Lindenstrauss target dimension}
\usage{
target_dimension(n, epsilon, beta = 1) 
}
\arguments{
\item{n}{Number of points, integer \code{>= 2}.}
\item{epsilon}{Distortion tolerance, in \code{(0, 1)}.}
\item{beta}{Confidence parameter, positive; the failure   probability is \code{n^-beta}.}
}
\description{
Theorem 1.1 from Achlioptas (2003): for \code{n} points and \code{epsilon}, \code{beta} > 0, \code{k0 = (4 + 2 beta) log n / (epsilon^2/2 - epsilon^3/3)}. Any \code{k >= k0} achieves every pairwise squared distance within \code{1 +/- epsilon} with probability at least \code{1 - n^-beta}.
}
\value{
A list with \code{k0}, \code{k} (the smallest integer   \code{>= k0}), \code{n}, \code{epsilon}, \code{beta},   \code{failure_probability}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- target_dimension(n = 8L, epsilon = 0.5); TRUE
}
