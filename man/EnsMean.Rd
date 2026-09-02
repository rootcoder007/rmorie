% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EnsMean}
\alias{EnsMean}
\title{Eq (3.15): mu_x(t1) = (1/M) sum_k x_k(t1)}
\usage{
EnsMean(observations, index = NULL) 
}
\arguments{
\item{observations}{Passed to \code{.morie_rg_aslist}.}
\item{index}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_ch3 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{m}, \code{sd}, \code{se}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
EnsMean(V)
}
