% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rndnet_combine_returns}
\alias{morie_rndnet_combine_returns}
\title{Public: section 2.3, R = R_E + R_I as two value heads. The extrinsic}
\usage{
morie_rndnet_combine_returns(reward_ext, reward_int, gamma_ext = 0.999, gamma_int = 0.99, done = NULL)
}
\arguments{
\item{reward_ext}{See Usage.}
\item{reward_int}{See Usage.}
\item{gamma_ext}{Numeric; combined arithmetically in the body. Defaults to \code{0.999}.}
\item{gamma_int}{Numeric; combined arithmetically in the body. Defaults to \code{0.99}.}
\item{done}{Defaults to \code{NULL}.}
}
\description{
stream is episodic (truncated at each `done`), the intrinsic stream is non-episodic.
}
\value{
A list with \code{estimate}, \code{return_ext}, \code{return_int}, \code{return_total}, \code{gamma_ext}, \code{gamma_int}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rndnet_combine_returns(V, V)
}
