% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.r2}
\alias{.r2}
\title{.r2}
\usage{
.r2(a, b, ls)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; indexed elementwise.}
\item{ls}{A vector; indexed elementwise.}
}
\description{
A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}, \code{matern52}, \code{squared_exponential}. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
.r2(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, ls = c(1, 2, 3, 4, 5, 6, 7, 8))
}
