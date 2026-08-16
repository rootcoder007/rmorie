% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lengths}
\alias{.lengths}
\title{.lengths}
\usage{
.lengths(ls, d)
}
\arguments{
\item{ls}{A vector; its length is taken.}
\item{d}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{gp_posterior_gradient}, \code{matern52}, \code{squared_exponential}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
