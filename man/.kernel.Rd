% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kernel}
\alias{.kernel}
\title{.kernel}
\usage{
.kernel(name)
}
\arguments{
\item{name}{One of \code{"matern52"}, \code{"se"}.}
}
\description{
A step of the bayopt_native implementation. Called by \code{gp_posterior}, \code{gp_posterior_gradient}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
