% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_initial_covariance}
\alias{.sarima_initial_covariance}
\title{.sarima_initial_covariance}
\usage{
.sarima_initial_covariance(T, R, r)
}
\arguments{
\item{T}{A matrix; indexed by row and column.}
\item{R}{A vector; indexed elementwise.}
\item{r}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.filter_column}, \code{loglik}. See the file header for the source the module follows.
}
\value{
The value of \code{P}, as built in the body.
}
\keyword{internal}
