% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{q_values}
\alias{q_values}
\title{q_values}
\usage{
q_values(T, R, gamma, V)
}
\arguments{
\item{T}{A vector; indexed elementwise.}
\item{R}{A vector; indexed elementwise.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{V}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the birl_native implementation. Called by \code{policy_iteration}, \code{policy_walk}. See the file header for the source the module follows.
}
\value{
The value of \code{Q}, as built in the body.
}
\keyword{internal}
