% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{elmo_mix}
\alias{elmo_mix}
\title{Eq. (1): gamma * sum_j s_j h_{k,j}}
\usage{
elmo_mix(reps, raw_weights, gamma = 1, position = NULL)
}
\arguments{
\item{reps}{A vector; its length is taken and its elements indexed.}
\item{raw_weights}{A vector; its length is taken.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{position}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the elmo_native implementation. Called by \code{elmo_representation}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
