% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{moments_accountant}
\alias{moments_accountant}
\title{moments_accountant}
\usage{
moments_accountant(vote_counts, gamma, delta, lambdas = NULL, data_dependent = TRUE)
}
\arguments{
\item{vote_counts}{See Usage.}
\item{gamma}{See Usage.}
\item{delta}{See Usage.}
\item{lambdas}{Defaults to \code{NULL}.}
\item{data_dependent}{Defaults to \code{TRUE}.}
}
\description{
Part of the pate_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{epsilon}, \code{lambda}, \code{alpha}, \code{delta}, \code{queries}, \code{used}.
}
\keyword{internal}
