% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_select_lambda}
\alias{cv_select_lambda}
\title{cv_select_lambda}
\usage{
cv_select_lambda(X, y, lambdas, V = 5L, seed = 0L, ...)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{lambdas}{See Usage.}
\item{V}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0L}.}
\item{...}{Passed through.}
}
\description{
Part of the tlhal_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lambda}, \code{cv_risks}, \code{note}.
}
\keyword{internal}
