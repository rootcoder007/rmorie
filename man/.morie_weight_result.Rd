% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_weight_result}
\alias{.morie_weight_result}
\title{.morie_weight_result}
\usage{
.morie_weight_result(weights, propensity, method, estimand, call, stabilize = FALSE, trim = NULL)
}
\arguments{
\item{weights}{A vector; its length is taken.}
\item{propensity}{See Usage.}
\item{method}{See Usage.}
\item{estimand}{See Usage.}
\item{call}{See Usage.}
\item{stabilize}{Defaults to \code{FALSE}.}
\item{trim}{Defaults to \code{NULL}.}
}
\description{
A step of the weight_native implementation. Called by \code{morie_weight_cbps}, \code{morie_weight_entropy}, \code{morie_weight_ow} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
