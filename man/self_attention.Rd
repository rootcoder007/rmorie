% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{self_attention}
\alias{self_attention}
\title{self_attention}
\usage{
self_attention(E, WQ, WK, WV, mask = NULL)
}
\arguments{
\item{E}{See Usage.}
\item{WQ}{See Usage.}
\item{WK}{See Usage.}
\item{WV}{See Usage.}
\item{mask}{Defaults to \code{NULL}.}
}
\description{
A step of the sasRec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{output}, \code{weights}, \code{note}.
}
\keyword{internal}
