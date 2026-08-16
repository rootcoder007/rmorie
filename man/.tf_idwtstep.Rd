% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_idwtstep}
\alias{.tf_idwtstep}
\title{.tf_idwtstep}
\usage{
.tf_idwtstep(lo, hi, h, g)
}
\arguments{
\item{lo}{A vector; its length is taken and its elements indexed.}
\item{hi}{A vector; indexed elementwise.}
\item{h}{A vector; its length is taken and its elements indexed.}
\item{g}{A vector; indexed elementwise.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_idwt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
