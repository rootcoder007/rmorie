% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prgrl_perceptron}
\alias{.prgrl_perceptron}
\title{.prgrl_perceptron}
\usage{
.prgrl_perceptron(X, y, order, updates, w0)
}
\arguments{
\item{X}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{order}{A vector; its length is taken and its elements indexed.}
\item{updates}{Coerced to integer by the body, with \code{as.integer}.}
\item{w0}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}. See the file header for the source the module follows.
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
