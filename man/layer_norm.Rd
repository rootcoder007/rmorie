% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{layer_norm}
\alias{layer_norm}
\title{layer_norm}
\usage{
layer_norm(x, gain = NULL, bias = NULL, eps = 1e-12) 
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{gain}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{bias}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.}
}
\description{
A step of the berte_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
layer_norm(V)
}
