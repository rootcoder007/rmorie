% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{softmax_attention}
\alias{softmax_attention}
\title{softmax_attention}
\usage{
softmax_attention(Q, K, V, causal = FALSE) 
}
\arguments{
\item{Q}{A matrix; passed to \code{as.matrix}.}
\item{K}{A matrix; passed to \code{as.matrix}.}
\item{V}{A matrix; passed to \code{as.matrix}.}
\item{causal}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the perfat_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
softmax_attention(Q = 0.5, K = 5L, V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
