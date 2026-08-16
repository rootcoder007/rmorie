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
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{causal}{Defaults to \code{FALSE}.}
}
\description{
Part of the perfat_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
softmax_attention(Q = 0.5, K = 5L, V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
