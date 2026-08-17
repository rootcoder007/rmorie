% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{attention_weights}
\alias{attention_weights}
\title{attention_weights}
\usage{
attention_weights(Q, K, n_heads, pad_mask = NULL, causal = FALSE)
}
\arguments{
\item{Q}{A matrix; indexed by row and column.}
\item{K}{A matrix; indexed by row and column.}
\item{n_heads}{A count; the body uses it as \code{vector(...)}.}
\item{pad_mask}{Optional; may be \code{NULL}. A flag; the body branches on it.}
\item{causal}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the berte_native implementation. Called by \code{multi_head_attention}. See the file header for the source the module follows.
}
\value{
The value of \code{heads}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(1)
Q <- matrix(rnorm(12), 3, 4)
K <- matrix(rnorm(12), 3, 4)
w <- attention_weights(Q, K, n_heads = 2)
str(w, max.level = 1)
}
