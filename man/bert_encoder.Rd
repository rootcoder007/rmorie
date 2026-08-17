% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bert_encoder}
\alias{bert_encoder}
\title{bert_encoder}
\usage{
bert_encoder(X, blocks, n_heads, pad_mask = NULL, pre_norm = FALSE)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{blocks}{A vector; its length is taken.}
\item{n_heads}{Carried through into a list the body builds.}
\item{pad_mask}{Passed to \code{encoder_block}.}
\item{pre_norm}{Coerced to logical by the body, with \code{as.logical}. Defaults to \code{FALSE}.}
}
\description{
A step of the berte_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{output}, \code{attention}, \code{pooled}, \code{L}, \code{d}, \code{n_layers}, \code{n_heads}, \code{pre_norm}, \code{bidirectional}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
X <- matrix(rnorm(12), 3, 4)
W <- function() matrix(rnorm(16, 0, 0.4), 4, 4)
blk <- list(Wq = W(), Wk = W(), Wv = W(), Wo = W(),
            W1 = matrix(rnorm(32, 0, 0.4), 8, 4), b1 = rep(0, 8),
            W2 = matrix(rnorm(32, 0, 0.4), 4, 8), b2 = rep(0, 4))
r <- bert_encoder(X, list(blk), n_heads = 2)
str(r, max.level = 1)
}
