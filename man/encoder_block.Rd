% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encoder_block}
\alias{encoder_block}
\title{encoder_block}
\usage{
encoder_block(X, Wq, Wk, Wv, Wo, W1, b1, W2, b2, n_heads, pad_mask = NULL, 
    gain1 = NULL, bias1 = NULL, gain2 = NULL, bias2 = NULL, pre_norm = FALSE) 
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Wq}{Passed to \code{apply}.}
\item{Wk}{Passed to \code{apply}.}
\item{Wv}{Passed to \code{apply}.}
\item{Wo}{Passed to \code{apply}.}
\item{W1}{Passed to \code{apply}.}
\item{b1}{Passed to \code{apply}.}
\item{W2}{Passed to \code{apply}.}
\item{b2}{Passed to \code{apply}.}
\item{n_heads}{Passed to \code{multi_head_attention}.}
\item{pad_mask}{Passed to \code{multi_head_attention}.}
\item{gain1}{Passed to \code{apply}.}
\item{bias1}{Passed to \code{apply}.}
\item{gain2}{Passed to \code{apply}.}
\item{bias2}{Passed to \code{apply}.}
\item{pre_norm}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the berte_native implementation. Called by \code{bert_encoder}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{weights}.
}
\keyword{internal}
\examples{
set.seed(2)
X <- matrix(rnorm(12), 3, 4)
W <- function() matrix(rnorm(16, 0, 0.4), 4, 4)
r <- encoder_block(X, W(), W(), W(), W(),
                   W1 = matrix(rnorm(32, 0, 0.4), 8, 4), b1 = rep(0, 8),
                   W2 = matrix(rnorm(32, 0, 0.4), 4, 8), b2 = rep(0, 4),
                   n_heads = 2)
str(r, max.level = 1)
}
