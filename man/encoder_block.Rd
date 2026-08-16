% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{encoder_block}
\alias{encoder_block}
\title{encoder_block}
\usage{
encoder_block(X, Wq, Wk, Wv, Wo, W1, b1, W2, b2, n_heads, pad_mask = NULL, gain1 = NULL, bias1 = NULL, gain2 = NULL, bias2 = NULL, pre_norm = FALSE)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Wq}{See Usage.}
\item{Wk}{See Usage.}
\item{Wv}{See Usage.}
\item{Wo}{See Usage.}
\item{W1}{See Usage.}
\item{b1}{See Usage.}
\item{W2}{See Usage.}
\item{b2}{See Usage.}
\item{n_heads}{See Usage.}
\item{pad_mask}{Defaults to \code{NULL}.}
\item{gain1}{Defaults to \code{NULL}.}
\item{bias1}{Defaults to \code{NULL}.}
\item{gain2}{Defaults to \code{NULL}.}
\item{bias2}{Defaults to \code{NULL}.}
\item{pre_norm}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the berte_native implementation. Called by \code{bert_encoder}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{weights}.
}
\keyword{internal}
